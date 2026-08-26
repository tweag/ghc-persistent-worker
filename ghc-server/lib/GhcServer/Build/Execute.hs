-- | Dispatch-time execution of a single module's @main@ via GHC's interactive evaluation machinery (see
-- 'Internal.Evaluate.executeMain'), mirroring the worker's @--expr@ mode.
--
-- Runs as an ordinary scheduler task ('GhcServer.Build.Schedule.ExecuteModule'), depending on the module's own
-- compile task ('GhcServer.Build.Schedule.ResolvedModule'). Because the scheduler guarantees that dependency ran
-- (or was cache-skipped) first, this only needs the interpreted (bytecode) recompile immediately before
-- 'executeMain' -- an object-code 'HomeModInfo' already in the HPT from the compile task is rejected by GHC's
-- interactive-context machinery ("not interpreted").
module GhcServer.Build.Execute where

import Control.Exception (Handler (..), catches)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import GHC (GhcException, ModuleName, moduleNameString)
import GHC.Types.SourceError (SourceError)
import GhcServer.Build.Compile (moduleTarget)
import GhcServer.Build.Schedule (BuildExt (..), ModuleKey (..), buildModuleCachedDeps)
import GhcServer.Cache (loadHomeUnitCache)
import GhcServer.Data.BuildEnv (BuildEnv (..))
import GhcServer.Data.Unit (Project (..), Unit (..), UnitName (..))
import GhcServer.Log (instrumentLogger, withBuildLog)
import GhcServer.Scheduler (TaskResult (..))
import Internal.Compile.Make (compileModuleWithDepsInHpt)
import Internal.Evaluate (executeMain)
import Internal.Session (withGhcMakeModule)
import Prelude hiding (log)
import System.Directory.OsPath (createDirectoryIfMissing)
import System.OsPath ((</>))
import System.OsPath.Extra (fromOsPath, toOsPath)
import Types.Args (Args (..))
import Types.BuckArgs (IsInterpreted (..))
import Types.Env (Env (..))
import Types.Log (Logger (..))

-- | Outcome of attempting to run a module's @main@, collapsing the layered result that
-- 'Internal.Evaluate.executeMain' and its GHC session wrapper produce into one flat type.
data ExecOutcome =
  -- | The GHC session itself failed to start (an exception was caught by 'runGhcCatchingExceptions' or by
  -- 'Internal.Session.withGhcMakeModule''s own exception handling).
  ExecSessionFailed String
  |
  -- | Setup succeeded but a precondition failed without an exception (home unit missing, module not found).
  ExecSetupFailed String
  |
  -- | The module has no @main@ binding. A deliberate silent skip: the scheduler task still completes
  -- successfully, but no instrumentation event is emitted for it.
  ExecNoMain
  |
  -- | @main@ ran to completion; 'False' signals a runtime failure inside @main@ (its own stderr is captured by
  -- the build log rather than carried here), and the optional 'String' is a result value the target function
  -- exfiltrated (see 'Internal.Evaluate.executeMain').
  ExecRan Bool (Maybe String)

-- | Run a single module's @main@, distinguishing three failure modes ('ExecSessionFailed', 'ExecSetupFailed',
-- and a runtime failure via 'ExecRan False') from the deliberate no-@main@ skip ('ExecNoMain'). Only the
-- failure modes are reported as a failed task ('TaskFailed'); 'ExecNoMain' is reported as 'Nothing' so the
-- caller ('GhcServer.Build.Propagate.dispatchTask') can distinguish "skip" from "ran/failed".
executeModuleTask :: BuildEnv -> BuildExt -> UnitName -> ModuleName -> IO (Maybe (TaskResult String))
executeModuleTask buildEnv ext name modName =
  case Map.lookup name buildEnv.project.units of
    Nothing -> pure (Just (TaskFailed ("Unit not found in project: " ++ name.string)))
    Just unit -> do
      let
        modBaseName = moduleNameString modName
        modTmpDir = buildEnv.tmpDir </> toOsPath name.string </> toOsPath (modBaseName ++ "-execute")
        modKey = ModuleKey {unit = name, name = modName}
        cachedDeps = buildModuleCachedDeps ext.moduleMap modKey
      createDirectoryIfMissing True modTmpDir
      cachedUnit <- loadHomeUnitCache unit.cache
      withBuildLog \ rawLogger -> do
        let
          logger = instrumentLogger buildEnv.instrChan (name.string ++ ":" ++ modBaseName ++ ":execute") rawLogger
          args = buildEnv.baseArgs {tempDir = Just modTmpDir, homeUnit = cachedUnit, cachedDeps = Just cachedDeps}
          env = Env {log = logger, state = buildEnv.stateVar, args}
          target = moduleTarget name modName
        outcome <- runGhcCatchingExceptions do
          withGhcMakeModule Interpreted target env \ targetSpec -> do
            _ <- compileModuleWithDepsInHpt logger targetSpec
            Just <$> executeMain env (fromOsPath <$> args.homeUnit) target
        captured <- logger.flush
        logger.debug ("executeModuleTask: " ++ name.string ++ ":" ++ modBaseName ++ " outcome=" ++ show (outcomeLabel outcome))
        pure $ case outcome of
          ExecSessionFailed reason -> Just (TaskFailed reason)
          ExecSetupFailed reason -> Just (TaskFailed reason)
          ExecNoMain -> Nothing
          ExecRan True mResultStr -> Just (TaskSuccess (Text.pack <$> mResultStr))
          ExecRan False _ -> Just (TaskFailed ("Execution failed:\n" ++ unlines captured))
  where
    outcomeLabel = \case
      ExecSessionFailed _ -> "session-failed" :: String
      ExecSetupFailed _ -> "setup-failed"
      ExecNoMain -> "no-main"
      ExecRan ok _ -> if ok then "ran" else "failed"

-- | Run the GHC-interacting call chain ('Internal.Session.withGhcMakeModule', 'compileModuleWithDepsInHpt',
-- 'Internal.Evaluate.executeMain'), converting its layered result and any escaping GHC exception into a flat
-- 'ExecOutcome'.
--
-- 'Internal.Session.runWithSession' wraps a session's action in 'Internal.Error.handleExceptions', which catches
-- most exceptions raised inside GHC and converts them to a log message plus a 'Nothing' from
-- 'withGhcMakeModule'. It deliberately rethrows 'System.Exit.ExitCode' and 'Control.Exception.UserInterrupt',
-- which is correct for a single-session CLI tool that should actually terminate on those, but wrong for a task
-- running inside a long-lived scheduler: an escaping 'ExitCode' would kill the entire @ghc-server@ process, and
-- either exception would leave the scheduler task permanently "in flight" with no result ever recorded. This
-- catches the two named GHC exception types ('SourceError', 'GhcException') as defense-in-depth against that,
-- without a blanket @SomeException@ catch (which would also swallow
-- 'Control.Exception.StackOverflow'\/'Control.Exception.HeapOverflow'\/'Control.Exception.ThreadKilled').
--
-- This does not address a genuine indefinite hang (GHC blocking forever without ever throwing) -- there is no
-- exception to catch in that case, so the task still stays "in flight" regardless of this handler.
runGhcCatchingExceptions :: IO (Maybe (Either String (Maybe (Bool, Maybe String)))) -> IO ExecOutcome
runGhcCatchingExceptions action =
  catches (toOutcome <$> action) [
    Handler \ (e :: SourceError) -> pure (ExecSessionFailed ("Uncaught source error during execute: " ++ show e)),
    Handler \ (e :: GhcException) -> pure (ExecSessionFailed ("Uncaught GHC exception during execute: " ++ show e))
  ]
  where
    toOutcome = \case
      Nothing -> ExecSessionFailed "GHC session setup failed"
      Just (Left reason) -> ExecSetupFailed reason
      Just (Right Nothing) -> ExecNoMain
      Just (Right (Just (ok, mResultStr))) -> ExecRan ok mResultStr

