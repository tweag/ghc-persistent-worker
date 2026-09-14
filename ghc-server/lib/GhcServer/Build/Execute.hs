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
import qualified Data.Text as Text
import GHC (GhcException, ModuleName)
import GHC.Types.SourceError (SourceError)
import GhcServer.Build.Compile (withModuleSession)
import GhcServer.Build.Schedule (BuildExt (..), ModuleKey (..), buildModuleCachedDeps)
import GhcServer.Data.BuildEnv (BuildEnv (..))
import GhcServer.Data.Unit (Unit (..))
import GhcServer.Log (emitEvent)
import Test.Scheduler (TaskResult (..))
import Internal.Compile.Make (compileModuleWithDepsInHpt)
import Internal.Evaluate (executeMain)
import Internal.Session (withGhcMakeModule)
import Prelude hiding (log)
import System.OsPath.Extra (fromOsPath)
import Types.Args (Args (..))
import Types.BuckArgs (IsInterpreted (..))
import Types.Env (Env (..))

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
executeModuleTask :: BuildEnv -> BuildExt -> Unit -> ModuleName -> Int -> IO (Maybe (TaskResult String))
executeModuleTask buildEnv ext unit modName requestId = do
  (outcome, captured) <- withModuleSession buildEnv unit modName (Just "execute") cachedDeps \ logger env target ->
    runGhcCatchingExceptions do
      withGhcMakeModule Interpreted target env \ targetSpec -> do
        _ <- compileModuleWithDepsInHpt logger (emitEvent buildEnv.instrChan) requestId targetSpec
        Just <$> executeMain env (fromOsPath <$> env.args.homeUnit) target
  pure $ case outcome of
    ExecSessionFailed reason -> Just (TaskFailed (reason ++ "\n" ++ unlines captured))
    ExecSetupFailed reason -> Just (TaskFailed reason)
    ExecNoMain -> Nothing
    ExecRan True mResultStr -> Just (TaskSuccess (Text.pack <$> mResultStr))
    ExecRan False _ -> Just (TaskFailed ("Execution failed:\n" ++ unlines captured))
  where
    cachedDeps = buildModuleCachedDeps ext.moduleMap ModuleKey {unit = unit.name, name = modName}

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

