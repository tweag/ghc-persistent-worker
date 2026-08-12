-- | Dispatch-time execution of a single module's @main@ via GHC's interactive evaluation machinery (see
-- 'Internal.Evaluate.executeMain'), mirroring the worker's @--expr@ mode.
--
-- Runs as an ordinary scheduler task ('GhcServer.Build.Schedule.ExecuteModule'), depending on the module's own
-- compile task ('GhcServer.Build.Schedule.ResolvedModule'). Because the scheduler guarantees that dependency ran
-- (or was cache-skipped) first, this no longer needs to force its own redundant object-code pre-compile -- only
-- the interpreted (bytecode) recompile immediately before 'executeMain', which is still required since any
-- object-code 'HomeModInfo' already in the HPT from the compile task is rejected by GHC's interactive-context
-- machinery ("not interpreted").
module GhcServer.Build.Execute where

import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import GHC (ModuleName, moduleNameString)
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

-- | Run a single module's @main@.
--
-- Returns 'Nothing' if the module has no @main@ (skipped) OR if 'withGhcMakeModule' itself failed to set up a
-- session for the target -- these two cases are indistinguishable through 'withGhcMakeModule'\'s single-'Maybe'
-- result type. This is deliberately not wrapped in scheduler instrumentation events here: the caller
-- ('GhcServer.Build.Propagate.dispatchTask') only emits 'CompileStart'\/'CompileEnd' once it knows a real
-- execution attempt happened, matching the no-@main@ silent-skip semantics documented for the 'ExecuteModule'
-- task.
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
        ghcResult <-
          withGhcMakeModule Interpreted target env \ targetSpec -> do
            -- Recompile to bytecode: any HPT entry left by the (already-completed) compile task has an
            -- object-code linkable, which GHC's interactive context rejects with "not interpreted".
            _ <- compileModuleWithDepsInHpt logger targetSpec
            executeMain env (fromOsPath <$> args.homeUnit) target
        captured <- logger.flush
        logger.debug ("executeModuleTask: " ++ name.string ++ ":" ++ modBaseName ++ " GHC result=" ++ show ghcResult)
        pure $ case ghcResult of
          Just (True, mResultStr) -> Just (TaskSuccess (Text.pack <$> mResultStr))
          Just (False, _) -> Just (TaskFailed ("Execution failed:\n" ++ unlines captured))
          Nothing -> Nothing
