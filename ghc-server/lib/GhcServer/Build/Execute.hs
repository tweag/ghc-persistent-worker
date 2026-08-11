-- | Execute the @main@ binding of every module in a unit, in parallel, via GHC's interactive evaluation
-- machinery (see 'Internal.Evaluate.executeMain'), mirroring the worker's @--expr@ mode.
--
-- Unlike compilation, execution has no cross-module dependency graph to resolve: each module's @main@ is
-- run independently, so this bypasses the scheduler entirely and just fans out with 'Control.Concurrent.Async'.
-- Modules that don't export @main@ are silently skipped (no instrumentation event emitted for them).
module GhcServer.Build.Execute where

import Control.Concurrent.Async (forConcurrently_)
import GHC (mkModuleName)
import GhcServer.Build.Compile (moduleTarget)
import GhcServer.Build.Propagate (emitTaskEnd, emitTaskStart)
import GhcServer.Cache (loadHomeUnitCache)
import GhcServer.Data.BuildEnv (BuildEnv (..))
import GhcServer.Data.Unit (Project (..), Unit (..), UnitName (..))
import GhcServer.Log (withBuildLog)
import GhcServer.Path (fp)
import GhcServer.Scheduler (TaskResult (..))
import Internal.Evaluate (executeMain)
import Internal.Session (withGhcMakeModule)
import Prelude hiding (log)
import System.Directory.OsPath (createDirectoryIfMissing)
import System.FilePath (takeBaseName)
import System.OsPath ((</>))
import System.OsPath.Extra (fromOsPath, toOsPath)
import Types.Args (Args (..))
import Types.BuckArgs (IsInterpreted (..))
import Types.Env (Env (..))
import Types.Log (Logger (..))
import qualified Data.Map.Strict as Map

-- | Run a single module's @main@ via 'Internal.Evaluate.executeMain'.
--
-- Returns 'Nothing' if the module has no @main@ (skipped) OR if 'withGhcMakeModule' itself failed to set up a
-- session for the target (e.g. an exception during module loading) -- these two cases are indistinguishable
-- through 'withGhcMakeModule'\'s single-'Maybe' result type. Otherwise returns 'Just' a 'TaskResult' reflecting
-- success or failure (including captured diagnostics on failure).
executeModule :: BuildEnv -> UnitName -> Unit -> String -> IO (Maybe (TaskResult String))
executeModule buildEnv name unit modBaseName = do
  let
    modName = mkModuleName modBaseName
    modTmpDir = buildEnv.tmpDir </> toOsPath name.string </> toOsPath (modBaseName ++ "-execute")
  createDirectoryIfMissing True modTmpDir
  cachedUnit <- loadHomeUnitCache unit.cache
  withBuildLog \ logger -> do
    let
      args = buildEnv.baseArgs {tempDir = Just modTmpDir, homeUnit = cachedUnit}
      env = Env {log = logger, state = buildEnv.stateVar, args}
      target = moduleTarget name modName
    result <- withGhcMakeModule Interpreted target env \ _targetSpec ->
      executeMain env (fromOsPath <$> args.homeUnit) target
    captured <- logger.flush
    pure $ case result of
      Just True -> Just TaskSuccess
      Just False -> Just (TaskFailed ("Execution failed:\n" ++ unlines captured))
      Nothing -> Nothing

-- | Execute all modules of a unit in parallel, emitting a 'CompileStart'\/'CompileEnd' instrumentation event
-- pair (target text @unitName:moduleName:execute@) around each module that actually has a @main@ to run.
-- Modules without @main@ produce no event, matching the skip semantics used elsewhere in the build pipeline.
executeUnit :: BuildEnv -> UnitName -> IO ()
executeUnit buildEnv name =
  case Map.lookup name buildEnv.project.units of
    Nothing -> buildEnv.log.debug ("executeUnit: unknown unit " ++ name.string)
    Just unit ->
      forConcurrently_ (moduleNames unit) \ modBaseName -> do
        let target = name.string ++ ":" ++ modBaseName ++ ":execute"
        mresult <- executeModule buildEnv name unit modBaseName
        case mresult of
          Nothing -> pure ()
          Just result -> do
            emitTaskStart buildEnv target
            emitTaskEnd buildEnv target result
  where
    moduleNames unit = [takeBaseName (fp src) | src <- unit.sources]
