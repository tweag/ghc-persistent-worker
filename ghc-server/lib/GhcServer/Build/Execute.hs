-- | Execute the @main@ binding of every module in a unit, in parallel, via GHC's interactive evaluation
-- machinery (see 'Internal.Evaluate.executeMain'), mirroring the worker's @--expr@ mode.
--
-- Unlike compilation, execution has no cross-module dependency graph to resolve: each module's @main@ is
-- run independently, so this bypasses the scheduler entirely and just fans out with 'Control.Concurrent.Async'.
-- Modules that don't export @main@ are silently skipped (no instrumentation event emitted for them).
module GhcServer.Build.Execute where

import Control.Concurrent.Async (forConcurrently_)
import qualified Data.Map.Strict as Map
import GHC (mkModuleName)
import GhcServer.Build.Compile (moduleTarget)
import GhcServer.Build.Propagate (emitTaskEnd, emitTaskStart)
import GhcServer.Cache (loadHomeUnitCache)
import GhcServer.Data.BuildEnv (BuildEnv (..))
import GhcServer.Data.Unit (Project (..), Unit (..), UnitName (..))
import GhcServer.Log (instrumentLogger, withBuildLog)
import GhcServer.Path (fp)
import GhcServer.Scheduler (TaskResult (..))
import Internal.Compile.Make (compileModuleWithDepsInHpt)
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
import Types.Target (TargetSpec (TargetModule))

-- | Run a single module's @main@ via 'Internal.Evaluate.executeMain'.
--
-- Returns 'Nothing' if the module has no @main@ (skipped) OR if 'withGhcMakeModule' itself failed to set up a
-- session for the target (e.g. an exception during module loading) -- these two cases are indistinguishable
-- through 'withGhcMakeModule'\'s single-'Maybe' result type. Otherwise returns 'Just' a pair of a 'TaskResult'
-- reflecting success or failure (including captured diagnostics on failure) and, on success, @main@'s
-- exfiltrated return value as 'String' (see 'Internal.Evaluate.classifyMainResultType'), when its type was one
-- of the small set recognized there.
executeModule :: BuildEnv -> UnitName -> Unit -> String -> IO (Maybe (TaskResult String, Maybe String))
executeModule buildEnv name unit modBaseName = do
  let
    modName = mkModuleName modBaseName
    modTmpDir = buildEnv.tmpDir </> toOsPath name.string </> toOsPath (modBaseName ++ "-execute")
  createDirectoryIfMissing True modTmpDir
  cachedUnit <- loadHomeUnitCache unit.cache
  withBuildLog \ rawLogger -> do
    let
      logger = instrumentLogger buildEnv.instrChan (name.string ++ ":" ++ modBaseName ++ ":execute") rawLogger
      args = buildEnv.baseArgs {tempDir = Just modTmpDir, homeUnit = cachedUnit}
      env = Env {log = logger, state = buildEnv.stateVar, args}
      target = moduleTarget name modName
    ghcResult <-
      withGhcMakeModule Interpreted target env \ targetSpec -> do
        -- Build the module the regular (object-code) way first, exactly like 'GhcServer.Build.Compile.compileSingleModule'
        -- does. This ensures the module (and, via GHC's downsweep-driven summary lookup, its dependencies) actually
        -- has real interface\/object artifacts on disk before we ask for a bytecode recompile -- see the
        -- 'kb-instrument-ui' "known limitation" this addresses: a metadata-only build has no compiled interfaces at
        -- all, so 'executeMain' silently fails to find any module's @main@.
        _ <- compileModuleWithDepsInHpt logger (TargetModule target)
        -- The module must be (re)compiled to bytecode before 'executeMain's 'setContext' call: any prior HPT
        -- entry for this module (e.g. from the regular compile just above, or an earlier plain compile-to-object-
        -- code request) has an object-code linkable, which GHC's interactive context machinery rejects with "not
        -- interpreted". 'compileModuleWithDepsInHpt' with the 'TargetModuleInterp' spec ('targetSpec' here, since
        -- 'interp = Interpreted') compiles via 'mkTargetAsInterpreted' (bytecode backend) and inserts the
        -- resulting 'HomeModInfo' into the HPT, overwriting the object-code entry just inserted above.
        _ <- compileModuleWithDepsInHpt logger targetSpec
        executeMain env (fromOsPath <$> args.homeUnit) target
    captured <- logger.flush
    logger.debug ("executeModule: " ++ name.string ++ ":" ++ modBaseName ++ " GHC result=" ++ show ghcResult)
    -- The executed @main@'s real stdout\/stderr output goes to the actual process handles (this is a
    -- backgrounded @ghc-server@ process, not an in-process capture), which the @instrument@ client streams and
    -- logs itself (see 'ServeGhcServer.spawnGhcServer'); this function no longer redirects\/captures them.
    pure $ case ghcResult of
      Just (True, mResultStr) -> Just (TaskSuccess, mResultStr)
      Just (False, _) -> Just (TaskFailed ("Execution failed:\n" ++ unlines captured), Nothing)
      Nothing -> Nothing

-- | Execute a single named module of a unit, emitting a 'CompileStart'\/'CompileEnd' instrumentation event pair
-- (target text @unitName:moduleName:execute@) if the module actually has a @main@ to run. Produces no event if the
-- module has none, matching the skip semantics used elsewhere in the build pipeline. This is the sole entry point
-- for module-granularity execution (the instrument UI's 'x' key on a selected module row): it never touches any
-- other module of the unit.
executeUnitModule :: BuildEnv -> UnitName -> String -> IO ()
executeUnitModule buildEnv name modBaseName =
  case Map.lookup name buildEnv.project.units of
    Nothing -> buildEnv.log.debug ("executeUnitModule: unknown unit " ++ name.string)
    Just unit -> do
      let target = name.string ++ ":" ++ modBaseName ++ ":execute"
      buildEnv.log.debug ("executeUnitModule: dispatching " ++ target)
      mresult <- executeModule buildEnv name unit modBaseName
      case mresult of
        Nothing -> buildEnv.log.debug ("executeUnitModule: " ++ target ++ " skipped (no main, or session setup failed)")
        Just (result, mResultStr) -> do
          buildEnv.log.debug ("executeUnitModule: " ++ target ++ " finished: " ++ show result ++ ", result=" ++ show mResultStr)
          emitTaskStart buildEnv target
          emitTaskEnd buildEnv target result mResultStr

-- | Execute all modules of a unit in parallel, delegating to 'executeUnitModule' for each. A successful
-- execution's exfiltrated return value (if any) is attached to the 'CompileEnd' event, so it can be displayed by
-- the @instrument@ UI under the task's row.
executeUnit :: BuildEnv -> UnitName -> IO ()
executeUnit buildEnv name =
  case Map.lookup name buildEnv.project.units of
    Nothing -> buildEnv.log.debug ("executeUnit: unknown unit " ++ name.string)
    Just unit -> do
      let modules = moduleNames unit
      buildEnv.log.debug ("executeUnit: running " ++ show (length modules) ++ " module(s) in unit " ++ name.string ++ ": " ++ show modules)
      forConcurrently_ modules (executeUnitModule buildEnv name)
  where
    moduleNames unit = [takeBaseName (fp src) | src <- unit.sources]

-- | Execute every module of every unit in the project in parallel, delegating to 'executeUnit' per unit. This
-- backs the project-root node's 'x' execute action in the @instrument@ UI's task tree.
executeProject :: BuildEnv -> IO ()
executeProject buildEnv =
  forConcurrently_ (Map.keys buildEnv.project.units) (executeUnit buildEnv)
