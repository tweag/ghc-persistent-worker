-- | The subprocess side of the self-relaunched execute-task child process (see 'GhcServer.Build.Process' for
-- the parent side and the overall design rationale, @kb-process-isolation@ for why a subprocess relaunch is
-- used instead of forking).
--
-- Everything in this module runs only in the child: it decodes the 'ProcessEvalConfig' passed on argv,
-- rebuilds a minimal 'GhcServer.Data.BuildEnv.BuildEnv' from scratch around the one unit being executed,
-- restores that unit's module map from the on-disk cache the parent's compile step already wrote, runs
-- 'GhcServer.Build.Execute.executeModuleTask', and writes exactly one JSON-encoded 'ProcessEvalResult' to
-- stdout before exiting. It never prints operational messages to the real stdout\/stderr; those are captured
-- and folded into the result instead.
module GhcServer.Build.ProcessChild where

import Control.Concurrent.MVar (newMVar)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as Lazy.ByteString
import Data.Foldable (traverse_)
import Data.IORef (newIORef)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Text (Text, pack)
import GHC.Data.Graph.Directed (graphFromEdgedVerticesOrd)
import GHC.Stats (RTSStats (..), getRTSStats)
import GHC.Unit.Types (unitIdString)
import GhcServer.Build.Execute (executeModuleTask)
import GhcServer.Build.Schedule (BuildExt (..), ModuleInfo, ModuleKey (..), emptyBuildExt, resolveFromCachedUnit)
import GhcServer.Build.SharedBytecode (bytecodeImportEntries, importSharedBytecode)
import GhcServer.Cache (loadCachedUnit, loadCachedUnitAt, loadDepUnitPlans)
import GhcServer.Data.BuildEnv (BuildEnv (..))
import GhcServer.Data.BuildEvent (newBuildEvents)
import GhcServer.Data.ProcessEval (
  EvalOutcome (..),
  ProcessEvalConfig (..),
  ProcessEvalOptions (..),
  ProcessEvalResult (..),
  )
import GhcServer.Data.Unit (Project (..), Unit (..), UnitCache)
import GhcServer.Path (outputDirName, tmpDirName)
import Internal.State (newState, updateMakeStateVar)
import Prelude hiding (log)
import System.Directory.OsPath (createDirectoryIfMissing)
import qualified System.IO as IO
import System.IO.Silently (capture, hCapture)
import System.Mem (performMajorGC)
import System.OsPath.Extra (OsPath, fromOsPath, (</>))
import Test.Log (newTestLog)
import Test.Scheduler (TaskResult (..))
import Types.Api (ModuleName (..), ProcessStats (..), UnitName (..), toGhcModuleName)
import qualified Types.Args as Args
import Types.Args (emptyArgs)
import Types.CachedDeps (CachedBuildPlan (..), CachedBuildPlans (..), CachedUnit (..), JsonFs (..))
import Types.Log (Logger (..), debugT)
import Types.Settings (defaultSettings)
import Types.State.Make (MakeState (..))

-- | Resolve a unit's own module map plus its full transitive dependency closure's module maps, purely from the
-- on-disk cache written by a prior in-process build (@cached_unit.json@ and, for each dependency unit,
-- @dep_units.json@'s @cached_unit.json@ paths). Returns 'Nothing' if the target unit itself has no cache.
--
-- This mirrors what the in-process scheduler accumulates incrementally in 'BuildExt.moduleMap' across every
-- unit's metadata completion (see 'GhcServer.Build.Propagate.propagateCompletion'): 'buildModuleCachedDeps'
-- needs the *full* module map to resolve a module's cross-unit dependencies, not just its own unit's. The
-- subprocess child has no such accumulated history -- it never ran metadata for the dependency units, only the
-- parent did -- so it must reconstruct the same closure from disk instead. Without this, cross-unit imports
-- silently resolve to an empty 'Types.CachedDeps.CachedDeps', leaving the HPT missing the dependency (visible as
-- @hugSomeThingsBelowUs@ warnings, masked in-process only because the long-lived session already has the
-- dependency loaded from earlier scheduler tasks).
--
-- Dependency units are resolved by 'GHC.Unit.Types.unitIdString' on the 'Types.CachedDeps.CachedBuildPlan' name,
-- which is exactly how 'GhcServer.Build.Schedule.resolveFromCachedUnit' derives package-dependency 'ModuleKey's,
-- so the two agree on unit naming.
loadTransitiveModuleMap ::
  OsPath ->
  UnitName ->
  UnitCache ->
  ExceptT Text IO (Maybe (Map ModuleKey ModuleInfo))
loadTransitiveModuleMap outputDir name unitCache =
  loadCachedUnit unitCache >>= traverse \ cu -> do
    depMap <- maybe (pure Map.empty) loadDepUnits cu.dep_units
    pure (Map.union (resolveFromCachedUnit name outputDir cu) depMap)
  where
    loadDepUnits depUnitsPath =
      loadDepUnitPlans depUnitsPath >>= \case
        Nothing -> pure []
        Just (CachedBuildPlans plans) -> Map.unions <$> traverse loadPlan plans

    loadPlan CachedBuildPlan {name = JsonFs uid, build_plan} =
      maybe Map.empty (resolveFromCachedUnit (depUnitName uid) outputDir) <$> loadCachedUnitAt build_plan

    depUnitName uid = UnitName (pack (unitIdString uid))

-- | Rebuild the 'GhcServer.Data.BuildEnv.BuildEnv' from scratch (a fresh 'Types.State.WorkerState', no state
-- inherited from the parent) around the unit the parent already resolved, restore the target unit's module map
-- from @cached_unit.json@ (plus its transitive dependency units' own @cached_unit.json@ files, so that
-- cross-unit imports resolve to cached interfaces instead of leaving the HPT missing them -- see
-- 'loadTransitiveModuleMap'), and run 'GhcServer.Build.Execute.executeModuleTask'. All progress reporting goes
-- to the given logger; the outcome is returned rather than written anywhere.
runEval :: Logger -> ProcessEvalConfig -> IO EvalOutcome
runEval logger ProcessEvalConfig {projectRoot, unit, moduleName, sharedBytecodePath} = do
  debugT logger ("Evaluating " <> moduleName.text <> " in unit " <> unit.name.text)
  runExceptT (loadTransitiveModuleMap outputDir unit.name unit.cache) >>= \case
    Left err -> pure (EvalCacheUnreadable err)
    Right Nothing -> pure EvalCacheMissing
    Right (Just moduleMap) -> do
      buildEnv <- childBuildEnv
      let ext = emptyBuildExt {moduleMap}
      traverse_ (installBytecodeImport buildEnv) sharedBytecodePath
      outcome <$> executeModuleTask buildEnv ext unit (toGhcModuleName moduleName) 0 Nothing
  where
    -- Restore mirrored bytecode from shared memory (if any was exported by the parent) into
    -- 'Types.State.Make.MakeState.bytecodeImport', so 'Internal.State.Linkables.addLazyByteCode' can rehydrate it
    -- lazily, only for modules actually reached by the executed module's link dependencies, instead of eagerly
    -- installing every mirrored module up front via 'GhcServer.Build.SharedBytecode.installBytecode'.
    installBytecodeImport buildEnv path =
      importSharedBytecode path >>= \case
        Just bytecodeMap -> do
          debugT logger (
            "Restored bytecode for " <> pack (show (Map.size bytecodeMap)) <> " module(s) from shared memory"
            )
          updateMakeStateVar buildEnv.stateVar \ make ->
            make {bytecodeImport = Map.union make.bytecodeImport (bytecodeImportEntries bytecodeMap)}
        Nothing ->
          debugT logger "Could not restore bytecode from shared memory; falling back to cached interfaces"

    outcome = \case
      Just (TaskSuccess payload) -> EvalSuccess payload
      Just (TaskFailed msg) -> EvalExecuteFailed (pack msg)
      Nothing -> EvalNoMain

    outputDir = projectRoot </> outputDirName
    tmpDir = projectRoot </> tmpDirName

    -- A minimal 'BuildEnv' for a process that only ever runs one execute task and exits: default feature
    -- flags (the parent's flags aren't available here and don't matter for a single execute task),
    -- instrumentation disabled (no channel to forward events to), and the in-memory logger created by
    -- 'runProcessEval', so that nothing reaches stdio. The 'Project' only ever needs to contain the one unit
    -- being executed; 'executeModuleTask' and cache restoration never consult sibling units.
    childBuildEnv = do
      createDirectoryIfMissing True outputDir
      createDirectoryIfMissing True tmpDir
      stateVar <- newState defaultSettings
      events <- newBuildEvents
      extDepsDb <- newMVar Nothing
      diff <- newMVar Map.empty
      requestIdCounter <- newIORef 0
      pure BuildEnv {
        baseArgs = (emptyArgs Map.empty) {Args.settings = defaultSettings},
        projectRoot,
        outputDir,
        tmpDir,
        stateVar,
        project = Project {units = Map.singleton unit.name unit, depGraph = graphFromEdgedVerticesOrd []},
        log = logger,
        events,
        instrChan = Nothing,
        extDepsDb,
        diff,
        requestIdCounter
      }

resolveConfig :: Maybe OsPath -> IO (Either String ProcessEvalConfig)
resolveConfig = \case
  Just path ->
    Aeson.eitherDecodeFileStrict' (fromOsPath path)
  Nothing ->
    Aeson.eitherDecodeStrict' <$> ByteString.getContents

-- | Run in the child process: set up an in-memory logger before anything else, capture the stdout and stderr of
-- everything that follows, and write the single JSON-encoded 'ProcessEvalResult' to the restored stdout.
--
-- The exit code is deliberately not part of the protocol: the child always exits successfully once it has
-- emitted a result, so a nonzero exit code unambiguously means it crashed before doing so.
--
-- TODO catch exceptions here?
runProcessEval :: ProcessEvalOptions -> IO ()
runProcessEval ProcessEvalOptions {configFile} = do
  (logger, _) <- newTestLog
  (evalStderr, (evalStdout, outcome)) <- resolveConfig configFile >>= \case
    Left err -> pure ("", ("", EvalConfigInvalid (pack err)))
    Right config -> hCapture [IO.stderr] (capture (runEval logger config))
  logger.debug ("Evaluation result: " <> show outcome)
  logMessages <- fmap pack <$> logger.flush
  -- 'max_mem_in_use_bytes'/'max_live_bytes' are only updated when a GC actually runs; a short-lived subprocess
  -- task can otherwise exit without ever triggering one, reporting all-zero stats despite '-T' being enabled
  -- (see the '-with-rtsopts' flags on the executable). Force one last GC so the reported peak reflects reality.
  performMajorGC
  rtsStats <- getRTSStats
  Lazy.ByteString.putStr (Aeson.encode ProcessEvalResult {
    evalStdout = pack evalStdout,
    evalStderr = pack evalStderr,
    logMessages,
    outcome,
    stats = ProcessStats {
      maxMemInUseBytes = rtsStats.max_mem_in_use_bytes,
      maxLiveBytes = rtsStats.max_live_bytes
    }
  })
