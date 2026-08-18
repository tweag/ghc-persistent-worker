-- | Run a unit's @execute@ task in a genuinely separate OS process, spawned via @typed-process@, rather than
-- in-process (see @kb-process-isolation@ for why: forking a live multi-capability GHC session deadlocks, but a
-- fresh subprocess relaunch does not).
--
-- The child is a relaunch of the running @ghc-server@ executable (detected via 'evalProcessFlag' in its argv,
-- see 'GhcServer.Run.runServer') that builds its own 'GhcServer.Data.BuildEnv.BuildEnv' and
-- 'Types.State.WorkerState' from scratch, using default feature flags rather than the parent's -- the child only
-- runs a single execute task and exits, so there is no persistent state for feature flags to govern. It restores
-- the target unit's module map purely from the on-disk cache the parent's compile step already wrote
-- (@cached_unit.json@ plus the @.dyn_hi@\/@.dyn_o@ interface files), the same mechanism Buck uses to resume a
-- killed-and-restarted worker (see @kb-buck-cache@).
--
-- The child's stdio follows a strict protocol: it captures everything the evaluated module writes to stdout and
-- stderr with 'hCapture', routes its own operational messages into an in-memory logger, and writes exactly one
-- JSON-encoded 'ProcessEvalResult' to stdout before exiting. The parent therefore parses the child's entire
-- stdout as a single JSON document and treats any child stderr output as unexpected.
--
-- Originally prototyped as a self-contained test (@Test.SubprocessTest@, 'test_subprocessExecute'); this module
-- is the production extraction of that experiment's child-process and parent-process logic.
module GhcServer.Build.Process where

import Control.Concurrent.MVar (newMVar, readMVar)
import Control.Exception (SomeException, bracket, displayException, try)
import Control.Monad (unless)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import qualified Data.Aeson as Aeson
import Data.Bifunctor (first)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as Lazy.ByteString
import Data.Foldable (traverse_)
import Data.Functor ((<&>))
import Data.IORef (newIORef)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Text as Text
import Data.Text (Text, pack)
import qualified GHC
import GHC (moduleNameString)
import GHC.Data.Graph.Directed (graphFromEdgedVerticesOrd)
import GHC.Unit.Types (unitIdString)
import GhcServer.Build.Execute (executeModuleTask)
import GhcServer.Build.Schedule (BuildExt (..), ModuleInfo, ModuleKey (..), emptyBuildExt, resolveFromCachedUnit)
import GhcServer.Build.SharedBytecode (
  bytecodeImportEntries,
  cleanupSharedBytecode,
  collectBytecode,
  exportSharedBytecode,
  importSharedBytecode,
  )
import GhcServer.Cache (loadCachedUnit, loadCachedUnitAt, loadDepUnitPlans)
import GhcServer.Data.BuildEnv (BuildEnv (..))
import GhcServer.Data.BuildEvent (newBuildEvents)
import GhcServer.Data.ProcessEval (
  EvalOutcome (..),
  ProcessEvalConfig (..),
  ProcessEvalOptions (..),
  ProcessEvalOutput (..),
  ProcessEvalResult (..),
  )
import GhcServer.Data.Unit (Project (..), Unit (..), UnitCache)
import GhcServer.Path (outputDirName, tmpDirName)
import Internal.State (newState, updateMakeStateVar)
import Prelude hiding (log)
import System.Directory.OsPath (createDirectoryIfMissing)
import System.Environment (getExecutablePath)
import qualified System.IO as IO
import System.IO.Silently (capture, hCapture)
import System.OsPath.Extra (OsPath, fromOsPath, toOsPath, (</>))
import System.Process.Typed (byteStringInput, proc, readProcess, setStdin)
import Test.Log (newTestLog)
import Test.Scheduler (TaskResult (..))
import Types.Api (ModuleName (..), UnitName (..), fromGhcModuleName, toGhcModuleName)
import qualified Types.Args as Args
import Types.Args (emptyArgs)
import Types.ByteString (fromUtf8Lazy)
import Types.CachedDeps (CachedBuildPlan (..), CachedBuildPlans (..), CachedUnit (..), JsonFs (..))
import Types.FeatureFlags (Feature (..))
import Types.Log (Logger (..), debugT)
import Types.Settings (defaultSettings, featureOn)
import Types.State (WorkerState (..))
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
      processUnits <- newMVar mempty
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
        requestIdCounter,
        processUnits
      }

-- | Spawn a fresh child process (a relaunch of @selfPath@, typically the current @ghc-server@ executable's own
-- path via 'System.Environment.getExecutablePath') with 'evalProcessFlag' and the JSON-encoded config, and
-- decode its entire stdout as the child's 'ProcessEvalResult'.
spawnProcessEval :: OsPath -> ProcessEvalConfig -> IO ProcessEvalOutput
spawnProcessEval selfPath config = do
  readProcess processConfig <&> \ (_, out, err) ->
    ProcessEvalOutput {
      result = first (decodeError out) (Aeson.eitherDecode out),
      processStderr = fromUtf8Lazy err
    }
  where
    processConfig = setStdin (byteStringInput (Aeson.encode config)) (proc (fromOsPath selfPath) ["eval"])

    decodeError out err =
      "Could not decode the subprocess result: " <> pack err <> "\nstdout: " <> fromUtf8Lazy out

-- | Parent-side entry point used by 'GhcServer.Build.Propagate.dispatchTask' in place of a direct
-- 'GhcServer.Build.Execute.executeModuleTask' call, when the unit's execute tasks were requested with
-- @--process@. Spawns a fresh @ghc-server@ subprocess (via 'getExecutablePath', i.e. the currently running
-- executable) to run exactly this one module's execute task, forwards everything the child recorded to the
-- parent's logger, and converts the child's 'EvalOutcome' back into a 'TaskResult'.
executeModuleTaskProcess :: BuildEnv -> Unit -> GHC.ModuleName -> IO (Maybe (TaskResult String))
executeModuleTaskProcess buildEnv unit modName = do
  buildEnv.log.debug ("Executing " ++ moduleNameString modName ++ " in a subprocess")
  bracket acquireSharedBytecode (traverse_ cleanupSharedBytecode) \ sharedBytecodePath -> do
    try (spawnProcessEval' sharedBytecodePath) >>= \case
      Left err -> pure (Just (TaskFailed (subprocessCrashMessage err)))
      Right result -> pure result
  where
    -- Mirror and export the parent's currently compiled bytecode into shared memory for the child to restore,
    -- unless the @sharedMemory@ feature is disabled, in which case the child falls back to its usual approach
    -- of restoring cached interfaces\/objects and compiling Core bindings to bytecode itself.
    acquireSharedBytecode
      | featureOn FeatureSharedMemory buildEnv.baseArgs.settings = do
          state <- readMVar buildEnv.stateVar
          bytecodeMap <- collectBytecode state.make.hug
          path <- exportSharedBytecode bytecodeMap
          case path of
            Just p ->
              buildEnv.log.debug (
                "Stored bytecode for " ++ show (Map.size bytecodeMap) ++ " module(s) in shared memory at " ++ p
                )
            Nothing -> buildEnv.log.debug "No mirrorable bytecode to store in shared memory"
          pure path
      | otherwise = do
          buildEnv.log.debug "sharedMemory feature disabled; subprocess will restore bytecode from cached interfaces"
          pure Nothing

    spawnProcessEval' sharedBytecodePath = do
      self <- toOsPath <$> getExecutablePath
      output <- spawnProcessEval self (cfg sharedBytecodePath)
      unless (Text.null output.processStderr) do
        buildEnv.log.info ("Unexpected subprocess stderr: " ++ Text.unpack output.processStderr)
      case output.result of
        Left err -> pure (Just (TaskFailed (Text.unpack err)))
        Right result -> do
          traverse_ (debugT buildEnv.log) result.logMessages
          traverse_ (debugT buildEnv.log) (captured result)
          pure (taskResult result)

    -- A crash here (process spawn failure, the child being killed, or any other exception thrown by
    -- 'spawnProcessEval') must never escape uncaught: this runs on the scheduler's own thread, and an
    -- uncaught exception there silently kills the scheduler loop, leaving every subsequent
    -- 'awaitIdle'\/'awaitBuild' call blocked forever with no diagnostic (see @kb-standalone-server@'s
    -- "reproducing scheduler issues" pitfall). Converting it into a 'TaskFailed' keeps the failure visible
    -- and the scheduler alive.
    subprocessCrashMessage err =
      "Subprocess for " ++ moduleNameString modName ++ " crashed: " ++ displayException (err :: SomeException)

    taskResult result =
      case result.outcome of
        EvalSuccess payload -> Just (TaskSuccess payload)
        EvalNoMain -> Nothing
        EvalExecuteFailed msg -> failed msg result
        EvalConfigInvalid msg -> failed ("Invalid subprocess config: " <> msg) result
        EvalCacheUnreadable msg -> failed ("Could not load the cached unit: " <> msg) result
        EvalCacheMissing -> failed "No cached unit on disk" result

    failed msg result =
      Just (TaskFailed (Text.unpack (Text.unlines (msg : captured result))))

    -- The evaluated module's output, as labeled blocks, omitting the streams that stayed empty.
    captured result =
      [
        label <> ":\n" <> text
        | (label, text) <- [("Subprocess stdout", result.evalStdout), ("Subprocess stderr", result.evalStderr)]
        , not (Text.null text)
      ]

    cfg sharedBytecodePath =
      ProcessEvalConfig {
        projectRoot = buildEnv.projectRoot,
        unit,
        moduleName = fromGhcModuleName modName,
        sharedBytecodePath
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
  Lazy.ByteString.putStr (Aeson.encode ProcessEvalResult {
    evalStdout = pack evalStdout,
    evalStderr = pack evalStderr,
    logMessages,
    outcome
  })
