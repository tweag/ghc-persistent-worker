-- | Run a unit's @execute@ task in a genuinely separate OS process, spawned via @typed-process@, rather than
-- in-process (see @kb-process-isolation@ for why: forking a live multi-capability GHC session deadlocks, but a
-- fresh subprocess relaunch does not). This module is the parent side only; the child side (decoding the
-- config, restoring the module map, running the evaluation, and writing the JSON result) lives in
-- 'GhcServer.Build.ProcessChild'.
--
-- The child is a relaunch of the running @ghc-server@ executable (detected via 'evalProcessFlag' in its argv,
-- see 'GhcServer.Run.runServer').
--
-- The child's stdio follows a strict protocol: it captures everything the evaluated module writes to stdout and
-- stderr with 'hCapture', routes its own operational messages into an in-memory logger, and writes exactly one
-- JSON-encoded 'ProcessEvalResult' to stdout before exiting. The parent therefore parses the child's entire
-- stdout as a single JSON document and treats any child stderr output as unexpected.
--
-- Originally prototyped as a self-contained test (@Test.SubprocessTest@, 'test_subprocessExecute'); this module
-- is the production extraction of that experiment's parent-process logic.
module GhcServer.Build.Process where

import Control.Concurrent.MVar (readMVar)
import Control.Exception (SomeException, bracket, displayException, try)
import Control.Monad (unless)
import qualified Data.Aeson as Aeson
import Data.Bifunctor (first)
import Data.Foldable (traverse_)
import Data.Functor ((<&>))
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Data.Text (pack)
import qualified GHC
import GHC (moduleNameString)
import GhcServer.Build.SharedBytecode (cleanupSharedBytecode, collectBytecode, exportSharedBytecode)
import GhcServer.Data.BuildEnv (BuildEnv (..))
import GhcServer.Data.ProcessEval (
  EvalOutcome (..),
  ProcessEvalConfig (..),
  ProcessEvalOutput (..),
  ProcessEvalResult (..),
  )
import GhcServer.Data.Unit (Unit (..))
import GhcServer.Log (instrumentLogger)
import Internal.State (dynamicFeatureOn)
import System.Environment (getExecutablePath)
import System.OsPath.Extra (OsPath, fromOsPath, toOsPath)
import System.Process.Typed (byteStringInput, proc, readProcess, setStdin)
import Test.Scheduler (TaskResult (..))
import Types.Api (ProcessStats, UnitName (..), fromGhcModuleName)
import Types.ByteString (fromUtf8Lazy)
import Types.FeatureFlags (Feature (..))
import Types.Log (Logger (..), debugT)
import Types.State (WorkerState (..))
import Types.State.Make (MakeState (..))

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
-- parent's logger, and converts the child's 'EvalOutcome' back into a 'TaskResult', alongside the RTS stats
-- the child reported (see 'GhcServer.Data.ProcessEval.ProcessEvalResult'), if the child got far enough to
-- report a result at all.
executeModuleTaskProcess :: BuildEnv -> Unit -> GHC.ModuleName -> IO (Maybe (TaskResult String), Maybe ProcessStats)
executeModuleTaskProcess buildEnv =
  executeModuleTaskWith runChild buildEnv
  where
    runChild config = do
      self <- toOsPath <$> getExecutablePath
      spawnProcessEval self config

-- | Run a module's execute task out of process via the given transport, which sends a 'ProcessEvalConfig' to some
-- child process and returns its output: either a fresh one-shot subprocess ('executeModuleTaskProcess') or a
-- persistent executor ('GhcServer.Build.Executor.executeModuleTaskExecutor').
--
-- Exports the parent's bytecode to shared memory for the duration of the call, forwards everything the child
-- recorded to the parent's logger, and converts the child's 'EvalOutcome' into a 'TaskResult'.
executeModuleTaskWith ::
  (ProcessEvalConfig -> IO ProcessEvalOutput) ->
  BuildEnv ->
  Unit ->
  GHC.ModuleName ->
  IO (Maybe (TaskResult String), Maybe ProcessStats)
executeModuleTaskWith runChild buildEnv unit modName = do
  logger.debug ("Executing " ++ moduleNameString modName ++ " in a subprocess")
  bracket acquireSharedBytecode (traverse_ cleanupSharedBytecode) \ sharedBytecodePath -> do
    try (spawnProcessEval' sharedBytecodePath) >>= \case
      Left err -> pure (Just (TaskFailed (subprocessCrashMessage err)), Nothing)
      Right result -> pure result
  where
    -- Forward every message logged in this function to the instrument channel, tagged the same way
    -- 'GhcServer.Build.Compile.withModuleSession' tags its own per-task logger, so that messages logged here
    -- (which run on the parent side, outside any GHC session and thus outside 'withModuleSession') are actually
    -- visible to the UI instead of only accumulating in the un-flushed, non-instrumented 'BuildEnv.log'.
    logger = instrumentLogger buildEnv.instrChan logCategory buildEnv.log

    logCategory = Text.unpack unit.name.text ++ ":" ++ moduleNameString modName ++ ":process"
    -- Mirror and export the parent's currently compiled bytecode into shared memory for the child to restore,
    -- unless the @sharedMemory@ feature is disabled, in which case the child falls back to its usual approach
    -- of restoring cached interfaces\/objects and compiling Core bindings to bytecode itself.
    acquireSharedBytecode =
      dynamicFeatureOn FeatureSharedMemory buildEnv.stateVar >>= \case
        True -> do
          state <- readMVar buildEnv.stateVar
          bytecodeMap <- collectBytecode state.make.hug
          path <- exportSharedBytecode bytecodeMap
          case path of
            Just p ->
              logger.debug (
                "Stored bytecode for " ++ show (Map.size bytecodeMap) ++ " module(s) in shared memory at " ++ p
                )
            Nothing -> logger.debug "No mirrorable bytecode to store in shared memory"
          pure path
        False -> do
          logger.debug "sharedMemory feature disabled; subprocess will restore bytecode from cached interfaces"
          pure Nothing

    spawnProcessEval' sharedBytecodePath = do
      output <- runChild (cfg sharedBytecodePath)
      unless (Text.null output.processStderr) do
        logger.info ("Unexpected subprocess stderr: " ++ Text.unpack output.processStderr)
      case output.result of
        Left err -> pure (Just (TaskFailed (Text.unpack err)), Nothing)
        Right result -> do
          traverse_ (debugT logger) result.logMessages
          traverse_ (debugT logger) (captured result)
          pure (taskResult result, Just result.stats)

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

