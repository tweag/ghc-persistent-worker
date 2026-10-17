{-# LANGUAGE CPP #-}

-- | Eval mode in persistent executor subprocesses.
--
-- A @haskell_eval@ target with an @executor@ attribute passes @--eval-executor <id>@ to the worker, which then runs
-- the evaluated expression in a subprocess instead of in the worker's own process.
-- All targets with the same id share one subprocess, which is a relaunch of the worker executable in @executor@ mode
-- (see 'GhcWorker.Executor.Child') and keeps the bytecode it loaded for earlier targets.
--
-- The worker still loads the target's module and its dependencies in its own session, like for in-process
-- evaluation, but only to obtain their bytecode ('GhcWorker.Executor.Plan'), which is sent to the executor as
-- serialized GBC payloads.
-- Evaluation results that the in-process mode would show via 'Show' aren't supported, see
-- 'Internal.Evaluate.exprEntryKind'.
module GhcWorker.Eval.Executor where

#if defined(GBC)

import Control.Concurrent.MVar (MVar, readMVar)
import Data.Foldable (for_)
import qualified Data.Text as Text
import GHC.Utils.Outputable (text)
import GhcWorker.Executor.Plan (executePlanned, needsRespawn)
import GhcWorker.Instrumentation (EvalReport (..), failedEval)
import Internal.Executor.Client (captureStdioArgs, rtsArgs, spawnExecutorWith)
import Internal.Executor.Lifecycle (ExecutorEnv (..), executorSocketName)
import qualified Internal.Executor.Lifecycle as Lifecycle
import System.Directory (getTemporaryDirectory)
import System.Environment (getExecutablePath)
import System.FilePath ((</>))
import System.OsPath.Extra (toOsPath)
import System.Posix.Process (getProcessID)
import Types.Api (Event, ExecutorId (..), RequestId)
import Types.ExecutorPlan (EntryKind, ExecutorPlan)
import Types.Log (Logger (..))
import Types.Settings (Settings (..))
import Types.Executor.Protocol (
  CallError,
  CallResult (..),
  Captured (..),
  EvalOutcome (..),
  SetupError,
  displayCallError,
  )
import Types.State (WorkerState (..))
import Types.State.Executor (ExecutorHandle (..), HomeModuleId)
import Types.ExecutorPlan (PayloadTransport (..))

-- | Prefix of the worker's temporary files for executors, unique per worker process.
tempPrefix :: IO FilePath
tempPrefix = do
  tmp <- getTemporaryDirectory
  pid <- getProcessID
  pure (shortDir tmp </> ("gw-" ++ show pid))
  where
    -- Unix socket paths are limited to 107 bytes.
    shortDir tmp
      | length tmp > 50 = "/tmp"
      | otherwise = tmp

-- | The socket an executor serves on, see 'executorSocketName'.
executorSocket :: ExecutorId -> IO FilePath
executorSocket executorId = do
  prefix <- tempPrefix
  pure (prefix ++ "-" ++ executorSocketName executorId)

-- | Bytecode payloads are written to files in a directory that is private to the worker process.
payloadTransport :: IO PayloadTransport
payloadTransport =
  PayloadFiles . (++ "-payloads") <$> tempPrefix

-- | Launch a new executor with the given extra arguments (capture flag and RTS options), without waiting for it to
-- become ready.
spawnExecutor :: [String] -> ExecutorId -> IO ExecutorHandle
spawnExecutor args executorId = do
  self <- getExecutablePath
  socket <- executorSocket executorId
  spawnExecutorWith self args (toOsPath socket)

-- | The environment of the shared executor lifecycle logic.
-- The executor arguments are derived from the worker's settings (see 'Types.Settings.Settings.executorRts' and
-- 'Types.FeatureFlags.FeatureExecutorCaptureStdio') up front, since the state is locked while an executor is spawned.
-- Status events of new executor processes are published with the given action.
executorEnv :: (Event -> IO ()) -> MVar WorkerState -> IO ExecutorEnv
executorEnv emit stateVar = do
  settings <- (.settings) <$> readMVar stateVar
  pure ExecutorEnv {
    stateVar,
    emit,
    spawn = spawnExecutor (captureStdioArgs settings ++ rtsArgs settings.executorRts)
  }

type PlannedOutput = Either CallError (ExecutorPlan, CallResult)

-- | Run the entry of a module in the executor, starting or restarting it if necessary, forward the output and
-- determine whether the evaluation succeeded.
-- 'Left' is an error message for a failure to evaluate, as opposed to a successful evaluation of a failing test.
-- Status events of new executor processes are published with the given action.
runInExecutor ::
  (Event -> IO ()) ->
  Logger ->
  MVar WorkerState ->
  ExecutorId ->
  RequestId ->
  [String] ->
  [(String, String)] ->
  HomeModuleId ->
  String ->
  EntryKind ->
  IO EvalReport
runInExecutor emit logger stateVar executorId requestId options env root entry kind = do
  transport <- payloadTransport
  let execute = executePlanned transport stateVar executorId requestId options env root entry kind
  executorEnv_ <- executorEnv emit stateVar
  seen <- Lifecycle.ensureExecutor executorEnv_ executorId
  first <- execute
  result <-
    if needsRespawn first
    then Lifecycle.respawnExecutor executorEnv_ executorId seen.incarnation *> execute
    else pure first
  reportResult logger result

-- | Forward the executor's output to the logger and convert the outcome.
reportResult :: Logger -> PlannedOutput -> IO EvalReport
reportResult logger = \case
  Left err -> pure (failedEval (displayCallError err))
  Right (_, result) -> do
    logger.infoD (text (Text.unpack result.captured.stdout))
    logger.infoD (text (Text.unpack result.captured.stderr))
    for_ result.logMessages (logger.info . Text.unpack)
    report <- outcomeResult logger result.outcome
    pure EvalReport {outcome = report.outcome, result = report.result, stats = Just result.stats}

outcomeResult :: Logger -> Either SetupError EvalOutcome -> IO EvalReport
outcomeResult logger = \case
  Right (EvalSucceeded mResult) -> do
    for_ mResult (logger.info . Text.unpack)
    pure EvalReport {outcome = Right True, result = Text.unpack <$> mResult, stats = Nothing}
  Right (EvalFailed err) -> pure (failedEval (Text.unpack err))
  other -> pure (failedEval ("Executor failed: " ++ show other))

#endif
