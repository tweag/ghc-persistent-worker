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
import GhcWorker.Executor.Plan (PlanError (..), displayPlanError, executePlanned)
import GhcWorker.Instrumentation (EvalReport (..), failedEval)
import Internal.Executor.Client (rtsArgs, spawnExecutorWith)
import Internal.Executor.Lifecycle (ExecutorEnv (..), executorSocketName)
import qualified Internal.Executor.Lifecycle as Lifecycle
import System.Directory (getTemporaryDirectory)
import System.Environment (getExecutablePath)
import System.FilePath ((</>))
import System.OsPath.Extra (toOsPath)
import System.Posix.Process (getProcessID)
import Types.Api (Event, ExecutorId (..))
import Types.ExecutorPlan (EntryKind, ExecutorPlan)
import Types.Log (Logger (..))
import Types.Settings (Settings (..))
import Types.ProcessEval (EvalOutcome (..), ProcessEvalOutput (..), ProcessEvalResult (..))
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

-- | Launch a new executor with the given RTS options, without waiting for it to become ready.
spawnExecutor :: [String] -> ExecutorId -> IO ExecutorHandle
spawnExecutor rts executorId = do
  self <- getExecutablePath
  socket <- executorSocket executorId
  spawnExecutorWith self (rtsArgs rts) (toOsPath socket)

-- | The environment of the shared executor lifecycle logic.
-- The RTS options are read from the worker's settings (see 'Types.Settings.Settings.executorRts') up front, since the
-- state is locked while an executor is spawned.
-- Status events of new executor processes are published with the given action.
executorEnv :: (Event -> IO ()) -> MVar WorkerState -> IO ExecutorEnv
executorEnv emit stateVar = do
  rts <- (.settings.executorRts) <$> readMVar stateVar
  pure ExecutorEnv {stateVar, emit, spawn = spawnExecutor rts}

type PlannedOutput = Either PlanError (ExecutorPlan, ProcessEvalOutput)

-- | Whether the executor couldn't be reached or its session has incompatible options.
needsRespawn :: PlannedOutput -> Bool
needsRespawn = \case
  Left (ExecutorUnreachable _) -> True
  Right (_, ProcessEvalOutput {result = Right ProcessEvalResult {outcome = EvalOptionsMismatch _}}) -> True
  _ -> False

-- | Run the entry of a module in the executor, starting or restarting it if necessary, forward the output and
-- determine whether the evaluation succeeded.
-- 'Left' is an error message for a failure to evaluate, as opposed to a successful evaluation of a failing test.
-- Status events of new executor processes are published with the given action.
runInExecutor ::
  (Event -> IO ()) ->
  Logger ->
  MVar WorkerState ->
  ExecutorId ->
  [String] ->
  [(String, String)] ->
  HomeModuleId ->
  String ->
  EntryKind ->
  IO EvalReport
runInExecutor emit logger stateVar executorId options env root entry kind = do
  transport <- payloadTransport
  let execute = executePlanned id transport stateVar executorId options env root entry kind
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
  Left err -> pure (failedEval (displayPlanError err))
  Right (_, output) -> case output.result of
    Left err -> pure (failedEval (Text.unpack err))
    Right result -> do
      logger.infoD (text (Text.unpack result.evalStdout))
      logger.infoD (text (Text.unpack result.evalStderr))
      for_ result.logMessages (logger.info . Text.unpack)
      report <- outcomeResult logger result.outcome
      pure EvalReport {outcome = report.outcome, result = report.result, stats = Just result.stats}

outcomeResult :: Logger -> EvalOutcome -> IO EvalReport
outcomeResult logger = \case
  EvalSuccess mResult -> do
    for_ mResult (logger.info . Text.unpack)
    pure EvalReport {outcome = Right True, result = Text.unpack <$> mResult, stats = Nothing}
  EvalExecuteFailed err -> pure (failedEval (Text.unpack err))
  other -> pure (failedEval ("Executor failed: " ++ show other))

#endif
