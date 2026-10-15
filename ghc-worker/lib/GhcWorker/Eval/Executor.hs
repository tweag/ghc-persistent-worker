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

import Control.Concurrent.MVar (MVar, modifyMVar, readMVar, withMVar)
import Control.Monad (when)
import Data.Char (isAlphaNum)
import Data.Foldable (for_, traverse_)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import GHC.Utils.Outputable (text)
import GhcWorker.Executor.Client (
  awaitExecutor,
  isRunning,
  rtsArgs,
  spawnExecutorWith,
  stopExecutor,
  watchExecutor,
  )
import GhcWorker.Executor.Plan (PlanError (..), displayPlanError, executePlanned)
import GhcWorker.Instrumentation (EvalReport (..), failedEval)
import System.Directory (getTemporaryDirectory)
import System.Environment (getExecutablePath)
import System.FilePath ((</>))
import System.OsPath.Extra (toOsPath)
import System.Posix.Process (getProcessID)
import Types.Api (Event, ExecutorId (..))
import Types.ExecutorPlan (EntryKind, ExecutorPlan)
import Types.Log (Logger (..))
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

-- | The socket an executor serves on. The id is sanitized, since it is chosen by the rule.
executorSocket :: ExecutorId -> IO FilePath
executorSocket executorId = do
  prefix <- tempPrefix
  pure (prefix ++ "-" ++ sanitized ++ ".sock")
  where
    sanitized = [if isAlphaNum c || c == '-' then c else '_' | c <- Text.unpack executorId.text]

-- | Bytecode payloads are written to files in a directory that is private to the worker process.
payloadTransport :: IO PayloadTransport
payloadTransport =
  PayloadFiles . (++ "-payloads") <$> tempPrefix

-- | Launch a new executor, without waiting for it to become ready.
spawnExecutor :: ExecutorId -> IO ExecutorHandle
spawnExecutor executorId = do
  self <- getExecutablePath
  socket <- executorSocket executorId
  -- The executor doesn't need the worker's capabilities; the compiled-in default is @-N@.
  spawnExecutorWith self (rtsArgs ["-N2"]) (toOsPath socket)

-- | Return the registered executor if the predicate accepts it, otherwise stop it and register a new one, and wait
-- until it serves its socket.
-- A new process is observed for status events, which are published with the given action.
acquireExecutor ::
  (Event -> IO ()) ->
  (ExecutorHandle -> IO Bool) ->
  MVar WorkerState ->
  ExecutorId ->
  IO ExecutorHandle
acquireExecutor emit reuse stateVar executorId = do
  (handle, spawned) <- modifyMVar stateVar \ state -> do
    let existing = Map.lookup executorId state.executors
    reusable <- maybe (pure False) reuse existing
    case existing of
      Just handle | reusable -> pure (state, (handle, False))
      _ -> do
        traverse_ stopExecutor existing
        spawned <- spawnExecutor executorId
        -- Keep the call lock, so that callers still holding it stay serialized with the new process.
        let handle = maybe spawned (\ old -> spawned {calls = old.calls, incarnation = old.incarnation + 1}) existing
        pure (state {executors = Map.insert executorId handle state.executors}, (handle, True))
  awaitExecutor handle
  when spawned (watchExecutor emit stateVar executorId handle)
  pure handle

-- | Replace the executor after a failed call.
--
-- The incarnation is that of the handle the caller saw failing. If the registered executor is a newer, running one,
-- another target has replaced it already and it is kept, since stopping it would kill that target's calls.
-- Waits for calls in flight on the old process before stopping it.
respawnExecutor :: (Event -> IO ()) -> MVar WorkerState -> ExecutorId -> Int -> IO ExecutorHandle
respawnExecutor emit stateVar executorId failed =
  (Map.lookup executorId . (.executors) <$> readMVar stateVar) >>= \case
    Just handle -> withMVar handle.calls \ () -> replace
    Nothing -> replace
  where
    replace = acquireExecutor emit replaced stateVar executorId
    replaced handle
      | handle.incarnation == failed = pure False
      | otherwise = isRunning handle

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
  HomeModuleId ->
  String ->
  EntryKind ->
  IO EvalReport
runInExecutor emit logger stateVar executorId options root entry kind = do
  transport <- payloadTransport
  let execute = executePlanned id transport stateVar executorId options root entry kind
  seen <- acquireExecutor emit isRunning stateVar executorId
  first <- execute
  result <-
    if needsRespawn first
    then respawnExecutor emit stateVar executorId seen.incarnation *> execute
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
