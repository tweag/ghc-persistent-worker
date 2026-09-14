-- | Running ready tasks in concurrent workers.
module Test.Scheduler.Dispatch where

import Control.Concurrent.Async (async)
import Control.Concurrent.STM (atomically, stateTVar, writeTQueue)
import Control.Exception (SomeAsyncException, fromException, throwIO, try)
import Control.Monad (void)
import Data.Foldable (traverse_)
import qualified Data.Map.Strict as Map
import System.Timeout (timeout)
import Test.Data.Scheduler (
  Handlers (..),
  Phase (..),
  SchedulerEnv (..),
  SchedulerEvent (..),
  SchedulerResources (..),
  SchedulerState (..),
  Task (..),
  TaskResult (..),
  )

-- | Execute the task's dispatch function with timeout and exception handling.
runTask ::
  SchedulerEnv request key task f ext ->
  ext ->
  Task key 'Resolved task ->
  IO (TaskResult f)
runTask env ext task =
  try (timeout (env.taskTimeout * 1_000_000) (env.handlers.dispatch ext task)) >>= \case
    Right (Just r) -> pure r
    Right Nothing -> pure (TaskFailed (env.mkFailure ("Task timed out after " ++ show env.taskTimeout ++ "s")))
    Left exc ->
      case fromException exc of
        Just (e :: SomeAsyncException) -> throwIO e
        Nothing -> pure (TaskFailed (env.mkFailure (show exc)))

-- | Run a task and signal completion via the event queue.
executeTask ::
  SchedulerEnv request key task f ext ->
  SchedulerResources request key task f ext ->
  ext ->
  Task key 'Resolved task ->
  IO ()
executeTask env resources ext task = do
  result <- runTask env ext task
  atomically (writeTQueue resources.events (CompletionEvent task.key result))

-- | Fork a task into an async worker.
startTask ::
  SchedulerEnv request key task f ext ->
  SchedulerResources request key task f ext ->
  ext ->
  Task key 'Resolved task ->
  IO ()
startTask env resources ext task =
  void (async (executeTask env resources ext task))

-- | Take as many tasks out of the ready pool as fit into the available job slots.
takeAvailableReadyTasks ::
  SchedulerEnv request key task f ext ->
  SchedulerState key task f ext ->
  ([(ext, Task key 'Resolved task)], SchedulerState key task f ext)
takeAvailableReadyTasks env state =
  (zip (repeat state.ext) dispatch, state {ready = excess, activeCount = state.activeCount + length dispatch})
  where
    available = env.maxJobs - state.activeCount

    (dispatch, excess) = splitAt available state.ready

-- | Take ready tasks from the pool up to the job limit and start them unless the build has failed.
fillSlots ::
  SchedulerEnv request key task f ext ->
  SchedulerResources request key task f ext ->
  IO ()
fillSlots env resources =
  traverse_ (uncurry (startTask env resources)) =<< atomically (stateTVar resources.state checkFailures)
  where
    checkFailures state =
      if null state.failures || env.continueOnFailure
      then takeAvailableReadyTasks env state
      else ([], state {ready = [], unsatisfied = Map.empty})
