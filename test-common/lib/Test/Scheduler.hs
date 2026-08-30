-- | Simple scheduler for pre-sorted task lists.
--
-- Delegates to 'Test.Scheduler.Concurrent' for the actual concurrent dispatch,
-- wrapping the simple (non-phase-indexed) task types into the concurrent
-- scheduler's phase-indexed types via 'SimpleKey'.
module Test.Scheduler (
  SimpleKey (..),
  runScheduler,
  initScheduler,
) where

import Control.Concurrent.Async (cancel)
import Control.Concurrent.STM (atomically, modifyTVar', readTVarIO)
import Data.Coerce (coerce)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Set (Set)
import Test.Data.Env (MaxJobs (..))
import Test.Data.Scheduler (
  Dispatch,
  RequestFailure (..),
  RequestResult (..),
  Schedule (..),
  SchedulerEnv (..),
  SchedulerState (..),
  Task (..),
  runDispatch,
  )
import qualified Test.Scheduler.Concurrent as C

-- | Phase-agnostic key wrapper: both phases use the same underlying key type.
data SimpleKey key (p :: C.Phase) where
  SK :: key -> SimpleKey key p

deriving stock instance Show key => Show (SimpleKey key p)
deriving stock instance Eq key => Eq (SimpleKey key p)
deriving stock instance Ord key => Ord (SimpleKey key p)

unwrapKey :: SimpleKey key p -> key
unwrapKey (SK k) = k

-- | Convert a simple 'Task' to the concurrent scheduler's phase-indexed 'Task'.
convertTask :: Ord key => Task key task -> C.Task (SimpleKey key) 'C.Resolved task
convertTask Task {key, deps, value} =
  C.Task {key = SK key, deps = Set.map SK deps, enabled = True, value}

-- | Set up a scheduler environment and initial state.
initScheduler ::
  MaxJobs ->
  Dispatch task ->
  Schedule key task ->
  Set key ->
  (SchedulerEnv key task, SchedulerState key task)
initScheduler maxJobs dispatch tasks completed =
  (env, state)
  where
    env = SchedulerEnv {maxJobs, dispatch}

    state = SchedulerState {
      schedule = tasks,
      completed,
      failures = Map.empty
    }

-- | Run a pre-sorted schedule of tasks to completion using the concurrent scheduler.
--
-- All tasks are submitted as a single batch and dispatched concurrently up to
-- the configured job limit.  Dependencies are respected via the concurrent
-- scheduler's unsatisfied\/ready tracking.
--
-- Early termination: when any task fails, the scheduler stops dispatching
-- further tasks and waits for active tasks to complete.
runScheduler ::
  Ord key =>
  SchedulerEnv key task ->
  SchedulerState key task ->
  IO (SchedulerState key task)
runScheduler env initialState = do
  resources <- C.newSchedulerState C.TracingOff ()
  atomically $ modifyTVar' resources.state \ s ->
    s {C.completed = Map.fromSet (const C.initialGeneration) (Set.map SK initialState.completed)}
  let
    wrapDispatch _ext task = do
      result <- runDispatch env.dispatch task.value
      pure $ case result of
        RequestSuccess -> C.TaskSuccess Nothing
        RequestFailure f -> C.TaskFailed f

    handlers = C.Handlers {
      dispatch = wrapDispatch,
      classify = \ tasks -> pure (tasks, []),
      propagate = \ _ _ s -> pure s
    }

    cEnv = C.SchedulerEnv {
      maxJobs = coerce env.maxJobs,
      handlers,
      taskTimeout = 10,
      mkFailure = RequestFatal,
      continueOnFailure = False
    }
  thread <- C.runScheduler cEnv resources
  C.submitRequest resources (map convertTask (reverse initialState.schedule.tasks))
  atomically (C.awaitIdle resources)
  cancel thread
  cState <- readTVarIO resources.state
  pure SchedulerState {
    schedule = Schedule [],
    completed = Set.map unwrapKey (C.completedKeys cState),
    failures = Map.mapKeys unwrapKey cState.failures
  }

