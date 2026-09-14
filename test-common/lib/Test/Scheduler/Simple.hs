-- | Simple scheduler for pre-sorted task lists.
--
-- Delegates to 'Test.Scheduler' for the actual concurrent dispatch,
-- wrapping the simple (non-phase-indexed) task types into the concurrent
-- scheduler's phase-indexed types via 'SimpleKey'.
module Test.Scheduler.Simple where

import Control.Concurrent.Async (cancel)
import Control.Concurrent.STM (atomically, modifyTVar', readTVarIO)
import Data.Coerce (coerce)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Set (Set)
import Test.Data.Env (MaxJobs (..))
import Test.Data.Schedule (
  Dispatch,
  RequestFailure (..),
  RequestResult (..),
  Schedule (..),
  ScheduleEnv (..),
  ScheduleNode (..),
  ScheduleProgress (..),
  runDispatch,
  )
import qualified Test.Data.Scheduler as Generic
import Test.Data.Scheduler (
  Handlers (..),
  Phase (..),
  Propagated (..),
  SchedulerEnv (..),
  SchedulerResources (..),
  Task (..),
  )
import qualified Test.Scheduler as Generic
import Test.Scheduler (awaitIdle, newSchedulerState, submitRequest)

-- | Phase-agnostic key wrapper: both phases use the same underlying key type.
data SimpleKey key (p :: Phase) where
  SK :: key -> SimpleKey key p

deriving stock instance Show key => Show (SimpleKey key p)
deriving stock instance Eq key => Eq (SimpleKey key p)
deriving stock instance Ord key => Ord (SimpleKey key p)

unwrapKey :: SimpleKey key p -> key
unwrapKey (SK k) = k

-- | The set of keys that have completed, discarding their generation stamps.
completedKeys :: Generic.SchedulerState key task f ext -> Set (key 'Resolved)
completedKeys state = Map.keysSet state.completed

-- | Convert a 'ScheduleNode' to the concurrent scheduler's phase-indexed 'Generic.Task'.
convertTask :: Ord key => ScheduleNode key task -> Task (SimpleKey key) 'Resolved task
convertTask ScheduleNode {key, deps, value} =
  Task {key = SK key, deps = Set.map SK deps, enabled = True, value}

-- | Set up a scheduler environment and initial state.
initScheduler ::
  MaxJobs ->
  Dispatch task ->
  Schedule key task ->
  Set key ->
  (ScheduleEnv key task, ScheduleProgress key task)
initScheduler maxJobs dispatch tasks completed =
  (env, state)
  where
    env = ScheduleEnv {maxJobs, dispatch}

    state = ScheduleProgress {
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
  ScheduleEnv key task ->
  ScheduleProgress key task ->
  IO (ScheduleProgress key task)
runScheduler env initialState = do
  resources <- newSchedulerState ()
  atomically $ modifyTVar' resources.state \ s ->
    s {Generic.completed = Map.fromSet (const 0) (Set.map SK initialState.completed)}
  let
    wrapDispatch _ext task = do
      result <- runDispatch env.dispatch task.value
      pure $ case result of
        RequestSuccess -> Generic.TaskSuccess Nothing
        RequestFailure f -> Generic.TaskFailed f

    handlers = Handlers {
      dispatch = wrapDispatch,
      classify = \ tasks -> pure (tasks, []),
      propagate = \ _ result s -> pure Propagated {result, newExt = s.ext, newResolutions = Map.empty},
      reportDecision = \ _ -> pure ()
    }

    cEnv = SchedulerEnv {
      maxJobs = coerce env.maxJobs,
      handlers,
      taskTimeout = 10,
      mkFailure = RequestFatal,
      continueOnFailure = False
    }
  thread <- Generic.runScheduler cEnv resources
  submitRequest resources (map convertTask (reverse initialState.schedule.tasks))
  atomically (awaitIdle resources)
  cancel thread
  cState <- readTVarIO resources.state
  pure ScheduleProgress {
    schedule = Schedule [],
    completed = Set.map unwrapKey (completedKeys cState),
    failures = Map.mapKeys unwrapKey cState.failures
  }
