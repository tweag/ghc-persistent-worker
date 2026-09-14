-- | Generic concurrent scheduler with inbox-based architecture.
--
-- External clients submit work via 'submitRequest'.  The scheduler loop
-- classifies requests into tasks (via 'classify'), respects
-- inter-task dependency ordering, and dispatches ready tasks up to a
-- configurable concurrency limit.
--
-- Tasks exist in two pools:
--
-- * Active (unsatisfied or ready): participate in dependency tracking and dispatch.
-- * Pending: invisible to 'awaitIdle'.  They are activated by 'addResolutions'
--   (enabled pending tasks that now have resolution entries) or immediately
--   at insertion time if a resolution already exists.
--
-- Resolution entries (mapping pending keys to resolved keys, values, and
-- dependency sets) are stored in 'SchedulerState' and populated by the
-- build layer through the 'propagate' callback via 'addResolutions'.
--
-- On task completion, the 'propagate' callback lets the build layer apply
-- domain-specific effects (e.g. computing resolution maps from metadata
-- results and promoting compile tasks).
module Test.Scheduler where

import Control.Concurrent.Async (Async, async)
import Control.Concurrent.STM (
  STM,
  atomically,
  check,
  isEmptyTQueue,
  newTQueueIO,
  newTVarIO,
  readTQueue,
  readTVar,
  readTVarIO,
  writeTQueue,
  )
import Control.Concurrent.STM.TQueue (peekTQueue)
import Control.Monad (forever, (>=>))
import Data.Foldable (traverse_)
import qualified Data.Map.Strict as Map
import Data.Void (Void)
import Test.Data.Scheduler (
  Handlers (..),
  OrdKey,
  Phase (..),
  Propagated (..),
  SchedulerEnv (..),
  SchedulerEvent (..),
  SchedulerResources (..),
  SchedulerState (..),
  )
import Test.Scheduler.Active (enqueueTasks)
import Test.Scheduler.Decision (Traced, modifyTraced)
import Test.Scheduler.Dispatch (fillSlots)
import Test.Scheduler.Pending (addResolutions, enqueuePending)
import Test.Scheduler.State (bumpGeneration, initialSchedulerState, recordResult)

-- | Apply the outcome of the 'propagate' handler to the scheduler state: install the new extension state, merge the
-- resolutions and record the task's result.
applyPropagated ::
  OrdKey key =>
  key 'Resolved ->
  Propagated key f ext ->
  SchedulerState key task f ext ->
  Traced key (SchedulerState key task f ext)
applyPropagated key propagated state =
  addResolutions propagated.newResolutions state {ext = propagated.newExt} >>= recordResult key propagated.result

-- | Handle a single event: classify inbox tasks or record completions.
processEvent ::
  OrdKey key =>
  SchedulerEnv request key task f ext ->
  SchedulerResources request key task f ext ->
  SchedulerEvent request key f ->
  IO ()
processEvent env resources = \case
  RequestEvent req -> do
    (activeTasks, pendingTasks) <- env.handlers.classify req
    decisions <- atomically do
      modifyTraced resources.state (bumpGeneration >=> enqueueTasks activeTasks >=> enqueuePending pendingTasks)
    traverse_ env.handlers.reportDecision decisions
  CompletionEvent key result -> do
    propagated <- env.handlers.propagate key result =<< readTVarIO resources.state
    decisions <- atomically do
      modifyTraced resources.state (applyPropagated key propagated)
    traverse_ env.handlers.reportDecision decisions

-- | Main scheduler loop. Reads one event at a time, processes it and dispatches ready tasks.
schedulerLoop ::
  OrdKey key =>
  SchedulerEnv request key task f ext ->
  SchedulerResources request key task f ext ->
  IO Void
schedulerLoop env resources =
  forever do
    fillSlots env resources
    event <- atomically do
      peekTQueue resources.events
    processEvent env resources event
    atomically do
      readTQueue resources.events

-- API -------------------------------------------------------------------

-- | Start the scheduler loop in a background thread.
runScheduler ::
  OrdKey key =>
  SchedulerEnv request key task f ext ->
  SchedulerResources request key task f ext ->
  IO (Async Void)
runScheduler env resources =
  async (schedulerLoop env resources)

newSchedulerState :: ext -> IO (SchedulerResources request key task f ext)
newSchedulerState initialExt = do
  events <- newTQueueIO
  state <- newTVarIO (initialSchedulerState initialExt)
  pure SchedulerResources {events, state}

submitRequest :: SchedulerResources request key task f ext -> request -> IO ()
submitRequest resources request =
  atomically (writeTQueue resources.events (RequestEvent request))

-- | Block until the scheduler is idle: no active tasks, no ready tasks, no unsatisfied tasks, and no pending events in
-- the queue.
--
-- Pending tasks are excluded, they are not considered active work.
--
-- This is the primary termination criterion for tests and single-shot builds.
awaitIdle :: SchedulerResources request key task f ext -> STM ()
awaitIdle resources = do
  empty <- isEmptyTQueue resources.events
  check empty
  state <- readTVar resources.state
  check (state.activeCount == 0 && null state.ready && Map.null state.unsatisfied)
