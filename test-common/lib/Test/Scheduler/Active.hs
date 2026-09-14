-- | Classification of active tasks, which go straight into the dependency-tracking pools.
module Test.Scheduler.Active where

import Data.Foldable (foldrM)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Set (Set)
import Test.Data.Scheduler (Phase (..), SchedulerDecision (..), SchedulerState (..), Task (..))
import Test.Scheduler.Decision (Traced, traceDecision)
import Test.Scheduler.State (promoteReady, satisfiedKeys)

-- | Classify a single active task: skip if already enqueued, otherwise insert into 'unsatisfied' and promote if ready.
--
-- @batch@ is the set of keys enqueued together with this task.
-- Those count as unmet dependencies even if they carry a completion record, because they are about to be activated
-- again.
-- Without this, whether a dependent waits for its dependency would hinge on the order in which the batch happens to be
-- folded over.
classifyTaskIn ::
  Ord (key 'Resolved) =>
  Set (key 'Resolved) ->
  Task key 'Resolved task ->
  SchedulerState key task f ext ->
  Traced key (SchedulerState key task f ext)
classifyTaskIn batch task state =
  if Map.member task.key state.accepted
  then pure state
  else
    newState <$ traceDecision (DecisionActivated task.key state.generation)
  where
    newState =
      promoteReady state {
        unsatisfied = Map.insert task.key (task, unmet) state.unsatisfied,
        accepted = Map.insert task.key state.generation state.accepted
      }

    unmet = Set.difference task.deps (Set.difference (satisfiedKeys state) batch)

-- | Enqueue pre-built active tasks directly. Skips tasks whose keys are already known.
--
-- This is intended for use by dispatch callbacks that generate follow-up tasks (e.g. compile tasks after metadata
-- completion).
enqueueTasks ::
  Ord (key 'Resolved) =>
  [Task key 'Resolved task] ->
  SchedulerState key task f ext ->
  Traced key (SchedulerState key task f ext)
enqueueTasks tasks state =
  foldrM (classifyTaskIn batch) state tasks
  where
    batch = Set.fromList [task.key | task <- tasks]
