-- | Basic queries and transitions on 'SchedulerState' that are shared by the active and pending task pools.
module Test.Scheduler.State where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Set (Set)
import Test.Data.Scheduler (
  Activation (..),
  Phase (..),
  Resolution (..),
  SchedulerDecision (..),
  SchedulerState (..),
  TaskResult (..),
  )
import Test.Scheduler.Decision (Traced, traceDecision)

-- | The state of a scheduler that has not processed any request yet.
initialSchedulerState :: ext -> SchedulerState key task f ext
initialSchedulerState initialExt =
  SchedulerState {
    unsatisfied = Map.empty,
    ready = [],
    pending = Map.empty,
    completed = Map.empty,
    accepted = Map.empty,
    activeCount = 0,
    failures = Map.empty,
    resolutions = Map.empty,
    generation = 0,
    ext = initialExt
  }

-- | The keys that a new task may consider to be satisfied dependencies, consisting of the set of completed keys without
-- the set of accepted keys.
-- If a task is completed, it can subsequently get accepted again when it is determined to be stale.
-- This ensures that modified dependencies are recompiled.
satisfiedKeys :: Ord (key 'Resolved) => SchedulerState key task f ext -> Set (key 'Resolved)
satisfiedKeys state =
  Set.difference (Map.keysSet state.completed) (Map.keysSet state.accepted)

-- | Start a new generation.
-- Called once per external request.
bumpGeneration ::
  SchedulerState key task f ext ->
  Traced key (SchedulerState key task f ext)
bumpGeneration state = do
  traceDecision (DecisionGeneration generation)
  pure state {generation}
  where
    generation = succ state.generation

-- | Decide whether a resolution entry justifies (re-)activating its resolved key.
--
-- The in-flight check comes first and is pure deduplication: two requests within the same
-- in-flight window must not both dispatch the key.
--
-- The up-to-date check is the version comparison: the entry is only a reason to run if the
-- staleness analysis that produced it is /newer/ than the last run of the key.
activation ::
  Ord (key 'Resolved) =>
  Resolution key ->
  SchedulerState key task f ext ->
  Activation
activation resolution state
  | Just g <- Map.lookup resolution.key state.accepted = SkipInFlight g
  | Just g <- Map.lookup resolution.key state.completed
  , resolution.computedAt <= g
  = SkipUpToDate g
  | otherwise = ActivateNow

-- | Move all unsatisfied tasks with empty dep sets to the ready list.
promoteReady ::
  SchedulerState key task f ext ->
  SchedulerState key task f ext
promoteReady state =
  state {unsatisfied, ready = state.ready ++ (fst <$> Map.elems readyNow)}
  where
    (readyNow, unsatisfied) = Map.partition (null . snd) state.unsatisfied

-- | Record a task result: update completed set, decrement active count,
-- remove key from dep sets, promote newly ready tasks.
recordResult ::
  Ord (key 'Resolved) =>
  key 'Resolved ->
  TaskResult f ->
  SchedulerState key task f ext ->
  Traced key (SchedulerState key task f ext)
recordResult key result state = do
  traceDecision (DecisionCompleted key generation)
  pure $ promoteReady state {
    -- Stamp the completion with the generation the task was *activated* in, not the
    -- current one: a request that arrived while the task was already running was not
    -- taken into account by it, and must still be able to re-activate the key.
    completed = Map.insert key generation state.completed,
    -- Remove the key from 'accepted' now that it has actually finished: that map only
    -- dedupes a key while it is in flight (unsatisfied\/ready\/active), to protect against
    -- two concurrent requests within the same in-flight window both resolving to it.
    -- Re-activation by a later request is instead governed by the generation comparison
    -- in 'activation'.
    accepted = Map.delete key state.accepted,
    -- A later success must clear any earlier failure entry for the same key: otherwise a
    -- persistent scheduler that recompiles a previously-failed key successfully would keep
    -- reporting it as failed indefinitely, since 'failures' is never pruned anywhere else.
    failures = case result of
      TaskSuccess _ -> Map.delete key state.failures
      TaskFailed f -> Map.insert key f state.failures,
    unsatisfied = Map.map (fmap (Set.delete key)) state.unsatisfied,
    activeCount = state.activeCount - 1
  }
  where
    generation = Map.findWithDefault state.generation key state.accepted
