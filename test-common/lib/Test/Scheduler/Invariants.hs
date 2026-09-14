module Test.Scheduler.Invariants where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Test.Data.Scheduler (OrdKey, Phase (..), SchedulerState (..), Task (..))

-- | Structural invariants that should hold for any 'SchedulerState' once the scheduler has gone idle.
--
-- Intended to be checked once per external request, right after it drains, so a violation is reported immediately at
-- the point it was introduced instead of manifesting later as a subtly stale or inconsistent result. Returns a
-- human-readable description of every violation found.
-- The empty list means all checked invariants hold.
schedulerInvariantViolations ::
  OrdKey key =>
  Show (key 'Resolved) =>
  SchedulerState key task f ext ->
  [String]
schedulerInvariantViolations state =
  acceptedEmptyWhenIdle ++ failuresSubsetOfCompleted ++ activeCountNonNegative ++ readyDisjointFromUnsatisfied
  where
    acceptedEmptyWhenIdle :: [String]
    acceptedEmptyWhenIdle
      | Map.null state.accepted = []
      | otherwise = ["'accepted' is non-empty at idle: " ++ show (Map.keys state.accepted)]

    -- Every key with a failure entry must also have a completion entry, since both are only ever written together.
    failuresSubsetOfCompleted :: [String]
    failuresSubsetOfCompleted
      | Set.null stale = []
      | otherwise = ["'failures' contains keys absent from 'completed': " ++ show (Set.toList stale)]
      where
        stale = Map.keysSet state.failures `Set.difference` Map.keysSet state.completed

    activeCountNonNegative :: [String]
    activeCountNonNegative
      | state.activeCount >= 0 = []
      | otherwise = ["negative activeCount: " ++ show state.activeCount]

    -- A key cannot simultaneously be ready for dispatch and waiting on unmet dependencies.
    readyDisjointFromUnsatisfied :: [String]
    readyDisjointFromUnsatisfied
      | Set.null dup = []
      | otherwise = ["keys present in both 'ready' and 'unsatisfied': " ++ show (Set.toList dup)]
      where
        dup = Set.fromList [task.key | task <- state.ready] `Set.intersection` Map.keysSet state.unsatisfied
