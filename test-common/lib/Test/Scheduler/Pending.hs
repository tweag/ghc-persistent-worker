-- | The pending task pool and its promotion to active tasks via resolution entries.
module Test.Scheduler.Pending where

import Control.Monad (foldM)
import Data.Foldable (foldrM, traverse_)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Set as Set
import Data.Set (Set)
import Test.Data.Scheduler (
  Activation (..),
  OrdKey,
  Phase (..),
  Resolution (..),
  SchedulerDecision (..),
  SchedulerState (..),
  Task (..),
  )
import Test.Scheduler.Decision (Traced, traceDecision)
import Test.Scheduler.State (activation, promoteReady, satisfiedKeys)

-- | Update the state with a pending task.
--
-- - If the resolution map already contains an entry for this key and the task is enabled, resolve it immediately as
--   'unsatisfied'.
-- - If already pending, merge the @enabled@ flag with OR.
-- - Otherwise, insert as a new pending task.
insertPending ::
  OrdKey key =>
  Task key 'Pending task ->
  SchedulerState key task f ext ->
  Traced key (SchedulerState key task f ext)
insertPending task state =
  promoteIfResolved inserted
  where
    promoteIfResolved =
      if resolved
      then promote (Set.singleton task.key)
      else pure

    resolved = task.enabled && Map.member task.key state.resolutions
  
    inserted = state {pending = Map.insertWith mergeTask task.key task state.pending}

    mergeTask new old = old {enabled = old.enabled || new.enabled}

-- | Insert tasks into the pending pool.
enqueuePending ::
  OrdKey key =>
  [Task key 'Pending task] ->
  SchedulerState key task f ext ->
  Traced key (SchedulerState key task f ext)
enqueuePending tasks state =
  foldrM insertPending state tasks

-- | The resolved keys that will actually be (re-)activated by promoting the given closure of
-- pending keys, per 'activation'.
--
-- Computed up front, from the state as it stood before any of the closure's keys were
-- processed, so that it does not depend on the (arbitrary) order 'promote' visits them in --
-- see 'promote' for why this matters.
activatingKeys ::
  OrdKey key =>
  Set (key 'Pending) ->
  SchedulerState key task f ext ->
  Set (key 'Resolved)
activatingKeys closure state =
  Set.fromList [
    resolution.key
    | key <- Set.toList closure
    , Map.member key state.pending
    , Just resolution <- [Map.lookup key state.resolutions]
    , ActivateNow <- [activation resolution state]
  ]

-- | Compute the full transitive closure of pending keys reachable from the given seeds via
-- resolution dependency sets, restricted to keys currently in the pending pool.
--
-- Computed as a single up-front pass over the (immutable, for the duration of one 'promote'
-- call) resolutions\/pending maps, rather than discovered incrementally while resolving keys
-- one at a time -- see 'promote' for why this matters.
pendingClosure ::
  OrdKey key =>
  Set (key 'Pending) ->
  SchedulerState key task f ext ->
  Set (key 'Pending)
pendingClosure seeds state =
  explore seeds (Set.toList seeds)
  where
    explore visited = \case
      []  -> visited
      (key : ks) ->
        explore (Set.union visited new) (Set.toList new ++ ks)
        where
          new = Set.filter isPending (Set.difference (resolutionDeps key) visited)

    resolutionDeps key = maybe Set.empty (.deps) (Map.lookup key state.resolutions)

    isPending key = Map.member key state.pending

-- | Promote keys from the pending pool to unsatisfied, transitively through dependencies.
--
-- For each pending key, if the pending pool contains the task and 'resolutions' provides a resolved key, value, and
-- pending dependency set, the task is moved to unsatisfied with the resolved identity.  Pending deps are converted to
-- resolved keys via 'resolutions' and placed in the unsatisfied dep set.
-- Any pending deps that are still in the pending pool are promoted as well, ensuring transitive activation.
--
-- The full closure of keys reachable from @keys@ ('pendingClosure') and the set of resolved
-- keys that will actually activate ('activatingKeys') are both computed up front, before any
-- individual key is resolved, so that the unmet-dependency computation in 'resolveTask' does
-- not depend on the order the closure happens to be visited in -- see 'resolveTask'\'s @batch@
-- parameter.
promote ::
  OrdKey key =>
  Set (key 'Pending) ->
  SchedulerState key task f ext ->
  Traced key (SchedulerState key task f ext)
promote keys state =
  promoteReady <$> foldM resolveIfPossible state (Set.toList closure)
  where
    resolveIfPossible z key = fromMaybe (pure z) (resolveTask activatingTogether key z)

    activatingTogether = activatingKeys closure state

    closure = pendingClosure keys state

resolveWith ::
  OrdKey key =>
  Set (key 'Resolved) ->
  key 'Pending ->
  SchedulerState key task f ext ->
  Task key 'Pending task ->
  Resolution key ->
  Traced key (SchedulerState key task f ext)
resolveWith batch key state task resolution =
  case activation resolution state of
    ActivateNow -> activate
    SkipInFlight g -> withoutPending <$ traceDecision (DecisionDeduped resolution.key g)
    SkipUpToDate g -> state <$ traceDecision (DecisionUpToDate resolution.key resolution.computedAt g)
  where
    activate = do
      traceDecision (DecisionActivated resolution.key state.generation)
      pure withoutPending {
        unsatisfied = Map.insert resolution.key (resolvedTask, unmetDeps) state.unsatisfied,
        accepted = Map.insert resolution.key state.generation state.accepted
      }

    resolvedTask = Task {key = resolution.key, deps = allDeps, enabled = task.enabled, value = task.value}

    unmetDeps = Set.difference allDeps satisfiedOutsideBatch

    allDeps = Set.union task.deps resolvedPendingDeps

    -- Deps whose resolution is not yet available are silently dropped; in practice this doesn't happen because
    -- metadata completes in dependency order.
    resolvedPendingDeps = Set.fromList (mapMaybe resolvedKey (Set.toList resolution.deps))

    resolvedKey pk = (.key) <$> Map.lookup pk state.resolutions

    satisfiedOutsideBatch = Set.difference (satisfiedKeys state) batch

    withoutPending = state {pending = Map.delete key state.pending}


-- | Try to resolve a single pending task using 'resolutions' from state.
--
-- Returns 'Nothing' if the key is not pending or has no resolution entry.
--
-- @batch@ is the set of resolved keys being activated together with this one in the same
-- 'promote' call (see 'activatingKeys'). Those count as unmet dependencies even if they carry a
-- stale completion record, because they are about to be (re-)activated: without this, whether a
-- dependent (e.g. a module compile task depending on another module in the same unit's metadata
-- resolution) waits for its dependency would hinge on the arbitrary order in which 'promote'
-- happens to visit the two keys, since a completion from an earlier generation would otherwise
-- be misread as still satisfying this generation's dependency purely because it hadn't been
-- reprocessed yet. Mirrors 'Test.Scheduler.Active.classifyTaskIn'\'s @batch@ parameter for the same reason.
--
-- Whether the resolution actually activates the task is decided by 'activation'.  Note the
-- asymmetry between the two negative outcomes: a deduped key is removed from the pending pool
-- (it /is/ being built, right now), whereas an up-to-date key stays pending, so that a later
-- generation which finds it stale again can still activate it.
resolveTask ::
  OrdKey key =>
  Set (key 'Resolved) ->
  key 'Pending ->
  SchedulerState key task f ext ->
  Maybe (Traced key (SchedulerState key task f ext))
resolveTask batch key state =
  resolveWith batch key state <$> Map.lookup key state.pending <*> Map.lookup key state.resolutions

-- | Promote all pending tasks that have @enabled = True@ and have an entry
-- in 'resolutions'.
--
-- Only tasks where @enabled = True@ and a matching resolution exists are promoted.
-- Promotion is transitive through dependencies, so tasks that are depended upon
-- by promoted ones also get promoted (if they have resolutions).
promoteEnabled ::
  OrdKey key =>
  SchedulerState key task f ext ->
  Traced key (SchedulerState key task f ext)
promoteEnabled state =
  promote enabledKeys state
  where
    enabledKeys = Set.fromList
      [
        key
        | (key, task) <- Map.toList state.pending
        , task.enabled
        , Map.member key state.resolutions
      ]

-- | Merge new resolution entries into state and promote eligible pending tasks.
--
-- This is the primary interface for the build layer to supply resolution data
-- after metadata completes.  Entries are stamped with the current generation here, so the
-- build layer does not need to know about versioning; "this key is stale" is expressed simply
-- by supplying an entry for it in the current generation.
--
-- After merging, all enabled pending tasks that now have current resolutions are promoted
-- (transitively through deps).
addResolutions ::
  OrdKey key =>
  Map (key 'Pending) (key 'Resolved, Set (key 'Pending)) ->
  SchedulerState key task f ext ->
  Traced key (SchedulerState key task f ext)
addResolutions newResolutions state = do
  traverse_ recordDecision (Map.keys newResolutions)
  promoteEnabled state {resolutions = Map.union stamped state.resolutions}
  where
    -- 'Map.union' is left-biased, so the freshly stamped entries replace older ones.
    stamped = Map.map stamp newResolutions
    stamp (key, deps) = Resolution {key, deps, computedAt = state.generation}
    recordDecision key =
      traceDecision (DecisionResolution key state.generation ((.computedAt) <$> Map.lookup key state.resolutions))
