{-# LANGUAGE UndecidableInstances #-}

-- | Generic concurrent scheduler with inbox-based architecture.
--
-- External clients submit work via 'submitRequest'.  The scheduler loop
-- classifies requests into tasks (via 'classify'), respects
-- inter-task dependency ordering, and dispatches ready tasks up to a
-- configurable concurrency limit.
--
-- Tasks exist in two pools:
--
-- * /Active/ (unsatisfied or ready): participate in dependency
--   tracking and dispatch.
-- * /Pending/: invisible to 'awaitIdle'.  They are activated via 'addResolutions'
--   enabled pending tasks that now have resolution entries) or immediately
--   at insertion time if a resolution already exists.
--
-- Resolution entries (mapping pending keys to resolved keys, values, and
-- dependency sets) are stored in 'SchedulerState' and populated by the
-- build layer through the 'propagate' callback via 'addResolutions'.
--
-- On task completion, the 'propagate' callback lets the build layer apply
-- domain-specific effects (e.g. computing resolution maps from metadata
-- results and promoting compile tasks).
--
-- The key type parameter @key :: 'Phase' -> 'Type'@ is phase-indexed:
-- @key \''Pending@ identifies tasks in the pending pool (e.g. by source path),
-- while @key \''Resolved@ identifies active\/completed tasks (e.g. by module name).
-- Dependencies are always expressed in terms of @key \''Resolved@ in the
-- active pools.  Resolution entries carry dependencies as @key \''Pending@,
-- which are converted to @key \''Resolved@ during promotion.
-- The task value parameter @task@ is a plain type shared across both pools.
module Test.Scheduler.Concurrent where

import Control.Concurrent.Async (Async, async)
import Control.Concurrent.STM (
  STM,
  TQueue,
  TVar,
  atomically,
  check,
  isEmptyTQueue,
  modifyTVar',
  newTQueueIO,
  newTVarIO,
  readTQueue,
  readTVar,
  readTVarIO,
  stateTVar,
  writeTQueue,
  writeTVar,
  )
import Control.Concurrent.STM.TQueue (peekTQueue)
import Control.Exception (SomeAsyncException, SomeException, fromException, throwIO, try)
import Control.Monad (forever, void)
import Data.Foldable (foldr', traverse_)
import Data.Kind (Constraint, Type)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Text (Text)
import Data.Void (Void)
import System.Timeout (timeout)

-- | Phase of a task in the scheduler lifecycle.
--
-- * @'Pending@: task is in the pending pool, awaiting resolution.
-- * @'Resolved@: task has been resolved and is eligible for dispatch.
data Phase = Pending | Resolved

-- | Constraint alias requiring 'Ord' for both phases of a key.
type OrdKey :: (Phase -> Type) -> Constraint
type OrdKey key = (Ord (key 'Pending), Ord (key 'Resolved))

-- | Monotonically increasing version stamp, bumped once per external request
-- (see 'bumpGeneration', called from 'processEvent').
--
-- This is the scheduler's answer to \"as of when\": every activation and every
-- resolution entry carries the generation it was computed at, so a later request
-- can distinguish \"this key completed and its result is still current\" from
-- \"this key completed, but a newer staleness analysis supersedes that result\".
-- Without it, the permanently accumulating 'accepted'\/'completed'\/'resolutions'
-- sets can only express \"ever\" and \"never\", which is why a persistent scheduler
-- either re-ran everything on every request or went permanently inert after the
-- first completion.
newtype Generation =
  Generation Int
  deriving stock (Eq, Ord, Show)

-- | The generation of a scheduler state that has not yet seen a request.
--
-- Strictly smaller than every generation a task can be activated in, so a
-- resolution recorded before the first request never counts as up to date.
initialGeneration :: Generation
initialGeneration = Generation 0

nextGeneration :: Generation -> Generation
nextGeneration (Generation n) = Generation (n + 1)

-- | Result of executing a build task.
data TaskResult f =
  -- | The task succeeded, optionally carrying a result payload (e.g. the value produced by an evaluated
  -- expression\/main action).
  TaskSuccess (Maybe Text)
  |
  TaskFailed f
  deriving stock (Eq, Show)

-- | A build task with a key, dependencies, and a dispatchable value.
--
-- The key and value are indexed by the same phase @p@, but dependencies are
-- always expressed in @key \''Resolved@ — pending tasks depend on resolved
-- (active) tasks, never on other pending tasks.
data Task (key :: Phase -> Type) (p :: Phase) a =
  Task {
    key :: key p,
    -- | Dependencies, always expressed as resolved keys.
    deps :: Set (key 'Resolved),
    -- | Whether the task is eligible for promotion from the pending pool.
    -- 'insertPending' merges this flag with OR when a duplicate key is inserted.
    -- 'promoteEnabled' only promotes tasks where this is 'True'.
    -- For active (non-pending) tasks, this field is ignored.
    enabled :: Bool,
    value :: a
  }

deriving stock instance (Show (key p), Show (key 'Resolved), Show a) => Show (Task key p a)
deriving stock instance (Eq (key p), Eq (key 'Resolved), Eq a) => Eq (Task key p a)

-- | A resolution entry: what a pending key resolves to, and when that was computed.
--
-- The @generation@ stamp is what makes eligibility decisions version-based rather than
-- membership-based: an entry is only a reason to run the task if it is /newer/ than the
-- key's last completion (see 'activation').
data Resolution (key :: Phase -> Type) task =
  Resolution {
    key :: key 'Resolved,
    value :: task,
    -- | Module-level dependencies, still in pending form.
    deps :: Set (key 'Pending),
    -- | The generation in which this entry was computed.
    computedAt :: Generation
  }

deriving stock instance (Show (key 'Pending), Show (key 'Resolved), Show task) => Show (Resolution key task)
deriving stock instance (Eq (key 'Pending), Eq (key 'Resolved), Eq task) => Eq (Resolution key task)

-- | A scheduling decision, forwarded to 'Handlers.reportDecision' as it is recorded.
--
-- These are deliberately the /negative/ decisions as well as the positive ones: \"nothing was
-- compiled\" is indistinguishable from \"the work was silently dropped\" unless the reason for
-- not scheduling is observable.
data SchedulerDecision (key :: Phase -> Type) =
  -- | A new request was accepted, starting the given generation.
  DecisionGeneration Generation
  |
  -- | A pending key was resolved and activated in the given generation.
  DecisionActivated (key 'Resolved) Generation
  |
  -- | A pending key resolved to a key that is already in flight (activated in the given
  -- generation), so the duplicate submission was deduped instead of dispatched.
  DecisionDeduped (key 'Resolved) Generation
  |
  -- | A pending key resolved to a key whose completion (second generation) is at least as
  -- recent as its resolution (first generation), so it is already up to date.  The task stays
  -- in the pending pool so a future generation's resolution can still activate it.
  DecisionUpToDate (key 'Resolved) Generation Generation
  |
  -- | A resolution entry was recorded for a pending key in the given generation, replacing an
  -- entry from the generation in the last field (if any).
  DecisionResolution (key 'Pending) Generation (Maybe Generation)
  |
  -- | A resolved task completed; the generation is the one it was activated in.
  DecisionCompleted (key 'Resolved) Generation

deriving stock instance (Show (key 'Pending), Show (key 'Resolved)) => Show (SchedulerDecision key)
deriving stock instance (Eq (key 'Pending), Eq (key 'Resolved)) => Eq (SchedulerDecision key)

-- | Events processed by the scheduler loop.
data SchedulerEvent request (key :: Phase -> Type) f =
  -- | An external request to be classified into tasks.
  RequestEvent request
  |
  -- | A task completed with its result.
  CompletionEvent (key 'Resolved) (TaskResult f)

-- | Immutable configuration for the scheduler.
data SchedulerEnv request (key :: Phase -> Type) task f ext =
  SchedulerEnv {
    maxJobs :: Int,
    -- | Domain-specific handler callbacks.
    handlers :: Handlers request key task f ext,
    -- | Timeout for each task in seconds.
    taskTimeout :: Int,
    -- | Convert timeout/exception messages to the failure type.
    mkFailure :: String -> f,
    -- | When 'False', the scheduler stops dispatching new tasks after the first
    -- failure and drains remaining ready\/unsatisfied tasks without executing them.
    continueOnFailure :: Bool
  }

-- | Domain-specific handler callbacks for the scheduler.
--
-- These callbacks bridge the generic scheduler with the build system:
-- dispatching tasks to workers, classifying requests into tasks, and
-- applying domain-specific effects on task completion.
data Handlers request (key :: Phase -> Type) task f ext =
  Handlers {
    -- | Dispatch a resolved task to a worker.
    -- Receives the scheduler's domain-specific extension state and the full 'Task'
    -- so the handler can inspect metadata like 'enabled'.
    dispatch :: ext -> Task key 'Resolved task -> IO (TaskResult f),
    -- | Convert a request into active and pending task lists.
    classify :: request -> IO ([Task key 'Resolved task], [Task key 'Pending task]),
    -- | Apply domain-specific effects after a task completes.
    --
    -- Runs in the scheduler loop thread, so IO is safe but should not block
    -- for extended periods.
    propagate ::
      key 'Resolved ->
      TaskResult f ->
      SchedulerState key task f ext ->
      IO (SchedulerState key task f ext),
    -- | Observe a scheduler decision as it is recorded, in chronological order.
    --
    -- Called once per processed event (see 'flushTrace'), after that event's state transition has
    -- been committed. Domain layers use this to make the decision log observable from the outside
    -- (e.g. appending to an 'Data.IORef.IORef' in tests, or forwarding to an instrumentation
    -- channel in production); a handler that does nothing (@\\ _ -> pure ()@) disables tracing
    -- entirely at negligible cost.
    reportDecision :: SchedulerDecision key -> IO ()
  }

-- | All mutable scheduler state.
--
-- The @ext@ parameter allows the build layer to store domain-specific data
-- (such as accumulated resolution maps) alongside the scheduler's own state.
--
-- The @key@ parameter is phase-indexed: @key \''Pending@ for the pending pool,
-- @key \''Resolved@ for active\/completed tasks.  Dependencies are always
-- expressed in @key \''Resolved@.
data SchedulerState (key :: Phase -> Type) task f ext =
  SchedulerState {
    -- | Tasks waiting on unmet dependencies.
    unsatisfied :: Map (key 'Resolved) (Task key 'Resolved task, Set (key 'Resolved)),
    -- | Tasks ready for dispatch.
    ready :: [Task key 'Resolved task],
    -- | Pre-resolution tasks awaiting promotion. Excluded from 'awaitIdle'.
    pending :: Map (key 'Pending) (Task key 'Pending task),
    -- | Keys that have finished, mapped to the generation their run was /activated/ in.
    --
    -- Deliberately the activation generation rather than the completion generation: a task
    -- that was already running when a new request arrived did not take that request's newer
    -- inputs into account, so it must not be credited with satisfying it.
    completed :: Map (key 'Resolved) Generation,
    -- | All resolved keys that are currently in flight (unsatisfied\/ready\/active), mapped to
    -- the generation they were activated in.  Used for idempotent enqueue.
    accepted :: Map (key 'Resolved) Generation,
    activeCount :: Int,
    failures :: Map (key 'Resolved) f,
    -- | Resolution map: converts pending keys to resolved keys, values, and
    -- pending dep sets.  Populated by the build layer via 'addResolutions'
    -- after metadata completes.
    resolutions :: Map (key 'Pending) (Resolution key task),
    -- | The generation of the most recent request (see 'Generation').
    generation :: Generation,
    -- | Decisions recorded since the last 'flushTrace', in reverse order.
    --
    -- Purely a transient buffer: 'flushTrace' drains it and forwards each entry, in chronological
    -- order, to 'Handlers.reportDecision' after every processed event, so it is always empty when a new
    -- event's state transition begins.
    trace :: [SchedulerDecision key],
    -- | Domain-specific state threaded through 'propagate'.
    ext :: ext
  }

-- | The set of keys that have completed, discarding their generation stamps.
completedKeys :: SchedulerState key task f ext -> Set (key 'Resolved)
completedKeys state = Map.keysSet state.completed

-- | The keys whose completion currently holds, i.e. that a dependent may treat as satisfied.
--
-- A completed key that has since been re-activated does /not/ qualify: its recorded completion
-- describes a run whose inputs a later request already found stale, so a dependent scheduled now
-- must wait for the new run instead of proceeding against the old artifacts.  Checking only
-- 'completed' would let a dependent overtake its dependency across requests, since completions
-- are never cleared in a scheduler that outlives a single request.
satisfiedKeys :: Ord (key 'Resolved) => SchedulerState key task f ext -> Set (key 'Resolved)
satisfiedKeys state = Set.difference (Map.keysSet state.completed) (Map.keysSet state.accepted)

-- | Start a new generation.  Called once per external request.
bumpGeneration ::
  SchedulerState key task f ext ->
  SchedulerState key task f ext
bumpGeneration state =
  traceDecision (DecisionGeneration generation) state {generation}
  where
    generation = nextGeneration state.generation

-- | Buffer a decision for the next 'flushTrace'.
traceDecision ::
  SchedulerDecision key ->
  SchedulerState key task f ext ->
  SchedulerState key task f ext
traceDecision decision state =
  state {trace = decision : state.trace}

-- | Whether a resolution should activate its task.
data Activation =
  -- | The key is not in flight and no completion supersedes the resolution.
  ActivateNow
  |
  -- | The key is already in flight, activated in this generation.
  SkipInFlight Generation
  |
  -- | The key completed in a generation at least as recent as the resolution.
  SkipUpToDate Generation

-- | Decide whether a resolution entry justifies (re-)activating its resolved key.
--
-- The in-flight check comes first and is pure deduplication: two requests within the same
-- in-flight window must not both dispatch the key.
--
-- The up-to-date check is the version comparison: the entry is only a reason to run if the
-- staleness analysis that produced it is /newer/ than the last run of the key.
activation ::
  Ord (key 'Resolved) =>
  Resolution key task ->
  SchedulerState key task f ext ->
  Activation
activation resolution state
  | Just g <- Map.lookup resolution.key state.accepted = SkipInFlight g
  | Just g <- Map.lookup resolution.key state.completed
  , resolution.computedAt <= g
  = SkipUpToDate g
  | otherwise = ActivateNow

-- | Mutable scheduler state, shared across worker threads and external callers.
data SchedulerResources request (key :: Phase -> Type) task f ext =
  SchedulerResources {
    -- | Event queue for inbox requests and task completions.
    events :: TQueue (SchedulerEvent request key f),
    state :: TVar (SchedulerState key task f ext)
  }

-- | Move all unsatisfied tasks with empty dep sets to the ready list.
promoteReady ::
  SchedulerState key task f ext ->
  SchedulerState key task f ext
promoteReady state =
  state {unsatisfied, ready = state.ready ++ (fst <$> Map.elems readyNow)}
  where
    (readyNow, unsatisfied) = Map.partition (null . snd) state.unsatisfied

-- | Classify a single active task: skip if already enqueued, otherwise insert into 'unsatisfied'
-- and promote if ready.
--
-- @batch@ is the set of keys enqueued together with this task.  Those count as unmet
-- dependencies even if they carry a completion record, because they are about to be activated
-- again; without this, whether a dependent waits for its dependency would hinge on the order in
-- which the batch happens to be folded over.
classifyTaskIn ::
  Ord (key 'Resolved) =>
  Set (key 'Resolved) ->
  Task key 'Resolved task ->
  SchedulerState key task f ext ->
  SchedulerState key task f ext
classifyTaskIn batch task state =
  if Map.member task.key state.accepted
  then state
  else promoteReady (traceDecision (DecisionActivated task.key state.generation) state {
    unsatisfied = Map.insert task.key (task, unmet) state.unsatisfied,
    accepted = Map.insert task.key state.generation state.accepted
  })
  where
    unmet = Set.difference task.deps (Set.difference (satisfiedKeys state) batch)

-- | 'classifyTaskIn' for a task that is not part of a batch.
classifyTask ::
  Ord (key 'Resolved) =>
  Task key 'Resolved task ->
  SchedulerState key task f ext ->
  SchedulerState key task f ext
classifyTask =
  classifyTaskIn Set.empty

-- | Record a task result: update completed set, decrement active count,
-- remove key from dep sets, promote newly ready tasks.
recordResult ::
  Ord (key 'Resolved) =>
  key 'Resolved ->
  TaskResult f ->
  SchedulerState key task f ext ->
  SchedulerState key task f ext
recordResult key result =
  promoteReady . record
  where
    record state =
      traceDecision (DecisionCompleted key generation) state {
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

-- | Insert a task into the pending pool, or resolve it immediately.
--
-- * If the resolution map already contains an entry for this key and the task
--   is enabled, resolve immediately into unsatisfied (via 'resolveTask').
-- * If already pending, merge the @enabled@ flag with OR.
-- * Otherwise, insert as a new pending task.
insertPending ::
  OrdKey key =>
  Task key 'Pending task ->
  SchedulerState key task f ext ->
  SchedulerState key task f ext
insertPending task state
  | task.enabled
  , Map.member task.key state.resolutions =
    let inserted = state {pending = Map.insertWith mergeTask task.key task state.pending}
    in case resolveTask task.key inserted of
      Nothing -> inserted
      Just (s', pendingDeps) -> promoteReady (go pendingDeps s')
  | otherwise = state {pending = Map.insertWith mergeTask task.key task state.pending}
  where
    mergeTask new old = old {enabled = old.enabled || new.enabled}
    go [] s = s
    go (k : ks) s =
      case resolveTask k s of
        Nothing -> go ks s
        Just (s', more) -> go (more ++ ks) s'

-- | Promote keys from the pending pool to unsatisfied, transitively through dependencies.
--
-- For each pending key, if the pending pool contains the task and 'resolutions' provides
-- a resolved key, value, and pending dependency set, the task is moved to unsatisfied with
-- the resolved identity.  Pending deps are converted to resolved keys via 'resolutions'
-- and placed in the unsatisfied dep set.  Any pending deps that are still in the pending pool
-- are added to the work list, ensuring transitive activation.
promote ::
  OrdKey key =>
  Set (key 'Pending) ->
  SchedulerState key task f ext ->
  SchedulerState key task f ext
promote keys =
  promoteReady . go (Set.toList keys)
  where
    go [] s = s
    go (k : ks) s =
      case resolveTask k s of
        Nothing -> go ks s
        Just (s', pendingDeps) -> go (pendingDeps ++ ks) s'

-- | Try to resolve a single pending task using 'resolutions' from state.
--
-- Returns 'Nothing' if the key is not pending or has no resolution entry.
-- On success, returns the updated state and a list of the task's own
-- dependencies that are still in the pending pool (for transitive promotion).
--
-- Whether the resolution actually activates the task is decided by 'activation'.  Note the
-- asymmetry between the two negative outcomes: a deduped key is removed from the pending pool
-- (it /is/ being built, right now), whereas an up-to-date key stays pending, so that a later
-- generation which finds it stale again can still activate it.
resolveTask ::
  OrdKey key =>
  key 'Pending ->
  SchedulerState key task f ext ->
  Maybe (SchedulerState key task f ext, [key 'Pending])
resolveTask k s = do
  task <- Map.lookup k s.pending
  resolution <- Map.lookup k s.resolutions
  let
    -- Convert pending deps to resolved keys via 'resolutions'.
    -- Deps whose resolution is not yet available are silently dropped;
    -- in practice this doesn't happen because metadata completes in
    -- dependency order.
    resolvedDeps = Set.fromList
      [r.key | pk <- Set.toList resolution.deps, Just r <- [Map.lookup pk s.resolutions]]
    allDeps = Set.union task.deps resolvedDeps
    resolvedTask =
      Task {key = resolution.key, deps = allDeps, enabled = task.enabled, value = resolution.value}
    withoutPending = s {pending = Map.delete k s.pending}
    activated =
      traceDecision (DecisionActivated resolution.key s.generation) withoutPending {
        unsatisfied =
          Map.insert resolution.key (resolvedTask, Set.difference allDeps (satisfiedKeys s)) s.unsatisfied,
        accepted = Map.insert resolution.key s.generation s.accepted
      }
    transitive s' = [pk | pk <- Set.toList resolution.deps, Map.member pk s'.pending]
  pure case activation resolution s of
    ActivateNow -> (activated, transitive activated)
    SkipInFlight g -> (traceDecision (DecisionDeduped resolution.key g) withoutPending, transitive withoutPending)
    SkipUpToDate g -> (traceDecision (DecisionUpToDate resolution.key resolution.computedAt g) s, [])

-- | Promote all pending tasks that have @enabled = True@ and have an entry
-- in 'resolutions'.
--
-- Only tasks where @enabled = True@ and a matching resolution exists are promoted.
-- Promotion is transitive through dependencies, so tasks that are depended upon
-- by promoted ones also get promoted (if they have resolutions).
promoteEnabled ::
  OrdKey key =>
  SchedulerState key task f ext ->
  SchedulerState key task f ext
promoteEnabled state =
  promote enabledKeys state
  where
    enabledKeys = Set.fromList
      [
        k
        | (k, task) <- Map.toList state.pending
        , task.enabled
        , Map.member k state.resolutions
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
  Map (key 'Pending) (key 'Resolved, task, Set (key 'Pending)) ->
  SchedulerState key task f ext ->
  SchedulerState key task f ext
addResolutions newResolutions state =
  promoteEnabled traced {resolutions = Map.union stamped state.resolutions}
  where
    -- 'Map.union' is left-biased, so the freshly stamped entries replace older ones.
    stamped = Map.map stamp newResolutions
    stamp (key, value, deps) = Resolution {key, value, deps, computedAt = state.generation}
    traced = foldr' recordDecision state (Map.keys newResolutions)
    recordDecision k =
      traceDecision (DecisionResolution k state.generation ((.computedAt) <$> Map.lookup k state.resolutions))

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
    Left (exc :: SomeException) ->
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
--
-- The caller is responsible for incrementing 'activeCount' before calling
-- this function (see 'fillSlots').
startTask ::
  SchedulerEnv request key task f ext ->
  SchedulerResources request key task f ext ->
  ext ->
  Task key 'Resolved task ->
  IO ()
startTask env resources ext task =
  void (async (executeTask env resources ext task))

-- | Enqueue pre-built active tasks directly. Skips tasks whose keys are already known.
--
-- This is intended for use by dispatch callbacks that generate follow-up tasks
-- (e.g. compile tasks after metadata completion). For external callers, prefer
-- 'submitRequest'.
enqueueTasks ::
  Ord (key 'Resolved) =>
  [Task key 'Resolved task] ->
  SchedulerState key task f ext ->
  SchedulerState key task f ext
enqueueTasks tasks =
  flip (foldr' (classifyTaskIn batch)) tasks
  where
    batch = Set.fromList [task.key | task <- tasks]

-- | Insert tasks into the pending pool, merging the @enabled@ flag for duplicate keys.
enqueuePending ::
  OrdKey key =>
  [Task key 'Pending task] ->
  SchedulerState key task f ext ->
  SchedulerState key task f ext
enqueuePending =
  flip (foldr' insertPending)

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
    atomically do
      modifyTVar' resources.state (enqueuePending pendingTasks . enqueueTasks activeTasks . bumpGeneration)
    flushTrace env.handlers resources.state
  CompletionEvent key result -> do
    propagated <- env.handlers.propagate key result =<< readTVarIO resources.state
    atomically do
      writeTVar resources.state (recordResult key result propagated)
    flushTrace env.handlers resources.state

-- | Drain decisions buffered since the last flush and forward each, in chronological order, to
-- the handlers' 'reportDecision' callback.
--
-- Called once per processed event, right after its state transition has been committed (see
-- 'processEvent'), so the buffer is always empty when a new event's transition begins.
flushTrace ::
  Handlers request key task f ext ->
  TVar (SchedulerState key task f ext) ->
  IO ()
flushTrace handlers stateVar = do
  decisions <- atomically (stateTVar stateVar \ s -> (reverse s.trace, clearTrace s))
  traverse_ handlers.reportDecision decisions
  where
    clearTrace :: SchedulerState key task f ext -> SchedulerState key task f ext
    clearTrace s = s {trace = []}

-- | Take ready tasks from the pool up to the job limit and start them.
--
-- Atomically moves tasks from ready to active (incrementing 'activeCount')
-- so that 'awaitIdle' cannot observe a transient state where ready is
-- empty but 'activeCount' has not yet been bumped.
fillSlots ::
  SchedulerEnv request key task f ext ->
  SchedulerResources request key task f ext ->
  IO ()
fillSlots env resources =
  traverse_ (uncurry (startTask env resources)) =<< atomically (stateTVar resources.state takeReady)
  where
    takeReady state
      | not env.continueOnFailure, not (Map.null state.failures) =
        ([], state {ready = [], unsatisfied = Map.empty})
      | otherwise =
        let
          available = env.maxJobs - state.activeCount
          (toDispatch, keep) = splitAt available state.ready
        in (map (state.ext,) toDispatch, state {ready = keep, activeCount = state.activeCount + length toDispatch})

-- | Main scheduler loop. Reads one event at a time, processes it, dispatches ready tasks,
-- repeats. Runs indefinitely, blocking on the event queue when idle.
--
-- Leaves the processed task in the queue while processing to avoid race conditions.
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
      void $ readTQueue resources.events

-- API -------------------------------------------------------------------

-- | Start the scheduler loop in a background thread.
--
-- The loop runs indefinitely, blocking on the event queue when idle.
runScheduler ::
  OrdKey key =>
  SchedulerEnv request key task f ext ->
  SchedulerResources request key task f ext ->
  IO (Async Void)
runScheduler env resources =
  async (schedulerLoop env resources)

-- | Create a fresh scheduler state.
--
-- Use this when the 'SchedulerEnv' callbacks need access to the scheduler state
-- (e.g. dispatch callbacks that submit follow-up requests to the inbox).
-- After creating the env with references to this state, call 'runScheduler'.
newSchedulerState :: ext -> IO (SchedulerResources request key task f ext)
newSchedulerState initialExt = do
  events <- newTQueueIO
  state <- newTVarIO SchedulerState {
    unsatisfied = Map.empty,
    ready = [],
    pending = Map.empty,
    completed = Map.empty,
    accepted = Map.empty,
    activeCount = 0,
    failures = Map.empty,
    resolutions = Map.empty,
    generation = initialGeneration,
    trace = [],
    ext = initialExt
  }
  pure SchedulerResources {events, state}

-- | Submit a request to the scheduler's event queue.
submitRequest :: SchedulerResources request key task f ext -> request -> IO ()
submitRequest resources request =
  atomically (writeTQueue resources.events (RequestEvent request))

-- | Block until the scheduler is idle: no active tasks, no ready tasks, no unsatisfied tasks,
-- and no pending events in the queue.
--
-- Pending tasks are excluded — they are not considered active work.
--
-- This is the primary termination criterion for tests and single-shot builds.
awaitIdle :: SchedulerResources request key task f ext -> STM ()
awaitIdle resources = do
  empty <- isEmptyTQueue resources.events
  check empty
  state <- readTVar resources.state
  check (state.activeCount == 0 && null state.ready && Map.null state.unsatisfied)

-- | Structural invariants that should hold for any 'SchedulerState' once the scheduler has gone
-- idle (see 'awaitIdle'), independent of any domain-specific interpretation of @key@\/@task@\/@ext@.
--
-- Intended to be checked once per external request, right after it drains, so a violation is
-- reported immediately at the point it was introduced instead of manifesting later as a subtly
-- stale or inconsistent result. Returns a human-readable description of every violation found;
-- the empty list means all checked invariants hold.
--
-- This deliberately does not attempt to check anything that isn't decidable from the idle state
-- alone (e.g. it cannot tell whether a 'failures' entry is stale relative to a completion that
-- superseded it -- 'completed' only stores a generation stamp, not the outcome it stamps).
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

    -- Every key with a failure entry must also have a completion entry, since both are only ever
    -- written together by 'recordResult'.
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
