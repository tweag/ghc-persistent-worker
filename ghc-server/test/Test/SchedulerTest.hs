-- | Unit tests for 'GhcServer.Scheduler' pure functions.
module Test.SchedulerTest where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Set (Set)
import GhcServer.Scheduler (
  Generation,
  Phase (..),
  Resolution (..),
  SchedulerState (..),
  Task (..),
  TaskResult (..),
  addResolutions,
  bumpGeneration,
  classifyTask,
  initialGeneration,
  insertPending,
  nextGeneration,
  promote,
  promoteEnabled,
  recordResult,
  )
import Hedgehog (TestT, property, test, withTests, (===))
import Test.Tasty (DependencyType (..), TestName, TestTree, dependentTestGroup)
import Test.Tasty.Hedgehog (testProperty)

-- ---------------------------------------------------------------------------
-- Test key and task types using Int-based keys
-- ---------------------------------------------------------------------------

-- | Phase-indexed key for testing.
-- Both phases use 'Int', which mirrors the case where pending and resolved
-- keys have the same underlying type.
data TestKey (p :: Phase) where
  TK :: Int -> TestKey p

deriving stock instance Show (TestKey p)
deriving stock instance Eq (TestKey p)
deriving stock instance Ord (TestKey p)

-- | Simple task value type for testing.
data TestTask =
  PendingVal Int
  |
  ResolvedVal Int
  deriving stock (Show, Eq)

type Key = Int
type State = SchedulerState TestKey TestTask String ()

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

unitTest :: TestName -> TestT IO () -> TestTree
unitTest desc t =
  testProperty desc (withTests 1 (property (test t)))

-- | The generation the pure tests operate in.
--
-- Deliberately one past 'initialGeneration', so that resolutions stamped with it are newer
-- than completions stamped with 'initialGeneration' -- mirroring the production shape, where a
-- request's resolutions are always computed after the completions of earlier requests.
testGeneration :: Generation
testGeneration = nextGeneration initialGeneration

emptyState :: State
emptyState =
  SchedulerState {
    unsatisfied = Map.empty,
    ready = [],
    pending = Map.empty,
    completed = Map.empty,
    accepted = Map.empty,
    activeCount = 0,
    failures = Map.empty,
    resolutions = Map.empty,
    generation = testGeneration,
    trace = [],
    ext = ()
  }

pendingTask :: Key -> Set Key -> Bool -> Int -> Task TestKey 'Pending TestTask
pendingTask k deps isEnabled val =
  Task {key = TK k, deps = Set.map TK deps, enabled = isEnabled, value = PendingVal val}

pendingKeys :: State -> Set Key
pendingKeys = Set.map (\(TK k) -> k) . Map.keysSet . (.pending)

unsatisfiedKeys :: State -> Set Key
unsatisfiedKeys = Set.map (\(TK k) -> k) . Map.keysSet . (.unsatisfied)

readyKeys :: State -> Set Key
readyKeys = Set.fromList . map (\(TK k) -> k) . map (.key) . (.ready)

-- | Build resolution map from raw (key, (resolvedValue, pendingDeps)) entries, stamped with
-- 'testGeneration'.
mkResolutions :: [(Key, (TestTask, Set Key))] -> Map.Map (TestKey 'Pending) (Resolution TestKey TestTask)
mkResolutions =
  Map.fromList . map \(k, (v, deps)) ->
    (TK k, Resolution {key = TK k, value = v, deps = Set.map TK deps, computedAt = testGeneration})

-- | Raw resolution input in the shape 'addResolutions' accepts (it stamps the generation
-- itself).
rawResolutions ::
  [(Key, (TestTask, Set Key))] ->
  Map.Map (TestKey 'Pending) (TestKey 'Resolved, TestTask, Set (TestKey 'Pending))
rawResolutions =
  Map.fromList . map \(k, (v, deps)) -> (TK k, (TK k, v, Set.map TK deps))


-- ---------------------------------------------------------------------------
-- Promote test spec
-- ---------------------------------------------------------------------------

-- | Specification for a @promote@ or @promoteEnabled@ test.
data PromoteSpec =
  PromoteSpec {
    pending_ :: [(Key, Set Key, Bool, Int)]
    ,
    completed_ :: Set Key
    ,
    accepted_ :: Set Key
    ,
    resolutions_ :: [(Key, (TestTask, Set Key))]
    ,
    -- | Keys to promote (used only by @promote@, ignored by @promoteEnabled@).
    promoteKeys :: Set Key
    ,
    expectPending :: Set Key
    ,
    expectUnsatisfied :: Set Key
    ,
    expectReady :: Set Key
  }

defaultPromoteSpec :: PromoteSpec
defaultPromoteSpec =
  PromoteSpec {
    pending_ = [],
    completed_ = Set.empty,
    accepted_ = Set.empty,
    resolutions_ = [],
    promoteKeys = Set.empty,
    expectPending = Set.empty,
    expectUnsatisfied = Set.empty,
    expectReady = Set.empty
  }

specState :: PromoteSpec -> State
specState spec =
  emptyState {
    pending = Map.fromList [(TK k, pendingTask k deps en val) | (k, deps, en, val) <- spec.pending_],
    completed = Map.fromSet (const initialGeneration) (Set.map TK spec.completed_),
    accepted = Map.fromSet (const testGeneration) (Set.map TK spec.accepted_),
    resolutions = mkResolutions spec.resolutions_
  }

runPromote :: PromoteSpec -> TestT IO ()
runPromote spec = do
  let result = promote (Set.map TK spec.promoteKeys) (specState spec)
  spec.expectPending === pendingKeys result
  spec.expectUnsatisfied === unsatisfiedKeys result
  spec.expectReady === readyKeys result

runPromoteEnabled :: PromoteSpec -> TestT IO ()
runPromoteEnabled spec = do
  let result = promoteEnabled (specState spec)
  spec.expectPending === pendingKeys result
  spec.expectUnsatisfied === unsatisfiedKeys result
  spec.expectReady === readyKeys result

-- ---------------------------------------------------------------------------
-- Tests for 'promote'
-- ---------------------------------------------------------------------------

test_promoteSingleNoDeps :: TestTree
test_promoteSingleNoDeps =
  unitTest "single task with no deps becomes ready" do
    runPromote defaultPromoteSpec {
      pending_ = [(1, Set.empty, False, 10)],
      resolutions_ = [(1, (ResolvedVal 10, Set.empty))],
      promoteKeys = Set.singleton 1,
      expectReady = Set.singleton 1
    }

test_promoteWithUnmetDep :: TestTree
test_promoteWithUnmetDep =
  unitTest "task with unmet dep goes to unsatisfied" do
    runPromote defaultPromoteSpec {
      pending_ = [(1, Set.singleton 99, False, 10)],
      resolutions_ = [(1, (ResolvedVal 10, Set.empty))],
      promoteKeys = Set.singleton 1,
      expectUnsatisfied = Set.singleton 1
    }

test_promoteDepAlreadyCompleted :: TestTree
test_promoteDepAlreadyCompleted =
  unitTest "task whose dep is completed becomes ready" do
    runPromote defaultPromoteSpec {
      pending_ = [(1, Set.singleton 2, False, 10)],
      completed_ = Set.singleton 2,
      resolutions_ = [(1, (ResolvedVal 10, Set.empty))],
      promoteKeys = Set.singleton 1,
      expectReady = Set.singleton 1
    }

test_promoteExtraDeps :: TestTree
test_promoteExtraDeps =
  unitTest "extra deps from resolution map are added" do
    -- Task 1 has an existing dep on resolved key 99 (from task.deps)
    -- and an extra pending dep on key 2 (from the resolution map).
    -- Both deps contribute to the unsatisfied set.
    runPromote defaultPromoteSpec {
      pending_ = [(1, Set.singleton 99, False, 10), (2, Set.empty, False, 20)],
      resolutions_ =
        [ (1, (ResolvedVal 10, Set.singleton 2))
        , (2, (ResolvedVal 20, Set.empty))
        ],
      promoteKeys = Set.singleton 1,
      expectUnsatisfied = Set.singleton 1,
      expectReady = Set.singleton 2
    }

test_promoteTransitive :: TestTree
test_promoteTransitive =
  unitTest "transitive promotion through pending deps" do
    runPromote defaultPromoteSpec {
      pending_ =
        [ (1, Set.empty, False, 10)
        , (2, Set.empty, False, 20)
        ],
      resolutions_ =
        [ (1, (ResolvedVal 10, Set.singleton 2))
        , (2, (ResolvedVal 20, Set.empty))
        ],
      promoteKeys = Set.singleton 1,
      expectReady = Set.singleton 2,
      expectUnsatisfied = Set.singleton 1
    }

test_promoteNotInPending :: TestTree
test_promoteNotInPending =
  unitTest "promoting key not in pending is a no-op" do
    runPromote defaultPromoteSpec {
      pending_ = [(1, Set.empty, False, 10)],
      resolutions_ = [(99, (ResolvedVal 99, Set.empty))],
      promoteKeys = Set.singleton 99,
      expectPending = Set.singleton 1
    }

test_promoteNoResolution :: TestTree
test_promoteNoResolution =
  unitTest "pending task without resolution stays pending" do
    runPromote defaultPromoteSpec {
      pending_ = [(1, Set.empty, False, 10)],
      promoteKeys = Set.singleton 1,
      expectPending = Set.singleton 1
    }

test_promoteAlreadyAccepted :: TestTree
test_promoteAlreadyAccepted =
  unitTest "already-accepted key is not in pending, skipped" do
    runPromote defaultPromoteSpec {
      accepted_ = Set.singleton 1,
      resolutions_ = [(1, (ResolvedVal 10, Set.empty))],
      promoteKeys = Set.singleton 1
    }

test_promoteUpdatesAccepted :: TestTree
test_promoteUpdatesAccepted =
  unitTest "promoted tasks are added to accepted set" do
    let
      spec = defaultPromoteSpec {
        pending_ = [(1, Set.empty, False, 10)],
        resolutions_ = [(1, (ResolvedVal 10, Set.empty))],
        promoteKeys = Set.singleton 1
      }
      result = promote (Set.map TK spec.promoteKeys) (specState spec)
    Set.member (TK 1) (Map.keysSet result.accepted) === True

-- ---------------------------------------------------------------------------
-- Tests for 'promoteEnabled'
-- ---------------------------------------------------------------------------

test_promoteEnabledSkipsDisabled :: TestTree
test_promoteEnabledSkipsDisabled =
  unitTest "promoteEnabled skips disabled tasks" do
    runPromoteEnabled defaultPromoteSpec {
      pending_ =
        [ (1, Set.empty, True, 10)
        , (2, Set.empty, False, 20)
        ],
      resolutions_ =
        [ (1, (ResolvedVal 10, Set.empty))
        , (2, (ResolvedVal 20, Set.empty))
        ],
      expectReady = Set.singleton 1,
      expectPending = Set.singleton 2
    }

test_promoteEnabledNoResolution :: TestTree
test_promoteEnabledNoResolution =
  unitTest "promoteEnabled skips enabled tasks without resolution" do
    runPromoteEnabled defaultPromoteSpec {
      pending_ =
        [ (1, Set.empty, True, 10)
        , (2, Set.empty, True, 20)
        ],
      resolutions_ = [(1, (ResolvedVal 10, Set.empty))],
      expectReady = Set.singleton 1,
      expectPending = Set.singleton 2
    }

test_promoteEnabledTransitive :: TestTree
test_promoteEnabledTransitive =
  unitTest "promoteEnabled transitively promotes disabled deps" do
    runPromoteEnabled defaultPromoteSpec {
      pending_ =
        [ (1, Set.empty, True, 10)
        , (2, Set.empty, False, 20)
        ],
      resolutions_ =
        [ (1, (ResolvedVal 10, Set.singleton 2))
        , (2, (ResolvedVal 20, Set.empty))
        ],
      expectReady = Set.singleton 2,
      expectUnsatisfied = Set.singleton 1
    }

-- ---------------------------------------------------------------------------
-- Tests for 'insertPending'
-- ---------------------------------------------------------------------------

test_insertPendingMergesEnabled :: TestTree
test_insertPendingMergesEnabled =
  unitTest "insertPending merges enabled with OR" do
    let
      state = emptyState {
        pending = Map.singleton (TK 1) (pendingTask 1 Set.empty False 10)
      }
      result = insertPending (pendingTask 1 Set.empty True 10) state
    case Map.lookup (TK 1) result.pending of
      Just t -> t.enabled === True
      Nothing -> fail "task should be in pending"

test_insertPendingResolvesImmediately :: TestTree
test_insertPendingResolvesImmediately =
  unitTest "insertPending resolves immediately when resolution exists and enabled" do
    let
      state = emptyState {
        resolutions = mkResolutions [(1, (ResolvedVal 10, Set.empty))]
      }
      result = insertPending (pendingTask 1 Set.empty True 10) state
    Map.null result.pending === True
    readyKeys result === Set.singleton 1

-- | Regression test: two independent requests (e.g. separate 'TriggerBuild' RPCs for sibling modules
-- that share an already-built implicit dependency) each submit a 'Pending' task for the same key once
-- its resolution is already cached. The second call must not append a duplicate entry to 'ready' --
-- 'readyKeys' alone can't detect this, since it de-duplicates through a 'Set', so this asserts on the
-- length of the raw 'ready' list instead.
test_insertPendingDoesNotDuplicateAlreadyAcceptedReadyTask :: TestTree
test_insertPendingDoesNotDuplicateAlreadyAcceptedReadyTask =
  unitTest "insertPending does not duplicate an already-accepted ready task" do
    let
      state = emptyState {
        resolutions = mkResolutions [(1, (ResolvedVal 10, Set.empty))]
      }
      afterFirst = insertPending (pendingTask 1 Set.empty True 10) state
      afterSecond = insertPending (pendingTask 1 Set.empty True 10) afterFirst
    length afterFirst.ready === 1
    Map.null afterSecond.pending === True
    length afterSecond.ready === 1

-- ---------------------------------------------------------------------------
-- Tests for 'addResolutions'
-- ---------------------------------------------------------------------------

test_addResolutionsPromotesPending :: TestTree
test_addResolutionsPromotesPending =
  unitTest "addResolutions promotes enabled pending tasks" do
    let
      state = emptyState {
        pending = Map.fromList
          [ (TK 1, pendingTask 1 Set.empty True 10)
          , (TK 2, pendingTask 2 Set.empty False 20)
          ]
      }
      result = addResolutions (rawResolutions [(1, (ResolvedVal 10, Set.empty)), (2, (ResolvedVal 20, Set.empty))]) state
    pendingKeys result === Set.singleton 2
    readyKeys result === Set.singleton 1

-- ---------------------------------------------------------------------------
-- Tests for recordResult
-- ---------------------------------------------------------------------------

test_recordResultRemovesFromAccepted :: TestTree
test_recordResultRemovesFromAccepted =
  unitTest "recordResult removes the completed key from accepted" do
    let
      state = emptyState {accepted = Map.singleton (TK 1) testGeneration}
      result = recordResult (TK 1) (TaskSuccess Nothing) state
    Map.member (TK 1) result.accepted === False
    Map.member (TK 1) result.completed === True

-- Regression test for the actual UI bug: a scheduler state that outlives a single batch (as
-- GhcServer.Build does across repeated TriggerBuild RPCs from the instrument UI per-module
-- b/r/m keypresses) must allow a key to be reclassified after it has already completed once.
-- Before the fix, recordResult never removed a key from accepted, so this second classifyTask
-- call was a silent no-op forever, reproducing "pressing b/r/m does nothing after the first build".
-- This test is expected to fail if the accepted-pruning line is reverted from recordResult.
test_classifyTaskCanReactivateAfterCompletion :: TestTree
test_classifyTaskCanReactivateAfterCompletion =
  unitTest "classifyTask can re-activate a key after its earlier completion" do
    let
      task = Task {key = TK 1, deps = Set.empty, enabled = True, value = ResolvedVal 10}
      afterFirst = classifyTask task emptyState
      -- Simulate the scheduler loop having already dispatched the sole ready task (as
      -- 'loopSchedule' does by popping the ready list before forking the request), so that only
      -- 'accepted' -- not a stale leftover 'ready' entry -- can account for the key reappearing.
      dispatched = afterFirst {ready = []}
      afterCompletion = recordResult (TK 1) (TaskSuccess Nothing) dispatched
      afterSecond = classifyTask task afterCompletion
    readyKeys afterFirst === Set.singleton 1
    readyKeys afterSecond === Set.singleton 1

-- Demonstrates that the intra-batch dedup invariant the accepted guard maintains is unaffected
-- by the recordResult fix, regardless of fan-out size. A shared dependency (key 0, mirroring an
-- ImplicitDep task with enabled = False) is reachable via a pending dep edge from a large number
-- of sibling modules (keys 1..30, mirroring Explicit per-module requests with enabled = True).
-- All siblings are submitted sequentially against the same evolving state with no recordResult
-- call in between (mirroring a single in-flight window before the shared dependency's own task has
-- finished) -- the shared dependency must still be promoted to ready exactly once.
--
-- This does not exercise genuine OS-thread-level timing (the scheduler loop processes one event at
-- a time, so this pure-function sequence is representative of that single-threaded processing
-- order). A later, separate batch re-triggering the same explicitly-requested key after its
-- completion is the fix's intended, safe behaviour (see test_classifyTaskCanReactivateAfterCompletion),
-- not a regression of this invariant.
test_sharedDependencyResolvedOnceAcrossManySiblings :: TestTree
test_sharedDependencyResolvedOnceAcrossManySiblings =
  unitTest "a shared dependency is resolved once across many sibling requests" do
    let
      siblingCount = 30
      siblings = [1 .. siblingCount]
      resolutions =
        mkResolutions ((0, (ResolvedVal 0, Set.empty)) : [(i, (ResolvedVal i, Set.singleton 0)) | i <- siblings])
      initial =
        emptyState {
          pending = Map.fromList ((TK 0, pendingTask 0 Set.empty False 0) : [(TK i, pendingTask i Set.empty True i) | i <- siblings]),
          resolutions
        }
      submitSibling state i = insertPending (pendingTask i Set.empty True i) state
      final = foldl' submitSibling initial siblings
      sharedDepReadyCount = length (filter (\t -> t.key == TK 0) final.ready)
    sharedDepReadyCount === 1
    Map.null final.pending === True

-- ---------------------------------------------------------------------------
-- Tests for generation-based eligibility
-- ---------------------------------------------------------------------------

-- | The redundant-rebuild case, in pure form: a second request re-submits a pending task whose
-- resolution is a leftover from the first request, and whose key already completed then.  The
-- resolution is not newer than the completion, so there is nothing to do.
--
-- The task must stay pending rather than being dropped, so that a later generation which does
-- find it stale can still activate it -- that is what
-- 'test_staleResolutionReactivatesCompletedKey' checks.
test_upToDateResolutionDoesNotReactivate :: TestTree
test_upToDateResolutionDoesNotReactivate =
  unitTest "a resolution no newer than the key's completion does not re-activate it" do
    let
      state = emptyState {
        completed = Map.singleton (TK 1) testGeneration,
        resolutions = mkResolutions [(1, (ResolvedVal 10, Set.empty))]
      }
      result = insertPending (pendingTask 1 Set.empty True 10) state
    readyKeys result === Set.empty
    unsatisfiedKeys result === Set.empty
    pendingKeys result === Set.singleton 1

-- | The complement: once a newer generation supplies a resolution for the same key (i.e. the
-- staleness analysis of a later request found it stale again, e.g. after an edit), the
-- completed key is activated again.
test_staleResolutionReactivatesCompletedKey :: TestTree
test_staleResolutionReactivatesCompletedKey =
  unitTest "a resolution newer than the key's completion re-activates it" do
    let
      state = emptyState {
        completed = Map.singleton (TK 1) testGeneration,
        generation = nextGeneration testGeneration,
        pending = Map.singleton (TK 1) (pendingTask 1 Set.empty True 10)
      }
      result = addResolutions (rawResolutions [(1, (ResolvedVal 10, Set.empty))]) state
    readyKeys result === Set.singleton 1
    pendingKeys result === Set.empty

-- | The in-flight dedup path is generation-independent: a key that is currently being built is
-- never dispatched twice, no matter how new the resolution is.  Unlike the up-to-date case, the
-- pending entry is consumed, because the work it asks for is already happening.
test_inFlightKeyIsDedupedRegardlessOfGeneration :: TestTree
test_inFlightKeyIsDedupedRegardlessOfGeneration =
  unitTest "an in-flight key is deduped even for a newer resolution" do
    let
      state = emptyState {
        accepted = Map.singleton (TK 1) initialGeneration,
        resolutions = mkResolutions [(1, (ResolvedVal 10, Set.empty))]
      }
      result = insertPending (pendingTask 1 Set.empty True 10) state
    readyKeys result === Set.empty
    unsatisfiedKeys result === Set.empty
    pendingKeys result === Set.empty

-- | Completions are stamped with the generation the task was /activated/ in, not the one
-- current when it finished.  A request that arrived while the task was already running did not
-- influence it, so crediting the task with satisfying that request would silently serve a
-- result computed from older inputs.
test_completionUsesActivationGeneration :: TestTree
test_completionUsesActivationGeneration =
  unitTest "a completion is stamped with the generation it was activated in" do
    let
      activated = classifyTask (Task {key = TK 1, deps = Set.empty, enabled = True, value = ResolvedVal 10}) emptyState
      -- A further request arrives while the task is in flight.
      later = (bumpGeneration activated) {ready = []}
      result = recordResult (TK 1) (TaskSuccess Nothing) later
    Map.lookup (TK 1) result.completed === Just testGeneration

-- ---------------------------------------------------------------------------
-- Test tree
-- ---------------------------------------------------------------------------

test_scheduler :: TestTree
test_scheduler =
  dependentTestGroup "GhcServer.Scheduler" AllFinish
    [ dependentTestGroup "promote" AllFinish
        [ test_promoteSingleNoDeps
        , test_promoteWithUnmetDep
        , test_promoteDepAlreadyCompleted
        , test_promoteExtraDeps
        , test_promoteTransitive
        , test_promoteNotInPending
        , test_promoteNoResolution
        , test_promoteAlreadyAccepted
        , test_promoteUpdatesAccepted
        ]
    , dependentTestGroup "promoteEnabled" AllFinish
        [ test_promoteEnabledSkipsDisabled
        , test_promoteEnabledNoResolution
        , test_promoteEnabledTransitive
        ]
    , dependentTestGroup "addResolutions" AllFinish
        [ test_addResolutionsPromotesPending
        ]
    , dependentTestGroup "insertPending" AllFinish
    [ test_insertPendingMergesEnabled
        , test_insertPendingResolvesImmediately
        , test_insertPendingDoesNotDuplicateAlreadyAcceptedReadyTask
        ]
    , dependentTestGroup "recordResult" AllFinish
        [ test_recordResultRemovesFromAccepted
        , test_classifyTaskCanReactivateAfterCompletion
        , test_sharedDependencyResolvedOnceAcrossManySiblings
        ]
    , dependentTestGroup "generations" AllFinish
        [ test_upToDateResolutionDoesNotReactivate
        , test_staleResolutionReactivatesCompletedKey
        , test_inFlightKeyIsDedupedRegardlessOfGeneration
        , test_completionUsesActivationGeneration
        ]
    ]
