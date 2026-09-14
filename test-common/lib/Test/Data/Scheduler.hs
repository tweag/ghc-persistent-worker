{-# LANGUAGE UndecidableInstances #-}

module Test.Data.Scheduler where

import Control.Concurrent.STM (TQueue, TVar)
import Data.Kind (Constraint, Type)
import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Text (Text)

-- | Phase of a task in the scheduler lifecycle.
data Phase =
  -- | Task is awaiting resolution.
  Pending
  |
  -- | Task is eligible for dispatch.
  Resolved

-- | Constraint alias requiring 'Ord' for both phases of a key.
type OrdKey :: (Phase -> Type) -> Constraint
type OrdKey key = (Ord (key 'Pending), Ord (key 'Resolved))

-- | Monotonically increasing version stamp, bumped once per external request.
-- Every activation and every resolution entry carries the generation it was computed at, so a later request can decide
-- whether a result is stale.
newtype Generation =
  Generation { number :: Word }
  deriving stock (Eq, Show)
  deriving newtype (Num, Real, Enum, Integral, Ord)

-- | Result of executing a build task.
data TaskResult f =
  -- | The task succeeded, optionally carrying the result of an evaluated expression.
  TaskSuccess (Maybe Text)
  |
  TaskFailed f
  deriving stock (Eq, Show)

-- | A build task with a key, dependencies, and a dispatchable value.
--
-- The key and value are indexed by the same phase @p@, but dependencies are always 'Resolved', since they are required
-- to be completed before this task can be processed.
data Task (key :: Phase -> Type) (p :: Phase) a =
  Task {
    key :: key p,
    -- | Dependencies, always expressed as resolved keys.
    deps :: Set (key 'Resolved),
    -- | Whether the task is eligible for promotion from the pending pool.
    enabled :: Bool,
    value :: a
  }

deriving stock instance (Show (key p), Show (key 'Resolved), Show a) => Show (Task key p a)
deriving stock instance (Eq (key p), Eq (key 'Resolved), Eq a) => Eq (Task key p a)

-- | Resolution of a pending task key.
--
-- Tasks get submitted with potentially incomplete target specifications, like a source path that has to be resolved
-- to a module name.
--
-- After some analysis has provided the missing data, the task gets promoted to the resolved state.
-- Usually that analysis is the build plan providing a module mapping and dependency graph, but it is abstract and
-- implemented differently by test builder and ghc-server.
--
-- This type then stores the task's 'Resolved' key, its 'Pending' dependencies, and its generation.
-- The generation number is increased in each subsequent dependency analysis step, to decide which resolution is up to
-- date after changes were made to the project.
data Resolution (key :: Phase -> Type) =
  Resolution {
    -- | Resolved version of the task key, e.g. replacing a source path with a module name.
    key :: key 'Resolved,
    -- | Task dependencies.
    deps :: Set (key 'Pending),
    -- | The generation in which this resolution was computed.
    computedAt :: Generation
  }

deriving stock instance (Show (key 'Pending), Show (key 'Resolved)) => Show (Resolution key)
deriving stock instance (Eq (key 'Pending), Eq (key 'Resolved)) => Eq (Resolution key)

-- | Whether a resolution should activate its task.
data Activation =
  -- | The key is not in flight and no completion supersedes the resolution.
  ActivateNow
  |
  -- | The key is already in flight, activated in the given generation.
  SkipInFlight Generation
  |
  -- | The key completed in a generation at least as recent as the resolution.
  SkipUpToDate Generation

-- | A step in the scheduler's reasoning about whether to run a task, recorded for observability.
--
-- Whenever the scheduler receives a request or a resolution, or a task completes, it decides what to do with the task in
-- question: start it, skip it because it is already running, or skip it because its result is still current.
-- Each of these outcomes is recorded as one of these values, so that it can be observed from the outside.
--
-- Decisions not to run a task are recorded just like the ones to run it.
-- Without them, a task that wasn't built could not be told apart from one that was dropped by mistake.
--
-- Most constructors carry one or two generations, which are the ones the decision was based on, for example the one of
-- the resolution and the one of the completion it was compared with.
data SchedulerDecision (key :: Phase -> Type) =
  -- | A new request was accepted, starting the given generation.
  DecisionGeneration Generation
  |
  -- | A pending key was resolved and its task activated in the given generation.
  DecisionActivated (key 'Resolved) Generation
  |
  -- | A pending key was resolved to a task that is already in flight, so the duplicate submission was dropped
  -- instead of dispatched.
  -- The generation is the one the running task was activated in.
  DecisionDeduped (key 'Resolved) Generation
  |
  -- | A pending key was resolved to a task that has already completed with a result at least as recent as the
  -- resolution, so there is nothing to do.
  -- The task stays in the pending pool, so that a later resolution can still activate it.
  -- The first generation is the one of the resolution, the second the one of the completion.
  DecisionUpToDate (key 'Resolved) Generation Generation
  |
  -- | A resolution was recorded for a pending key in the given generation.
  -- The last field is the generation of the resolution it replaced, if there was one.
  DecisionResolution (key 'Pending) Generation (Maybe Generation)
  |
  -- | A task completed.
  -- The generation is the one it was activated in.
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
    -- | When 'False', the scheduler stops dispatching new tasks after the first failure and drains remaining
    -- ready/unsatisfied tasks without executing them.
    continueOnFailure :: Bool
  }

-- | The outcome of propagating a task's completion, returned by the 'propagate' handler.
--
-- The scheduler applies it to its state in one step: it installs the new extension state, merges the resolutions
-- (promoting the pending tasks they enable) and records the result. The handler therefore never touches the
-- scheduler's own bookkeeping.
data Propagated (key :: Phase -> Type) f ext =
  Propagated {
    -- | The result to record for the completed task.
    --
    -- Usually the dispatched one, but propagation may turn a success into a failure, for example when the task's
    -- output turns out to be unusable. This has to be reported here: recording a result overwrites the task's entry
    -- in the failures, so a failure registered any other way would be lost.
    result :: TaskResult f,
    -- | The new extension state.
    newExt :: ext,
    -- | Resolutions for pending tasks that became known through this completion.
    newResolutions :: Map (key 'Pending) (key 'Resolved, Set (key 'Pending))
  }

-- | Domain-specific handler callbacks for the scheduler, implemented by concrete build systems like the test builder or
-- the @ghc-server@.
-- The build system's extension type @ext@ is threaded through various handlers as pure state.
data Handlers request (key :: Phase -> Type) task f ext =
  Handlers {
    -- | Dispatch a resolved task to a worker.
    dispatch :: ext -> Task key 'Resolved task -> IO (TaskResult f),

    -- | Convert a request into active and pending task lists.
    classify :: request -> IO ([Task key 'Resolved task], [Task key 'Pending task]),

    -- | Apply domain-specific effects after a task completes.
    --
    -- Computes the outcome from a snapshot of the state; the scheduler itself applies it (see 'Propagated').
    propagate ::
      key 'Resolved ->
      TaskResult f ->
      SchedulerState key task f ext ->
      IO (Propagated key f ext),

    -- | Process a scheduler decision for debugging and status reporting, for example in the UI or tests.
    reportDecision :: SchedulerDecision key -> IO ()
  }

-- | All mutable scheduler state.
--
-- The @ext@ parameter allows the build layer to store domain-specific data alongside the scheduler's own state.
data SchedulerState (key :: Phase -> Type) task f ext =
  SchedulerState {
    -- | Tasks waiting on unmet dependencies.
    unsatisfied :: Map (key 'Resolved) (Task key 'Resolved task, Set (key 'Resolved)),

    -- | Tasks ready for dispatch.
    ready :: [Task key 'Resolved task],

    -- | Added tasks that haven't been resolved to their full target yet.
    pending :: Map (key 'Pending) (Task key 'Pending task),

    -- | Keys that have finished, mapped to the generation their run was activated in.
    completed :: Map (key 'Resolved) Generation,

    -- | All resolved keys that are currently in flight, mapped to the generation they were activated in.
    -- Used for idempotent enqueueing.
    accepted :: Map (key 'Resolved) Generation,

    activeCount :: Int,

    failures :: Map (key 'Resolved) f,

    -- | Resolutions for each task key, determining its dependencies.
    resolutions :: Map (key 'Pending) (Resolution key),

    -- | The generation of the most recent request.
    generation :: Generation,

    -- | Domain-specific state threaded through 'propagate'.
    ext :: ext
  }

-- | Mutable scheduler state, shared across worker threads and external callers.
data SchedulerResources request (key :: Phase -> Type) task f ext =
  SchedulerResources {
    -- | Event queue for inbox requests and task completions.
    events :: TQueue (SchedulerEvent request key f),
    state :: TVar (SchedulerState key task f ext)
  }
