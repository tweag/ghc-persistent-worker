-- | Bundled arguments that flow through the build pipeline.
module GhcServer.Data.BuildEnv where

import Control.Concurrent.Chan (Chan)
import Control.Concurrent.MVar (MVar)
import Data.IORef (IORef)
import Data.Map.Strict (Map)
import Data.Set (Set)
import GhcServer.Build.Diff (UnitDiff)
import GhcServer.Data.BuildEvent (BuildEvents)
import GhcServer.Data.Unit (Project)
import System.OsPath (OsPath)
import Types.Api (Event, UnitName)
import Types.Args (Args)
import Types.Log (Logger)
import Types.State (WorkerState)

-- | Common arguments threaded from 'runBuild' through dispatch to worker adapters.
data BuildEnv =
  BuildEnv {
    baseArgs :: Args,
    projectRoot :: OsPath,
    outputDir :: OsPath,
    tmpDir :: OsPath,
    stateVar :: MVar WorkerState,
    project :: Project,
    log :: Logger,
    events :: BuildEvents,
    -- | Channel for pushing 'Types.Api.Event's to the instrument UI, if the @instrument@ feature is
    -- enabled. 'Nothing' when the feature is disabled, avoiding the cost of constructing events that nobody
    -- consumes.
    instrChan :: Maybe (Chan Event),
    -- | Memoized result of building the project's external Cabal dependencies into the store, shared by
    -- all units so the build only runs once per server lifetime. 'Nothing' until first requested.
    extDepsDb :: MVar (Maybe (Either String OsPath)),
    -- | Per-unit incremental analysis results (Phase 0 source diff + old module graph), written at
    -- classification time and consumed on metadata completion and at digest-commit time.
    diff :: MVar (Map UnitName UnitDiff),
    -- | Monotonic counter allocating a unique 'Types.Api.Event' request id for each task-dispatch
    -- instance (see 'GhcServer.Build.Propagate.nextRequestId'), included in the 'CompileStart'\/'CompileEnd'\/
    -- 'PhaseStart'\/'PhaseEnd' events a task emits, so the @instrument@ UI can match events to the exact task
    -- instance instead of matching by target text.
    requestIdCounter :: IORef Int,
    -- | Units whose @execute@ tasks should run in a fresh child process (spawned via
    -- 'GhcServer.Build.Process.executeModuleTaskProcess') rather than in-process, for the batch currently being
    -- dispatched. Written by 'GhcServer.Build.Classify.classifyBuildRequest' from the request's @--process@ flag
    -- and read by 'GhcServer.Build.Propagate.dispatchTask'. Like 'diff', this is request-scoped bookkeeping
    -- threaded outside the scheduler's own 'BuildExt'/'TaskKey' machinery, since a task's resolved 'BuildStatus'
    -- is entirely recomputed at promotion time (see 'GhcServer.Build.Schedule.resolutionsFromModuleMap') and has
    -- no way to carry a value chosen at classification time through to dispatch.
    processUnits :: MVar (Set UnitName)
  }
