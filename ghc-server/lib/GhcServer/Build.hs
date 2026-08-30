-- | Scheduler orchestration for the standalone GHC server.
--
-- This module manages the scheduler lifecycle: creating builds, submitting
-- batches, awaiting results.  It delegates build-system logic to
-- 'GhcServer.Build.Classify' and worker\/GHC adaptation to
-- 'GhcServer.Build.Propagate'.
module GhcServer.Build (
  -- * Build lifecycle
  Build (..),
  newBuild,
  scheduleBatch,
  awaitBuild,
  buildDecisions,
  stopBuild,
  runBuild,
  newBuildState,
  -- * Re-exports
  BuildResult (..),
  Tracing (..),
) where

import Control.Concurrent.Async (Async, cancel)
import Control.Concurrent.MVar (MVar, readMVar)
import Control.Concurrent.STM (atomically, readTVar, readTVarIO)
import Data.Foldable (traverse_)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Void (Void)
import GHC.Utils.Outputable (text)
import GhcServer.Build.Classify (BuildResult (..), classifyBuildRequest, collectBuildResult)
import GhcServer.Build.Diff (commitDigests)
import GhcServer.Build.Propagate (
  dispatchTask,
  propagateCompletion,
  )
import GhcServer.Build.Schedule (BuildExt (..), BuildStatus, ModuleInfo (..), ModuleKey (..), TaskKey (..), emptyBuildExt)
import GhcServer.Cache (mkBuildCache)
import GhcServer.Data.BuildEnv (BuildEnv (..))
import GhcServer.Data.Request (ScheduleRequest)
import GhcServer.Data.Unit (Project (..))
import GhcServer.Scheduler (
  Handlers (..),
  SchedulerDecision,
  SchedulerEnv (..),
  SchedulerResources (..),
  SchedulerState (..),
  Tracing (..),
  awaitIdle,
  newSchedulerState,
  runScheduler,
  schedulerDecisions,
  schedulerInvariantViolations,
  submitRequest,
  )
import Internal.State (newState)
import Prelude hiding (log)
import System.OsPath (OsPath)
import Types.Log (Logger (..))
import Types.State (WorkerState)

-- | Shared state for a build session.
-- Created once, supports multiple 'scheduleBatch' calls.
data Build =
  Build {
    scheduler :: SchedulerResources ScheduleRequest TaskKey BuildStatus String BuildExt,
    thread :: Async Void,
    -- | The environment, retained for digest commits at batch completion.
    env :: BuildEnv
  }

-- | Create a new build session.
--
-- Starts the scheduler loop in a background thread.  The loop classifies requests
-- and dispatches tasks.  Metadata completion triggers resolution and promotion
-- of pending compile tasks via the 'propagate' callback.
--
-- The 'Tracing' flag enables the scheduler's internal decision log, readable with
-- 'buildDecisions'.  It is meant for tests and diagnostics; production callers pass
-- 'TracingOff', for which recording is a no-op.
newBuild :: Tracing -> Int -> Int -> BuildEnv -> IO Build
newBuild tracing maxJobs taskTimeout buildEnv = do
  let cache = mkBuildCache buildEnv.outputDir buildEnv.project
  scheduler <- newSchedulerState tracing emptyBuildExt
  let
    env = SchedulerEnv {
      maxJobs,
      handlers = Handlers {
        dispatch = dispatchTask buildEnv,
        classify = classifyBuildRequest buildEnv,
        propagate = propagateCompletion cache buildEnv
      },
      taskTimeout,
      mkFailure = id,
      continueOnFailure = True
    }
  thread <- runScheduler env scheduler
  pure Build {scheduler, thread, env = buildEnv}

-- | Submit a batch of build requests to the scheduler.  Non-blocking.
scheduleBatch :: Build -> ScheduleRequest -> IO ()
scheduleBatch cb request =
  submitRequest cb.scheduler request

-- | Wait for all submitted tasks to complete, then collect results.
--
-- After the batch drains, commits the digest records of all fully-built units so the next
-- session's Phase 0 analysis sees them as up to date.
awaitBuild :: Build -> IO BuildResult
awaitBuild cb = do
  state@SchedulerState {completed, failures, ext} <- atomically do
    awaitIdle cb.scheduler
    readTVar cb.scheduler.state
  reportInvariantViolations cb.env.log (schedulerInvariantViolations state)
  diffs <- readMVar cb.env.diff
  let
    metaFailedUnits = Set.fromList [name | MetaTask name <- Map.keys failures]
    compiledModules = Set.fromList
      [ ModuleKey {unit, name}
      | ResolvedModule unit name <- Map.keys completed
      , not (Map.member (ResolvedModule unit name) failures)
      ]
    modulePaths = Map.mapMaybe modulePath ext.moduleMap
  commitDigests cb.env.project.units diffs metaFailedUnits modulePaths compiledModules ext.stale
  pure (collectBuildResult (Map.keysSet completed) failures)
  where
    modulePath :: ModuleInfo -> Maybe OsPath
    modulePath info = case info.task of
      PendingSource _ src -> Just src
      _ -> Nothing

-- | Log every scheduler-state invariant violation found for the batch that just drained, if any.
--
-- These indicate a scheduler bug, not a build failure, so they are reported via the logger's
-- 'fatal' channel (captured for diagnostics) rather than surfaced as part of the 'BuildResult'.
reportInvariantViolations :: Logger -> [String] -> IO ()
reportInvariantViolations log =
  traverse_ (log.fatal . text . ("scheduler invariant violation: " ++))

-- | Read the scheduler's internal decision log in chronological order.
--
-- Empty unless the build was created with 'TracingOn'.
buildDecisions :: Build -> IO [SchedulerDecision TaskKey]
buildDecisions cb =
  schedulerDecisions <$> readTVarIO cb.scheduler.state

-- | Wait for all submitted tasks, collect results, and cancel the scheduler thread.
--
-- Use this for one-shot builds and tests to avoid leaking the background thread.
stopBuild :: Build -> IO BuildResult
stopBuild cb = do
  result <- awaitBuild cb
  cancel cb.thread
  pure result

-- | Dispatch a build using the concurrent scheduler.
--
-- Creates a scheduler, submits one batch, waits for completion, then cancels
-- the scheduler thread.  For persistent schedulers, use 'newBuild',
-- 'scheduleBatch', and 'awaitBuild' directly.
runBuild :: Int -> Int -> BuildEnv -> ScheduleRequest -> IO BuildResult
runBuild maxJobs taskTimeout env schedule = do
  cb <- newBuild TracingOff maxJobs taskTimeout env
  scheduleBatch cb schedule
  stopBuild cb

-- | Create a fresh 'WorkerState' for use with 'runBuild'.
newBuildState :: IO (MVar WorkerState)
newBuildState = newState
