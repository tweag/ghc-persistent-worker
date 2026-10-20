-- | The child side of a persistent executor subprocess (see 'Internal.Executor.Client' for the parent side).
--
-- A persistent executor serves the @Executor@ gRPC service (@proto\/executor.proto@) on a Unix socket, so that the
-- GHC session state built up by one call is reused by the next one dispatched to the same executor.
--
-- Calls run concurrently; hosts that serve module calls have to serialize those themselves, since they share one
-- 'Types.State.WorkerState'.
-- Output is attributed to calls by thread (see 'Internal.Executor.Output').
--
-- The executor terminates when its stdin reaches EOF: the parent keeps the write end of a pipe open for as long as
-- it considers the executor alive, so the child also exits when the parent dies without terminating it.
module GhcWorker.Executor.Child where

import Data.IORef (IORef, readIORef)
import Data.Text (pack)
import GHC.Stats (GCDetails (..), RTSStats (..), getRTSStats, getRTSStatsEnabled)
import Internal.Log (newLogger)
import System.Mem (performMajorGC)
import Types.Api (ExecutorStatus (..), ProcessStats (..))
import Types.Log (Logger (..), newLog)
import Types.Executor.Protocol (CallResult (..), Captured (..), EvalOutcome, SetupError (..))

-- | Assemble the result of a call: the captured output, the log messages and the executor's RTS stats.
finishResult :: Logger -> Captured -> Either SetupError EvalOutcome -> IO CallResult
finishResult logger captured outcome = do
  logger.debug ("Evaluation result: " <> show outcome)
  logMessages <- fmap pack <$> logger.flush
  -- 'max_mem_in_use_bytes'/'max_live_bytes' are only updated when a GC actually runs; a short-lived subprocess
  -- task can otherwise exit without ever triggering one, reporting all-zero stats despite '-T' being enabled
  -- (see the '-with-rtsopts' flags on the executable). Force one last GC so the reported peak reflects reality.
  -- In a persistent executor, these are peaks over the executor's entire lifetime, not just this call.
  performMajorGC
  rtsStats <- getRTSStats
  pure CallResult {
    captured,
    logMessages,
    outcome,
    stats = ProcessStats {
      maxMemInUseBytes = rtsStats.max_mem_in_use_bytes,
      maxLiveBytes = rtsStats.max_live_bytes
    }
  }

-- | The result for a call that could not be run.
setupFailure :: SetupError -> IO CallResult
setupFailure err = do
  logger <- newLogger <$> newLog Nothing
  finishResult logger Captured {stdout = "", stderr = ""} (Left err)

-- | Interval between two status snapshots published by 'statusEvents', in microseconds.
statusIntervalUs :: Int
statusIntervalUs = 1_000_000

-- | Snapshot the executor's RTS memory (as of the last GC, so no GC is forced) and its number of active calls.
-- Memory is reported as zero if the RTS was not started with @-T@.
currentStatus :: IORef Int -> IO ExecutorStatus
currentStatus active = do
  activeTasks <- readIORef active
  enabled <- getRTSStatsEnabled
  (memInUseBytes, liveBytes) <-
    if enabled
    then (\ stats -> (stats.gc.gcdetails_mem_in_use_bytes, stats.gc.gcdetails_live_bytes)) <$> getRTSStats
    else pure (0, 0)
  pure ExecutorStatus {memInUseBytes, liveBytes, activeTasks}
