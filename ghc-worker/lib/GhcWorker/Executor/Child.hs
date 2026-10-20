-- | Executor logic.
module GhcWorker.Executor.Child where

import Data.Text (pack)
import GHC.Stats (GCDetails (..), RTSStats (..), getRTSStats, getRTSStatsEnabled)
import Internal.Log (newLogger)
import System.Mem (performMajorGC)
import Types.Api (MemoryUsage (..))
import Types.Executor.Protocol (CallResult (..), Captured (..), EvalOutcome, SetupError (..))
import Types.Log (Logger (..), newLog)

memoryUsageZero :: MemoryUsage
memoryUsageZero =
  MemoryUsage {allocated = 0, live = 0}

-- | This needs the process o be run with @+RTS -T -RTS@.
memoryIfEnabled :: (RTSStats -> MemoryUsage) -> IO MemoryUsage
memoryIfEnabled use =
  getRTSStatsEnabled >>= \case
    False ->
      pure memoryUsageZero
    True -> do
      performMajorGC
      use <$> getRTSStats

-- TODO move to shared module, and reuse at all use sites. Those don't perform GC, so might be desynced.
-- Check if GC at the given frequency is a perf issue as well
memoryUsage :: IO MemoryUsage
memoryUsage =
  memoryIfEnabled \ rtsStats ->
    MemoryUsage {
      allocated = rtsStats.gc.gcdetails_mem_in_use_bytes,
      live = rtsStats.gc.gcdetails_live_bytes
    }

maxMemoryUsage :: IO MemoryUsage
maxMemoryUsage =
  memoryIfEnabled \ rtsStats ->
    MemoryUsage {
      allocated = rtsStats.max_mem_in_use_bytes,
      live = rtsStats.max_live_bytes
    }

finishResult :: Logger -> Captured -> Either SetupError EvalOutcome -> IO CallResult
finishResult logger captured outcome = do
  logger.debug ("Evaluation result: " <> show outcome)
  logMessages <- fmap pack <$> logger.flush
  maxMemory <- maxMemoryUsage
  pure CallResult {..}

-- | The result for a call that could not be run.
setupFailure :: SetupError -> IO CallResult
setupFailure err = do
  logger <- newLogger <$> newLog Nothing
  finishResult logger Captured {stdout = "", stderr = ""} (Left err)
