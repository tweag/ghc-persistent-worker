-- | Executor logic.
module GhcWorker.Executor.Child where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (withAsync)
import Control.Concurrent.Chan (readChan, writeChan)
import Control.Monad (forever)
import qualified Data.Aeson as Aeson
import Data.ByteString (toStrict)
import Data.IORef (IORef, readIORef)
import Data.Text (pack)
import GHC.Stats (GCDetails (..), RTSStats (..), getRTSStats, getRTSStatsEnabled)
import Internal.Executor.Output (OutputRouter, withSubscription)
import Internal.Log (newLogger)
import Network.GRPC.Common (NextElem (..))
import Network.GRPC.Common.Protobuf (Proto, defMessage, (&), (.~))
import Proto.Executor (Empty, Encoded)
import qualified Proto.Executor_Fields as Fields
import System.Mem (performMajorGC)
import Types.Api (ExecutorStatus (..), MemoryUsage (..))
import Types.Executor.Protocol (CallResult (..), Captured (..), EvalOutcome, ExecutorEvent (..), SetupError (..))
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

-- | Interval between two status snapshots published by 'statusEvents', in microseconds.
statusIntervalUs :: Int
statusIntervalUs = 1_000_000

-- | Snapshot the executor's memory usage and its number of active calls.
currentStatus :: IORef Int -> IO ExecutorStatus
currentStatus active = do
  activeTasks <- readIORef active
  memory <- memoryUsage
  pure ExecutorStatus {memory, activeTasks}

-- | Handler for the streaming gRPC method @Events@ of the service @executor@.
--
-- Subscribes to the executor's event channel and forwards each event to the gRPC stream.
-- In a second thread, reads the current status every second and publishes a 'StatusEvent'.
--
-- TODO why is the third arg unused?
-- TODO how does this terminate? readChan throws an exception?
statusEvents :: OutputRouter -> IORef Int -> Proto Empty -> (NextElem (Proto Encoded) -> IO ()) -> IO ()
statusEvents router active _ callback =
  withSubscription router \ events ->
    withAsync (publishStatus events) \ _ ->
      forever (readChan events >>= callback . NextElem . encodeEvent)
  where
    publishStatus events =
      forever do
        status <- currentStatus active
        writeChan events (StatusEvent status)
        threadDelay statusIntervalUs

    encodeEvent event = defMessage & Fields.payload .~ toStrict (Aeson.encode event)
