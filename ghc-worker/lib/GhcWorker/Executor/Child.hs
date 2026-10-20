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

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (withAsync)
import Control.Concurrent.Chan (readChan, writeChan)
import Control.Exception (bracket_)
import Control.Monad (forever)
import qualified Data.Aeson as Aeson
import Data.ByteString (toStrict)
import Data.IORef (IORef, atomicModifyIORef', readIORef)
import Data.Text (pack)
import GHC.Stats (GCDetails (..), RTSStats (..), getRTSStats, getRTSStatsEnabled)
import Internal.Executor.Output (Attach, OutputRouter, scopedTask, withSubscription)
import Internal.Log (newLogger)
import Network.GRPC.Common (NextElem (..))
import Network.GRPC.Common.Protobuf (Proto, defMessage, (&), (.~))
import Proto.Executor (Empty, Encoded)
import qualified Proto.Executor_Fields as Fields
import qualified System.IO as IO
import System.Mem (performMajorGC)
import Types.Api (ExecutorStatus (..), ProcessStats (..))
import Types.Log (Logger (..), newLog)
import Types.Executor.Protocol (
  CallResult (..),
  Captured (..),
  EvalOutcome,
  ExecutorCall (..),
  ExecutorEvent (..),
  ExecutorRequest (..),
  SetupError (..),
  )

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

-- | Implementation of the @Events@ RPC: publish a JSON-encoded 'StatusEvent' every 'statusIntervalUs', and an
-- 'OutputEvent' for every line the executor writes, until the client disconnects.
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

-- | Handle one @Execute@ call: pass the call to the handler and encode the result.
-- The output written by the call is collected and returned as the result's 'Captured'.
executeCall ::
  OutputRouter ->
  IORef Int ->
  (Attach -> ExecutorCall -> IO CallResult) ->
  Proto Encoded ->
  IO (Proto Encoded)
executeCall router active handler request = do
  result <- case Aeson.eitherDecodeStrict' request.payload of
    Left err -> setupFailure (InvalidCall (pack err))
    Right ExecutorRequest {requestId, call} -> bracket_ (bump 1) (bump (-1)) (run requestId call)
  pure (defMessage & Fields.payload .~ toStrict (Aeson.encode result))
  where
    run requestId call = do
      (captured, result) <- scopedTask router requestId \ attach -> handler attach call
      pure result {captured}

    bump n = atomicModifyIORef' active \ count -> (count + n, ())

-- | Block until stdin is closed by the parent.
awaitStdinEof :: IO ()
awaitStdinEof =
  IO.hIsEOF IO.stdin >>= \case
    True -> pure ()
    False -> IO.hGetLine IO.stdin >> awaitStdinEof
