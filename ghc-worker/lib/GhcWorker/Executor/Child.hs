-- | The child side of a persistent executor subprocess that serves bytecode plans (see
-- 'Internal.Executor.Client' for the parent side and 'Internal.Executor.Load' for the session handling).
--
-- A persistent executor serves the @Executor@ gRPC service (@proto\/executor.proto@) on a Unix socket, so that the
-- GHC session state (loaded bytecode, interpreter) built up by one call is reused by the next one dispatched to the
-- same executor.
--
-- Calls are serialized, since each evaluation captures the process-global stdout\/stderr handles.
--
-- The executor terminates when its stdin reaches EOF: the parent keeps the write end of a pipe open for as long as
-- it considers the executor alive, so the child also exits when the parent dies without terminating it.
module GhcWorker.Executor.Child where

import BuckWorkerProto ()
import Common.Grpc (runGrpcServer)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race_)
import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Exception (bracket_)
import Control.Monad (forever)
import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString, toStrict)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Text (Text, pack)
import GHC.Stats (GCDetails (..), RTSStats (..), getRTSStats, getRTSStatsEnabled)
import Internal.Executor.Load (PlanSessionVar, executePlan, newPlanSessionVar)
import Internal.Log (newLogger)
import Network.GRPC.Common (NextElem (..))
import Network.GRPC.Common.Protobuf (Proto, defMessage, (&), (.~))
import Network.GRPC.Server.Protobuf (ProtobufMethodsOf)
import Network.GRPC.Server.StreamType (Methods, mkNonStreaming, mkServerStreaming, simpleMethods)
import Proto.Executor (Empty, Encoded, Executor)
import qualified Proto.Executor_Fields as Fields
import qualified System.IO as IO
import System.IO.Silently (capture, hCapture)
import System.Mem (performMajorGC)
import System.OsPath.Extra (OsPath)
import Types.Api (ExecutorStatus (..), ProcessStats (..))
import Types.ExecutorPlan (PlanRequest)
import Types.Log (Logger (..), newLog)
import Types.Executor.Protocol (CallResult (..), Captured (..), EvalOutcome, ExecutorCall (..), SetupError (..))

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

-- | Run a bytecode plan, capturing stdout and stderr.
planResult :: PlanSessionVar -> PlanRequest -> IO CallResult
planResult planVar plan = do
  logger <- newLogger <$> newLog Nothing
  (stderr, (stdout, outcome)) <- hCapture [IO.stderr] (capture (executePlan planVar plan))
  finishResult logger Captured {stdout = pack stdout, stderr = pack stderr} outcome

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

-- | Implementation of the @Events@ RPC: publish a JSON-encoded 'ExecutorStatus' every 'statusIntervalUs' until the
-- client disconnects.
statusEvents :: IORef Int -> Proto Empty -> (NextElem (Proto Encoded) -> IO ()) -> IO ()
statusEvents active _ callback =
  forever do
    status <- currentStatus active
    callback (NextElem (defMessage & Fields.payload .~ toStrict (Aeson.encode status)))
    threadDelay statusIntervalUs

-- | Handle one @Execute@ call: pass the payload to the handler while holding the lock and encode the result.
executeCall :: IORef Int -> MVar () -> (ByteString -> IO CallResult) -> Proto Encoded -> IO (Proto Encoded)
executeCall active lock handler request = do
  -- Counted outside the lock, so that calls waiting for it are included.
  result <- bracket_ (bump 1) (bump (-1)) (withMVar lock \ () -> handler request.payload)
  pure (defMessage & Fields.payload .~ toStrict (Aeson.encode result))
  where
    bump n = atomicModifyIORef' active \ count -> (count + n, ())

-- | Block until stdin is closed by the parent.
awaitStdinEof :: IO ()
awaitStdinEof =
  IO.hIsEOF IO.stdin >>= \case
    True -> pure ()
    False -> IO.hGetLine IO.stdin >> awaitStdinEof

-- | Serve the @Executor@ service on the given socket with a handler for the payloads of @Execute@ calls, until stdin
-- closes.
serveExecutor :: OsPath -> (ByteString -> IO CallResult) -> IO ()
serveExecutor socket handler = do
  lock <- newMVar ()
  active <- newIORef 0
  race_ awaitStdinEof (runGrpcServer socket (executorMethods active lock handler))

-- | The generated method list is alphabetical: @events@, @execute@.
executorMethods ::
  IORef Int ->
  MVar () ->
  (ByteString -> IO CallResult) ->
  Methods IO (ProtobufMethodsOf Executor)
executorMethods active lock handler =
  simpleMethods
    (mkServerStreaming (statusEvents active))
    (mkNonStreaming (executeCall active lock handler))

-- | Entry point of the worker's @executor@ CLI mode: serve JSON-encoded 'ExecutorCall's.
-- Module calls are only supported by the @ghc-server@'s executors.
runPlanExecutor :: OsPath -> IO ()
runPlanExecutor socket = do
  planVar <- newPlanSessionVar
  serveExecutor socket \ payload ->
    either (setupFailure . InvalidCall . pack) (dispatch planVar) (Aeson.eitherDecodeStrict' payload)
  where
    dispatch planVar = \case
      CallPlan plan -> planResult planVar plan
      CallModule _ -> setupFailure (Unsupported "The worker's executors only run bytecode plans")
