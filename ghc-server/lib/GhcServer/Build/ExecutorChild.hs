-- | The child side of a persistent executor subprocess (see 'GhcServer.Build.Executor' for the parent side).
--
-- Unlike the one-shot child in 'GhcServer.Build.ProcessChild', a persistent executor keeps a single
-- 'Types.State.WorkerState' for its whole lifetime and serves the @Executor@ gRPC service on a Unix socket, so
-- that the GHC session state (loaded interfaces, bytecode, interpreter) built up by one execute task is reused by
-- the next one dispatched to the same 'Types.Api.ExecutorId'.
--
-- Calls are serialized, since each evaluation captures the process-global stdout\/stderr handles.
--
-- The executor terminates when its stdin reaches EOF: the parent keeps the write end of a pipe open for as long as
-- it considers the executor alive, so the child also exits when the parent dies without terminating it.
module GhcServer.Build.ExecutorChild where

import BuckWorkerProto ()
import Common.Grpc (runGrpcServer)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race_)
import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Exception (bracket_)
import Control.Monad (forever)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import GHC.Stats (GCDetails (..), RTSStats (..), getRTSStats, getRTSStatsEnabled)
import Network.GRPC.Common (NextElem (..))
import Types.Api (ExecutorStatus (..))
import qualified Data.Aeson as Aeson
import Data.ByteString (toStrict)
import Network.GRPC.Common.Protobuf (Proto, defMessage, (&), (.~))
import Network.GRPC.Server.Protobuf (ProtobufMethodsOf)
import Network.GRPC.Server.StreamType (Methods, mkNonStreaming, mkServerStreaming, simpleMethods)
import Proto.Executor (Empty, Encoded, Executor)
import qualified Proto.Executor_Fields as Fields
import qualified System.IO as IO
import System.OsPath.Extra (OsPath)
import GhcServer.Build.ExecutorLoad (PlanSessionVar, executePlan, newPlanSessionVar)
import GhcServer.Build.ProcessChild (configInvalidResult, evalResult, finishResult)
import GhcServer.Data.ExecutorPlan (PlanRequest)
import GhcServer.Data.ProcessEval (ExecutorCall (..), ProcessEvalResult)
import Data.Text (pack)
import System.IO.Silently (capture, hCapture)
import Test.Log (newTestLog)
import Internal.State (newState)
import Types.Settings (defaultSettings)
import Types.State (WorkerState)

-- | Handle one @Execute@ call: decode the 'GhcServer.Data.ProcessEval.ExecutorCall', evaluate it against the
-- executor's persistent state while holding the lock, and encode the 'GhcServer.Data.ProcessEval.ProcessEvalResult'.
executeCall :: IORef Int -> MVar () -> MVar WorkerState -> PlanSessionVar -> Proto Encoded -> IO (Proto Encoded)
executeCall active lock stateVar planVar request = do
  -- Counted outside the lock, so that calls waiting for it are included.
  result <- bracket_ (bump 1) (bump (-1)) $ withMVar lock \ () ->
    either configInvalidResult dispatch (Aeson.eitherDecodeStrict' request.payload)
  pure (defMessage & Fields.payload .~ toStrict (Aeson.encode result))
  where
    bump n = atomicModifyIORef' active \ count -> (count + n, ())

    dispatch = \case
      ExecuteModule config -> evalResult stateVar config
      ExecutePlan plan -> planResult planVar plan

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

-- | Run a bytecode plan, capturing stdout and stderr like 'evalResult'.
planResult :: PlanSessionVar -> PlanRequest -> IO ProcessEvalResult
planResult planVar plan = do
  (logger, _) <- newTestLog
  (evalStderr, (evalStdout, outcome)) <- hCapture [IO.stderr] (capture (executePlan planVar plan))
  finishResult logger (pack evalStdout) (pack evalStderr) outcome

-- | Block until stdin is closed by the parent.
awaitStdinEof :: IO ()
awaitStdinEof =
  IO.hIsEOF IO.stdin >>= \case
    True -> pure ()
    False -> IO.hGetLine IO.stdin >> awaitStdinEof

-- | Entry point of the @executor@ CLI mode: serve the @Executor@ service on the given socket until stdin closes.
runExecutor :: OsPath -> IO ()
runExecutor socket = do
  stateVar <- newState defaultSettings
  planVar <- newPlanSessionVar
  lock <- newMVar ()
  active <- newIORef 0
  race_ awaitStdinEof (runGrpcServer socket (executorMethods active lock stateVar planVar))

-- | The generated method list is alphabetical: @events@, @execute@.
executorMethods :: IORef Int -> MVar () -> MVar WorkerState -> PlanSessionVar -> Methods IO (ProtobufMethodsOf Executor)
executorMethods active lock stateVar planVar =
  simpleMethods
    (mkServerStreaming (statusEvents active))
    (mkNonStreaming (executeCall active lock stateVar planVar))
