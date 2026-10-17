-- | Parent-side primitives for persistent, gRPC-addressable executor subprocesses (see
-- 'Types.State.WorkerState.executors'): spawning the relaunch of an executable in @executor@ mode, waiting for its
-- socket, sending calls and stopping it.
--
-- Used by the @ghc-server@ ('GhcServer.Build.Executor') and by the worker's eval mode ('GhcWorker.Eval.Executor').
module Internal.Executor.Client where

import BuckWorkerProto ()
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (MVar, readMVar)
import Control.Exception (SomeException, displayException, throwIO, try)
import Control.Monad (unless, void, when)
import qualified Data.Aeson as Aeson
import Data.Bifunctor (first)
import Data.ByteString (toStrict)
import Data.Foldable (traverse_)
import Data.Functor ((<&>))
import qualified Data.Map.Strict as Map
import Data.Text (pack)
import Network.GRPC.Client (Server (..), rpc, withConnection)
import Network.GRPC.Client.StreamType.IO (nonStreaming, serverStreaming)
import Network.GRPC.Common (NextElem (..), def)
import Network.GRPC.Common.Protobuf (Protobuf, defMessage, (&), (.~))
import Proto.Executor (Executor)
import qualified Proto.Executor_Fields as Fields
import System.Directory.OsPath (doesFileExist, removeFile)
import System.IO (hClose)
import System.OsPath.Extra (OsPath, fromOsPath)
import System.Process (
  CreateProcess (..),
  StdStream (..),
  createProcess,
  getPid,
  getProcessExitCode,
  proc,
  terminateProcess,
  waitForProcess,
  )
import Types.Api (Event (..), ExecutorId, RequestId)
import Types.Executor.Protocol (CallError (..), CallResult, ExecutorCall, ExecutorEvent (..), ExecutorRequest (..), OutputLine (..))
import Types.Settings (Settings (..))
import Types.State (WorkerState (..))
import Types.State.Executor (ExecutorHandle (..), emptyExecutorLoaded, newCallLock)

-- | Wrap RTS options in @+RTS ... -RTS@, or nothing if there are none.
rtsArgs :: [String] -> [String]
rtsArgs = \case
  [] -> []
  opts -> ["+RTS"] ++ opts ++ ["-RTS"]

-- | Argument that makes an executor attribute its stdout and stderr to the calls, see
-- 'Types.Settings.Settings.executorCaptureStdio'.
captureStdioArg :: String
captureStdioArg = "--capture-stdio"

-- | The extra executor arguments for the settings, to be passed to 'spawnExecutorWith'.
captureStdioArgs :: Settings -> [String]
captureStdioArgs settings =
  [captureStdioArg | settings.executorCaptureStdio]

-- | Launch the given executable as an executor child on the socket, with extra arguments (e.g. RTS options).
spawnExecutorWith :: FilePath -> [String] -> OsPath -> IO ExecutorHandle
spawnExecutorWith exe extraArgs socket = do
  removeIfExists socket
  calls <- newCallLock
  (mStdin, _, _, process) <-
    createProcess (proc exe (["executor", "--socket", fromOsPath socket] ++ extraArgs)) {std_in = CreatePipe}
  case mStdin of
    Just stdin -> pure ExecutorHandle {process, socket, stdin, loaded = emptyExecutorLoaded, stats = Nothing, calls, incarnation = 0}
    Nothing -> throwIO (userError "Executor was spawned without a stdin pipe")

removeIfExists :: OsPath -> IO ()
removeIfExists path =
  void (try @SomeException (removeFile path))

isRunning :: ExecutorHandle -> IO Bool
isRunning handle =
  null <$> getProcessExitCode handle.process

-- | Poll until the executor has bound its socket, failing if it exits beforehand or takes longer than 10 seconds.
awaitExecutor :: ExecutorHandle -> IO ()
awaitExecutor handle =
  check (100 :: Int)
  where
    check 0 = throwIO (userError "Executor did not open its socket within 10 seconds")
    check n =
      isRunning handle >>= \case
        False -> throwIO (userError "Executor exited during startup")
        True ->
          doesFileExist handle.socket >>= \case
            True -> pure ()
            False -> threadDelay 100_000 >> check (n - 1)

-- | Send one call to an executor via its @Execute@ RPC, as JSON.
-- The request id is the task on whose behalf the call is made, to which the executor attributes the output.
callExecutorJson :: ExecutorHandle -> RequestId -> ExecutorCall -> IO (Either CallError CallResult)
callExecutorJson handle requestId call =
  try @SomeException exchange <&> \case
    Left err -> Left (Unreachable (pack (displayException err)))
    Right result -> first (Undecodable . pack) result
  where
    exchange =
      withConnection def (ServerUnix (fromOsPath handle.socket)) \ connection ->
        Aeson.eitherDecodeStrict' . (.payload) <$> nonStreaming connection (rpc @(Protobuf Executor "execute")) message

    message = defMessage & Fields.payload .~ toStrict (Aeson.encode ExecutorRequest {requestId, call})

-- | Close the executor's stdin (which makes it exit on its own), terminate it for good measure, reap it and remove
-- its socket.
stopExecutor :: ExecutorHandle -> IO ()
stopExecutor handle = do
  void (try @SomeException (hClose handle.stdin))
  terminateProcess handle.process
  void (waitForProcess handle.process)
  removeIfExists handle.socket

-- | Forward the status snapshots and the output an executor publishes via its @Events@ RPC as 'ExecutorUpdate' and
-- 'ExecutorOutput' events, and publish
-- 'ExecutorGone' when the stream ends, unless a newer process has been registered for the id in the meantime.
--
-- The watcher is never cancelled: stopping the process ends the stream, which ends the watcher.
-- (Cancelling a thread blocked in a gRPC read would block until the connection drops.)
watchExecutor :: (Event -> IO ()) -> MVar WorkerState -> ExecutorId -> ExecutorHandle -> IO ()
watchExecutor emit stateVar executorId handle =
  void $ forkIO do
    pid <- fmap fromIntegral <$> getPid handle.process
    subscribe pid (20 :: Int)
    announceGone
  where
    -- The socket file exists slightly before the child accepts connections, so connecting is retried.
    subscribe pid retries =
      try @SomeException (streamStatus pid) >>= \case
        Left _ | retries > 0 -> do
          running <- isRunning handle
          when running (threadDelay 100_000 >> subscribe pid (retries - 1))
        _ -> pure ()

    streamStatus pid =
      withConnection def (ServerUnix (fromOsPath handle.socket)) \ connection ->
        serverStreaming connection (rpc @(Protobuf Executor "events")) defMessage (receiveAll pid)

    receiveAll pid recv =
      recv >>= \case
        NextElem message -> do
          traverse_ (publish pid) (Aeson.decodeStrict' message.payload)
          receiveAll pid recv
        NoNextElem -> pure ()

    publish pid = \case
      StatusEvent status -> emit ExecutorUpdate {executor = executorId, pid, status}
      OutputEvent OutputLine {task, stream, line} -> emit ExecutorOutput {executor = executorId, task, stream, line}

    announceGone = do
      current <- Map.lookup executorId . (.executors) <$> readMVar stateVar
      unless (any (\ newer -> newer.incarnation > handle.incarnation) current) $
        emit ExecutorGone {executor = executorId}
