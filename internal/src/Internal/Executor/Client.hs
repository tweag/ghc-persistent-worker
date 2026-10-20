-- | Parent-side primitives for persistent, gRPC-addressable executor subprocesses (see
-- 'Types.State.WorkerState.executors'): spawning the relaunch of an executable in @executor@ mode, waiting for its
-- socket, sending calls and stopping it.
--
-- Used by the @ghc-server@ ('GhcServer.Build.Executor') and by the worker's eval mode ('GhcWorker.Eval.Executor').
module Internal.Executor.Client where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (MVar, readMVar)
import Control.Exception (SomeException, throwIO, try)
import Control.Monad (unless, void, when)
import qualified Data.Map.Strict as Map
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
import Types.Api (Event (..), ExecutorId)
import Types.Executor.Protocol (ExecutorEvent (..), OutputLine (..))
import Types.Executor.Transport (ExecutorTransport (..))
import Types.Settings (Settings (..))
import Types.State (WorkerState (..))
import Types.State.Executor (ExecutorHandle (..), newCallLock)

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
    Just stdin -> pure ExecutorHandle {process, socket, stdin, stats = Nothing, calls, incarnation = 0}
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

-- | Close the executor's stdin (which makes it exit on its own), terminate it for good measure, reap it and remove
-- its socket.
stopExecutor :: ExecutorHandle -> IO ()
stopExecutor handle = do
  void (try @SomeException (hClose handle.stdin))
  terminateProcess handle.process
  void (waitForProcess handle.process)
  removeIfExists handle.socket

-- | Forward the status snapshots and the output an executor publishes via the transport as 'ExecutorUpdate' and
-- 'ExecutorOutput' events, and publish
-- 'ExecutorGone' when the stream ends, unless a newer process has been registered for the id in the meantime.
--
-- The watcher is never cancelled: stopping the process ends the stream, which ends the watcher.
-- (Cancelling a thread blocked in a gRPC read would block until the connection drops.)
watchExecutor :: ExecutorTransport -> (Event -> IO ()) -> MVar WorkerState -> ExecutorId -> ExecutorHandle -> IO ()
watchExecutor transport emit stateVar executorId handle =
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
      transport.subscribe handle.socket (publish pid)

    publish pid = \case
      StatusEvent status -> emit ExecutorUpdate {executor = executorId, pid, status}
      OutputEvent OutputLine {task, stream, line} -> emit ExecutorOutput {executor = executorId, task, stream, line}

    announceGone = do
      current <- Map.lookup executorId . (.executors) <$> readMVar stateVar
      unless (any (\ newer -> newer.incarnation > handle.incarnation) current) $
        emit ExecutorGone {executor = executorId}
