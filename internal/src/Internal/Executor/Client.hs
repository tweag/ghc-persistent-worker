-- | Logic for controlling executor processes.
module Internal.Executor.Client where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async)
import Control.Concurrent.MVar (MVar, readMVar)
import Control.Exception (SomeException, throwIO, try)
import Control.Monad (unless, void, when)
import Control.Monad.Extra (loopM)
import Data.Foldable (traverse_)
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

captureStdioArg :: String
captureStdioArg = "--capture-stdio"

-- | Additional CLI arguments for executors for the given settings.
captureStdioArgs :: Settings -> [String]
captureStdioArgs settings =
  [captureStdioArg | settings.executorCaptureStdio]

-- | Launch the given executable as an executor child on the socket, with extra arguments (e.g. RTS options).
--
-- TODO typed-process
spawnExecutorWith :: FilePath -> [String] -> OsPath -> IO ExecutorHandle
spawnExecutorWith exe extraArgs socket = do
  removeIfExists socket
  calls <- newCallLock
  (mStdin, _, _, process) <- createProcess (proc exe args) {std_in = CreatePipe}
  case mStdin of
    Just stdin -> pure ExecutorHandle {process, socket, stdin, memory = Nothing, calls, incarnation = 0}
    Nothing -> throwIO (userError "Executor was spawned without a stdin pipe")
  where
    args = ["executor", "--socket", fromOsPath socket] ++ extraArgs

-- TODO removePathForcibly?
removeIfExists :: OsPath -> IO ()
removeIfExists path =
  void (try @SomeException (removeFile path))

isRunning :: ExecutorHandle -> IO Bool
isRunning handle =
  null <$> getProcessExitCode handle.process

-- | Poll until the executor has bound its socket, failing if it exits beforehand or takes longer than 10 seconds.
awaitExecutor :: ExecutorHandle -> IO ()
awaitExecutor handle = do
  status <- loopM check (100 :: Int)
  traverse_ (throwIO . userError) status
  where
    check = \case
      0 ->
        pure (Right (Just "Executor did not open its socket within 10 seconds"))
      n ->
        isRunning handle >>= \case
          False ->
            pure (Right (Just "Executor exited during startup"))
          True ->
            doesFileExist handle.socket >>= \case
              True -> pure (Right Nothing)
              False -> Left (n - 1) <$ threadDelay 100_000

-- | Close the executor's stdin (which makes it exit on its own), terminate it for good measure, reap it and remove
-- its socket.
stopExecutor :: ExecutorHandle -> IO ()
stopExecutor handle = do
  void (try @SomeException (hClose handle.stdin))
  terminateProcess handle.process
  void (waitForProcess handle.process)
  removeIfExists handle.socket

-- | Run the event stream subscriber for an executor, transform received 'ExecutorEvent's to 'Event' and publish them
-- with the provided callback.
--
-- When the executor's stream terminates, publish 'ExecutorGone'.
--
-- TODO monitor Async handle?
watchExecutor :: ExecutorTransport -> (Event -> IO ()) -> MVar WorkerState -> ExecutorId -> ExecutorHandle -> IO ()
watchExecutor transport publish stateVar executorId handle =
  void $ async do
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
      transport.subscribe handle.socket \case
        StatusEvent status ->
          publish ExecutorUpdate {executor = executorId, pid, status}
        OutputEvent OutputLine {task, stream, line} ->
          publish ExecutorOutput {executor = executorId, task, stream, line}

    announceGone = do
      current <- Map.lookup executorId . (.executors) <$> readMVar stateVar
      unless (any (\ newer -> newer.incarnation > handle.incarnation) current) do
        publish ExecutorGone {executor = executorId}
