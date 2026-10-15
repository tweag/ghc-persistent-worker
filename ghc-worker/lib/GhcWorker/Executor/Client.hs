-- | Parent-side primitives for persistent, gRPC-addressable executor subprocesses (see
-- 'Types.State.WorkerState.executors'): spawning the relaunch of an executable in @executor@ mode, waiting for its
-- socket, sending calls and stopping it.
--
-- Used by the @ghc-server@ ('GhcServer.Build.Executor') and by the worker's eval mode ('GhcWorker.Executor.Plan').
module GhcWorker.Executor.Client where

import BuckWorkerProto ()
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (newMVar)
import Control.Exception (SomeException, throwIO, try)
import Control.Monad (void)
import Data.Aeson (ToJSON)
import qualified Data.Aeson as Aeson
import Data.Bifunctor (first)
import Data.ByteString (toStrict)
import Data.Text (pack)
import Network.GRPC.Client (Server (..), rpc, withConnection)
import Network.GRPC.Client.StreamType.IO (nonStreaming)
import Network.GRPC.Common (def)
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
  getProcessExitCode,
  proc,
  terminateProcess,
  waitForProcess,
  )
import Types.ProcessEval (ProcessEvalOutput (..))
import Types.State.Executor (ExecutorHandle (..), emptyExecutorLoaded)

-- | Wrap RTS options in @+RTS ... -RTS@, or nothing if there are none.
rtsArgs :: [String] -> [String]
rtsArgs = \case
  [] -> []
  opts -> ["+RTS"] ++ opts ++ ["-RTS"]

-- | Launch the given executable as an executor child on the socket, with extra arguments (e.g. RTS options).
spawnExecutorWith :: FilePath -> [String] -> OsPath -> IO ExecutorHandle
spawnExecutorWith exe extraArgs socket = do
  removeIfExists socket
  calls <- newMVar ()
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

-- | Send one JSON-encoded call to an executor via its @Execute@ RPC.
callExecutorJson :: ToJSON a => ExecutorHandle -> a -> IO ProcessEvalOutput
callExecutorJson handle call =
  withConnection def (ServerUnix (fromOsPath handle.socket)) \ connection -> do
    output <- nonStreaming connection (rpc @(Protobuf Executor "execute")) message
    pure ProcessEvalOutput {
      result = first decodeError (Aeson.eitherDecodeStrict' output.payload),
      processStderr = ""
    }
  where
    message = defMessage & Fields.payload .~ toStrict (Aeson.encode call)

    decodeError err = "Could not decode the executor result: " <> pack err

-- | Close the executor's stdin (which makes it exit on its own), terminate it for good measure, reap it and remove
-- its socket.
stopExecutor :: ExecutorHandle -> IO ()
stopExecutor handle = do
  void (try @SomeException (hClose handle.stdin))
  terminateProcess handle.process
  void (waitForProcess handle.process)
  removeIfExists handle.socket
