-- | Parent-side lifecycle management for persistent, gRPC-addressable executor subprocesses (see
-- 'Types.State.WorkerState.executors' and 'Types.Api.ExecutorId').
--
-- An executor is a relaunch of the running executable in @executor@ mode ('GhcServer.Build.ExecutorChild'),
-- serving the @Executor@ gRPC service (@proto\/executor.proto@) on a Unix socket under the project's socket
-- directory. It is spawned lazily by the first execute task dispatched to its id, reused by all subsequent ones
-- (keeping its GHC session state warm), and lives until 'terminateExecutor' is called or the parent exits (the
-- child exits when its stdin pipe is closed). An executor that died is respawned transparently on the next call.
module GhcServer.Build.Executor where

import BuckWorkerProto ()
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (modifyMVar)
import Control.Exception (SomeException, throwIO, try)
import Control.Monad (void)
import qualified Data.Aeson as Aeson
import Data.Bifunctor (first)
import Data.ByteString (toStrict)
import Data.Char (isAlphaNum)
import Data.Foldable (traverse_)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Data.Text (pack)
import qualified Data.Text as Text
import qualified GHC
import GhcServer.Build.Process (executeModuleTaskWith)
import GhcServer.Data.BuildEnv (BuildEnv (..))
import GhcServer.Data.ProcessEval (ProcessEvalConfig, ProcessEvalOutput (..))
import GhcServer.Data.Unit (Unit)
import GhcServer.Path (socketDirName)
import Network.GRPC.Client (Server (..), rpc, withConnection)
import Network.GRPC.Client.StreamType.IO (nonStreaming)
import Network.GRPC.Common (def)
import Network.GRPC.Common.Protobuf (Protobuf, defMessage, (&), (.~))
import Proto.Executor (Executor)
import qualified Proto.Executor_Fields as Fields
import System.Directory.OsPath (createDirectoryIfMissing, doesFileExist, removeFile)
import System.Environment (getExecutablePath)
import System.IO (hClose)
import System.OsPath.Extra (OsPath, fromOsPath, toOsPath, (</>))
import System.Process (
  CreateProcess (..),
  StdStream (..),
  createProcess,
  getProcessExitCode,
  proc,
  terminateProcess,
  waitForProcess,
  )
import Test.Scheduler (TaskResult)
import Types.Api (ExecutorId (..), ProcessStats)
import Types.State (WorkerState (..))
import Types.State.Executor (ExecutorHandle (..))

-- | The socket an executor serves on. The id is sanitized, since it is chosen by the client.
executorSocket :: BuildEnv -> ExecutorId -> OsPath
executorSocket env executorId =
  env.projectRoot </> socketDirName </> toOsPath ("executor-" ++ sanitized ++ ".sock")
  where
    sanitized = [if isAlphaNum c || c == '-' then c else '_' | c <- Text.unpack executorId.text]

-- | Launch a new executor child, without waiting for it to become ready.
spawnExecutor :: BuildEnv -> ExecutorId -> IO ExecutorHandle
spawnExecutor env executorId = do
  self <- getExecutablePath
  createDirectoryIfMissing True (env.projectRoot </> socketDirName)
  removeIfExists socket
  (mStdin, _, _, process) <-
    createProcess (proc self ["executor", "--socket", fromOsPath socket]) {std_in = CreatePipe}
  case mStdin of
    Just stdin -> pure ExecutorHandle {process, socket, stdin}
    Nothing -> throwIO (userError "Executor was spawned without a stdin pipe")
  where
    socket = executorSocket env executorId

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

-- | Return the running executor for the given id, spawning (or respawning, if it died) one if necessary, and wait
-- until it serves its socket.
ensureExecutor :: BuildEnv -> ExecutorId -> IO ExecutorHandle
ensureExecutor env executorId = do
  handle <- modifyMVar env.stateVar \ state -> do
    let existing = Map.lookup executorId state.executors
    running <- maybe (pure False) isRunning existing
    case existing of
      Just handle | running -> pure (state, handle)
      _ -> do
        traverse_ stopExecutor existing
        handle <- spawnExecutor env executorId
        pure (state {executors = Map.insert executorId handle state.executors}, handle)
  handle <$ awaitExecutor handle

-- | Send one evaluation to an executor via its @Execute@ RPC.
callExecutor :: ExecutorHandle -> ProcessEvalConfig -> IO ProcessEvalOutput
callExecutor handle config =
  withConnection def (ServerUnix (fromOsPath handle.socket)) \ connection -> do
    output <- nonStreaming connection (rpc @(Protobuf Executor "execute")) message
    pure ProcessEvalOutput {
      result = first decodeError (Aeson.eitherDecodeStrict' output.payload),
      processStderr = ""
    }
  where
    message = defMessage & Fields.payload .~ toStrict (Aeson.encode config)

    decodeError err = "Could not decode the executor result: " <> pack err

-- | Run a module's execute task via the persistent executor identified by 'ExecutorId', spawning it on first use.
-- Failures to spawn or reach the executor are reported as a failed task by 'executeModuleTaskWith'.
executeModuleTaskExecutor ::
  BuildEnv ->
  ExecutorId ->
  Unit ->
  GHC.ModuleName ->
  IO (Maybe (TaskResult String), Maybe ProcessStats)
executeModuleTaskExecutor env executorId =
  executeModuleTaskWith (\ config -> ensureExecutor env executorId >>= \ h -> callExecutor h config) env

-- | Close the executor's stdin (which makes it exit on its own), terminate it for good measure, reap it and remove
-- its socket.
stopExecutor :: ExecutorHandle -> IO ()
stopExecutor handle = do
  void (try @SomeException (hClose handle.stdin))
  terminateProcess handle.process
  void (waitForProcess handle.process)
  removeIfExists handle.socket

-- | Terminate the persistent executor subprocess identified by 'ExecutorId', if one is running, and remove it
-- from 'WorkerState.executors'. Returns whether an executor with that id was registered.
terminateExecutor :: BuildEnv -> ExecutorId -> IO Bool
terminateExecutor env executorId = do
  handle <- modifyMVar env.stateVar \ state ->
    pure (state {executors = Map.delete executorId state.executors}, Map.lookup executorId state.executors)
  traverse_ stopExecutor handle
  pure (isJust handle)
