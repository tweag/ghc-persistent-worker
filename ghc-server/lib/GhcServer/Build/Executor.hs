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
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (modifyMVar, modifyMVar_, newMVar, readMVar, withMVar)
import Control.Exception (SomeException, throwIO, try)
import Control.Monad (unless, void, when)
import qualified Data.Aeson as Aeson
import Data.Bifunctor (first)
import Data.ByteString (toStrict)
import Data.Char (isAlphaNum)
import Data.Foldable (traverse_)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Data.Text (pack)
import qualified Data.Text as Text
import GhcServer.Data.BuildEnv (BuildEnv (..))
import GhcServer.Data.ProcessEval (ExecutorCall (..), ProcessEvalConfig, ProcessEvalOutput (..))
import GhcServer.Log (emitEvent)
import GhcServer.Path (socketDirName)
import Network.GRPC.Client (Server (..), rpc, withConnection)
import Network.GRPC.Client.StreamType.IO (nonStreaming, serverStreaming)
import Network.GRPC.Common (NextElem (..), def)
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
  getPid,
  getProcessExitCode,
  proc,
  terminateProcess,
  waitForProcess,
  )
import Types.Api (Event (..), ExecutorId (..), ProcessStats (..))
import Types.State (WorkerState (..))
import Types.State.Executor (ExecutorHandle (..), emptyExecutorLoaded)

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
  spawnExecutorWith self (rtsArgs env.executorRts) (executorSocket env executorId)

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

-- | Return the running executor for the given id, spawning (or respawning, if it died) one if necessary, and wait
-- until it serves its socket.
ensureExecutor :: BuildEnv -> ExecutorId -> IO ExecutorHandle
ensureExecutor =
  acquireExecutor isRunning

-- | Replace the executor for the given id with a fresh process, e.g. when its session can't serve a request, and
-- wait until it serves its socket.
--
-- The incarnation is that of the handle the caller saw failing. If the registered executor is a newer, running one,
-- another task has replaced it already and it is kept, since stopping it would kill that task's calls.
-- Waits for calls in flight on the old process (see 'ExecutorHandle.calls') before stopping it.
respawnExecutor :: BuildEnv -> ExecutorId -> Int -> IO ExecutorHandle
respawnExecutor env executorId failed =
  (Map.lookup executorId . (.executors) <$> readMVar env.stateVar) >>= \case
    Just handle -> withMVar handle.calls \ () -> replace
    Nothing -> replace
  where
    replace = acquireExecutor replaced env executorId
    replaced handle
      | handle.incarnation == failed = pure False
      | otherwise = isRunning handle

-- | Return the registered executor if the predicate accepts it, otherwise stop it and register a new one.
acquireExecutor :: (ExecutorHandle -> IO Bool) -> BuildEnv -> ExecutorId -> IO ExecutorHandle
acquireExecutor reuse env executorId = do
  (handle, spawned) <- modifyMVar env.stateVar \ state -> do
    let existing = Map.lookup executorId state.executors
    reusable <- maybe (pure False) reuse existing
    case existing of
      Just handle | reusable -> pure (state, (handle, False))
      _ -> do
        traverse_ stopExecutor existing
        spawned <- spawnExecutor env executorId
        -- Keep the call lock, so that callers still holding it stay serialized with the new process.
        let handle = maybe spawned (\ old -> spawned {calls = old.calls, incarnation = old.incarnation + 1}) existing
        pure (state {executors = Map.insert executorId handle state.executors}, (handle, True))
  awaitExecutor handle
  when spawned (watchExecutor env executorId handle)
  pure handle

-- | Forward the status snapshots an executor publishes via its @Events@ RPC as 'ExecutorUpdate' events, and emit
-- 'ExecutorGone' when the stream ends, unless a newer process has been registered for the id in the meantime.
--
-- The watcher is never cancelled: stopping the process ends the stream, which ends the watcher.
-- (Cancelling a thread blocked in a gRPC read would block until the connection drops.)
watchExecutor :: BuildEnv -> ExecutorId -> ExecutorHandle -> IO ()
watchExecutor env executorId handle =
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

    publish pid status = emitEvent env.instrChan ExecutorUpdate {executor = executorId, pid, status}

    announceGone = do
      current <- Map.lookup executorId . (.executors) <$> readMVar env.stateVar
      unless (any (\ newer -> newer.incarnation > handle.incarnation) current) $
        emitEvent env.instrChan ExecutorGone {executor = executorId}

-- | Send one evaluation to an executor via its @Execute@ RPC.
callExecutor :: ExecutorHandle -> ProcessEvalConfig -> IO ProcessEvalOutput
callExecutor handle config =
  callExecutorWith handle (ExecuteModule config)

-- | Send one 'ExecutorCall' to an executor via its @Execute@ RPC.
callExecutorWith :: ExecutorHandle -> ExecutorCall -> IO ProcessEvalOutput
callExecutorWith handle call =
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

-- | Terminate the persistent executor subprocess identified by 'ExecutorId', if one is running, and remove it
-- from 'WorkerState.executors'. Returns whether an executor with that id was registered.
terminateExecutor :: BuildEnv -> ExecutorId -> IO Bool
terminateExecutor env executorId = do
  handle <- modifyMVar env.stateVar \ state ->
    pure (state {executors = Map.delete executorId state.executors}, Map.lookup executorId state.executors)
  traverse_ stopExecutor handle
  pure (isJust handle)

-- | Store the RTS stats an executor reported with a result in its handle.
recordExecutorStats :: BuildEnv -> ExecutorId -> ProcessStats -> IO ()
recordExecutorStats env executorId stats =
  modifyMVar_ env.stateVar \ state ->
    pure state {executors = Map.adjust (\ handle -> handle {stats = Just stats}) executorId state.executors}

-- | One line per registered executor with the RTS stats it reported most recently.
executorStatsReport :: BuildEnv -> IO [String]
executorStatsReport env =
  fmap (uncurry reportLine) . Map.toList . (.executors) <$> readMVar env.stateVar
  where
    reportLine executorId handle =
      "executor " ++ Text.unpack executorId.text ++ ": " ++ maybe "no stats" formatStats handle.stats

    formatStats stats =
      "max mem in use " ++ mebibytes stats.maxMemInUseBytes ++ ", max live " ++ mebibytes stats.maxLiveBytes

    mebibytes bytes = show (bytes `div` (1024 * 1024)) ++ " MiB"
