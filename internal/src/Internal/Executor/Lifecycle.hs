-- | Lifecycle management shared by executor hosts ('GhcServer.Build.Executor') for persistent executor subprocesses:
-- lazy spawning, reuse, respawning after failures
-- and the executor's socket name.
module Internal.Executor.Lifecycle where

import Control.Concurrent.MVar (MVar, modifyMVar, readMVar)
import Control.Monad (when)
import Data.Char (isAlphaNum)
import Data.Foldable (traverse_)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Internal.Executor.Client (awaitExecutor, isRunning, stopExecutor, watchExecutor)
import Types.Api (Event, ExecutorId (..))
import Types.Executor.Transport (ExecutorTransport)
import Types.State (WorkerState (..))
import Types.State.Executor (ExecutorHandle (..), withoutCalls)

-- | What the lifecycle functions need from their host: the worker state, the sink for status events, the transport
-- for observing executors and the way to launch a new executor process for an id.
data ExecutorEnv =
  ExecutorEnv {
    stateVar :: MVar WorkerState,
    emit :: Event -> IO (),
    transport :: ExecutorTransport,
    spawn :: ExecutorId -> IO ExecutorHandle
  }

-- | The file name of the socket of the executor with the given id, without directory.
-- The id is sanitized, since it is chosen by the client.
executorSocketName :: ExecutorId -> String
executorSocketName executorId =
  "executor-" ++ [if isAlphaNum c || c == '-' then c else '_' | c <- Text.unpack executorId.text] ++ ".sock"

-- | Return the running executor for the given id, spawning (or respawning, if it died) one if necessary, and wait
-- until it serves its socket.
ensureExecutor :: ExecutorEnv -> ExecutorId -> IO ExecutorHandle
ensureExecutor =
  acquireExecutor isRunning

-- | Replace the executor for the given id with a fresh process, e.g. when its session can't serve a request, and
-- wait until it serves its socket.
--
-- The incarnation is that of the handle the caller saw failing. If the registered executor is a newer, running one,
-- another task has replaced it already and it is kept, since stopping it would kill that task's calls.
-- Waits for calls in flight on the old process (see 'withoutCalls') before stopping it.
respawnExecutor :: ExecutorEnv -> ExecutorId -> Int -> IO ExecutorHandle
respawnExecutor env executorId failed =
  (Map.lookup executorId . (.executors) <$> readMVar env.stateVar) >>= \case
    Just handle -> withoutCalls handle replace
    Nothing -> replace
  where
    replace = acquireExecutor replaced env executorId
    replaced handle
      | handle.incarnation == failed = pure False
      | otherwise = isRunning handle

-- | Return the registered executor if the predicate accepts it, otherwise stop it and register a new one.
-- A new process is observed for status events, which are published with 'ExecutorEnv.emit'.
acquireExecutor :: (ExecutorHandle -> IO Bool) -> ExecutorEnv -> ExecutorId -> IO ExecutorHandle
acquireExecutor reuse env executorId = do
  (handle, spawned) <- modifyMVar env.stateVar \ state -> do
    let existing = Map.lookup executorId state.executors
    reusable <- maybe (pure False) reuse existing
    case existing of
      Just handle | reusable -> pure (state, (handle, False))
      _ -> do
        traverse_ stopExecutor existing
        spawned <- env.spawn executorId
        -- Keep the call lock, so that callers still holding it stay in sync with the new process.
        let handle = maybe spawned (\ old -> spawned {calls = old.calls, incarnation = old.incarnation + 1}) existing
        pure (state {executors = Map.insert executorId handle state.executors}, (handle, True))
  awaitExecutor handle
  when spawned (watchExecutor env.transport env.emit env.stateVar executorId handle)
  pure handle
