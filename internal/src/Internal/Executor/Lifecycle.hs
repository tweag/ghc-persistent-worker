-- | Lifecycle management for executor processes.
module Internal.Executor.Lifecycle where

import Control.Concurrent.MVar (MVar, modifyMVar, readMVar)
import Control.Monad (when)
import Data.Char (isAlphaNum)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Internal.Executor.Client (awaitExecutor, isRunning, stopExecutor, watchExecutor)
import Types.Api (Event, ExecutorId (..))
import Types.Executor.Transport (ExecutorTransport)
import Types.State (WorkerState (..))
import Types.State.Executor (ExecutorHandle (..), withoutCalls)

-- | Resources for executor lifecycle management.
data ExecutorEnv =
  ExecutorEnv {
    stateVar :: MVar WorkerState,
    publishEvent :: Event -> IO (),
    transport :: ExecutorTransport,
    spawn :: ExecutorId -> IO ExecutorHandle
  }

-- | The file name of the socket of the executor with the given id, without directory.
-- The id is sanitized, since it is chosen by the client.
--
-- TODO OsPath
executorSocketName :: ExecutorId -> String
executorSocketName executorId =
  "executor-" ++ [if isAlphaNum c || c == '-' then c else '_' | c <- Text.unpack executorId.text] ++ ".sock"

-- | Look up or spawn an executor.
--
-- If the id doesn't exist in the state, start a new executor process.
-- If the id already exists in the state:
-- - If @canReuse@ returns 'True', return it.
-- - Otherwise, stop the existing executor and start a new one.
acquireExecutor :: (ExecutorHandle -> IO Bool) -> ExecutorEnv -> ExecutorId -> IO ExecutorHandle
acquireExecutor canReuse env executorId = do
  (handle, spawned) <- modifyMVar env.stateVar \ state ->
    maybe (spawn id) reuse (Map.lookup executorId state.executors) state
  awaitExecutor handle
  when spawned do
    -- TODO use ExecutorEnv in watchExecutor
    watchExecutor env.transport env.publishEvent env.stateVar executorId handle
  pure handle
  where
    reuse handle state =
      canReuse handle >>= \case
        True -> pure (state, (handle, False))
        False -> restart state handle

    -- Keep the call lock, so that callers still holding it stay in sync with the new process.
    restart state old = do
      stopExecutor old
      spawn (\ handle -> handle {calls = old.calls, incarnation = old.incarnation + 1}) state

    spawn update state = withHandle state . update =<< env.spawn executorId

    withHandle state handle =
      pure (state {executors = Map.insert executorId handle state.executors}, (handle, True))

-- | Look up or spawn an executor with the given id.
ensureExecutor :: ExecutorEnv -> ExecutorId -> IO ExecutorHandle
ensureExecutor =
  acquireExecutor isRunning

-- | Replace the executor for the given id with a fresh process and wait until it serves its socket.
--
-- The incarnation is that of the handle the caller saw failing. If the registered executor is a newer, running one,
-- another task has replaced it already and it is kept, since stopping it would kill that task's calls.
-- Waits for calls in flight on the old process (see 'withoutCalls') before stopping it.
--
-- TODO unclear, revisit later
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
