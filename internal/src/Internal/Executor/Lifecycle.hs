-- | Lifecycle management shared by executor hosts ('GhcServer.Build.Executor') for persistent executor subprocesses:
-- lazy spawning, reuse, respawning after failures
-- and the executor's socket name.
module Internal.Executor.Lifecycle where

import Control.Concurrent.MVar (MVar)
import Types.Api (Event, ExecutorId (..))
import Types.Executor.Transport (ExecutorTransport)
import Types.State (WorkerState (..))
import Types.State.Executor (ExecutorHandle (..))

-- | What the lifecycle functions need from their host: the worker state, the sink for status events, the transport
-- for observing executors and the way to launch a new executor process for an id.
data ExecutorEnv =
  ExecutorEnv {
    stateVar :: MVar WorkerState,
    emit :: Event -> IO (),
    transport :: ExecutorTransport,
    spawn :: ExecutorId -> IO ExecutorHandle
  }
