-- | Lifecycle management for executor processes.
module Internal.Executor.Lifecycle where

import Control.Concurrent.MVar (MVar)
import Types.Api (Event, ExecutorId (..))
import Types.Executor.Transport (ExecutorTransport)
import Types.State (WorkerState (..))
import Types.State.Executor (ExecutorHandle (..))

-- | Resources for executor lifecycle management.
data ExecutorEnv =
  ExecutorEnv {
    stateVar :: MVar WorkerState,
    publishEvent :: Event -> IO (),
    transport :: ExecutorTransport,
    spawn :: ExecutorId -> IO ExecutorHandle
  }
