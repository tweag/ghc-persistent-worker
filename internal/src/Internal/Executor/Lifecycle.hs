-- | Lifecycle management for executor processes.
module Internal.Executor.Lifecycle where

import Control.Concurrent.MVar (MVar)
import Data.Char (isAlphaNum)
import qualified Data.Text as Text
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

-- | The file name of the socket of the executor with the given id, without directory.
-- The id is sanitized, since it is chosen by the client.
--
-- TODO OsPath
executorSocketName :: ExecutorId -> String
executorSocketName executorId =
  "executor-" ++ [if isAlphaNum c || c == '-' then c else '_' | c <- Text.unpack executorId.text] ++ ".sock"
