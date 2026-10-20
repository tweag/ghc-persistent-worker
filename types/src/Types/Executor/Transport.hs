-- | The abstraction over the channel between a GHC server and an executor process.
module Types.Executor.Transport where

import System.OsPath (OsPath)
import Types.Api (RequestId)
import Types.Executor.Protocol (CallError, CallResult, ExecutorCall, ExecutorEvent)

-- | Handlers that implement a concrete interface for an executor, like a gRPC server.
data ExecutorTransport =
  ExecutorTransport {
    -- | Send one call to the executor serving the socket, on behalf of the task with the given request id, to which
    -- the executor attributes the output written during the call.
    --
    -- TODO Rename to @handleRequest@ or something, add record for arguments
    call :: OsPath -> RequestId -> ExecutorCall -> IO (Either CallError CallResult),
    -- | Pass the events that the executor serving the socket publishes to the callback until the stream ends.
    -- Throws if the executor can't be reached.
    --
    -- TODO Rename to @events@
    subscribe :: OsPath -> (ExecutorEvent -> IO ()) -> IO ()
  }
