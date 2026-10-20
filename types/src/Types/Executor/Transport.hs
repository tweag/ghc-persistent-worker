-- | The abstraction over the channel between a parent and its executor subprocesses.
--
-- The generic executor code in @internal@ only talks to executors through this record, so that it doesn't depend on
-- the gRPC libraries. The gRPC implementation lives in @Common.Grpc.Executor.Client@.
module Types.Executor.Transport where

import System.OsPath (OsPath)
import Types.Api (RequestId)
import Types.Executor.Protocol (CallError, CallResult, ExecutorCall, ExecutorEvent)

data ExecutorTransport =
  ExecutorTransport {
    -- | Send one call to the executor serving the socket, on behalf of the task with the given request id, to which
    -- the executor attributes the output written during the call.
    call :: OsPath -> RequestId -> ExecutorCall -> IO (Either CallError CallResult),
    -- | Pass the events that the executor serving the socket publishes to the callback until the stream ends.
    -- Throws if the executor can't be reached.
    subscribe :: OsPath -> (ExecutorEvent -> IO ()) -> IO ()
  }
