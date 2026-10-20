-- | The gRPC implementation of 'ExecutorTransport', speaking the @Executor@ service (@proto\/executor.proto@) to an
-- executor subprocess over its Unix socket.
module Common.Grpc.Executor.Client where

import BuckWorkerProto ()
import Control.Exception (SomeException, displayException, try)
import qualified Data.Aeson as Aeson
import Data.Bifunctor (first)
import Data.ByteString (toStrict)
import Data.Foldable (traverse_)
import Data.Functor ((<&>))
import Data.Text (pack)
import Network.GRPC.Client (Server (..), rpc, withConnection)
import Network.GRPC.Client.StreamType.IO (nonStreaming, serverStreaming)
import Network.GRPC.Common (NextElem (..), def)
import Network.GRPC.Common.Protobuf (Protobuf, defMessage, (&), (.~))
import Proto.Executor (Executor)
import qualified Proto.Executor_Fields as Fields
import System.OsPath.Extra (OsPath, fromOsPath)
import Types.Api (RequestId)
import Types.Executor.Protocol (CallError (..), CallResult, ExecutorCall, ExecutorEvent, ExecutorRequest (..))

-- | Send one call to an executor via its @Execute@ RPC, as JSON.
callExecutorJson :: OsPath -> RequestId -> ExecutorCall -> IO (Either CallError CallResult)
callExecutorJson socket requestId executorCall =
  try @SomeException exchange <&> \case
    Left err -> Left (Unreachable (pack (displayException err)))
    Right result -> first (Undecodable . pack) result
  where
    exchange =
      withConnection def (ServerUnix (fromOsPath socket)) \ connection ->
        Aeson.eitherDecodeStrict' . (.payload) <$> nonStreaming connection (rpc @(Protobuf Executor "execute")) message

    message = defMessage & Fields.payload .~ toStrict (Aeson.encode ExecutorRequest {requestId, call = executorCall})

-- | Pass the elements of an executor's @Events@ stream to the callback until the stream ends.
subscribeEvents :: OsPath -> (ExecutorEvent -> IO ()) -> IO ()
subscribeEvents socket publish =
  withConnection def (ServerUnix (fromOsPath socket)) \ connection ->
    serverStreaming connection (rpc @(Protobuf Executor "events")) defMessage receiveAll
  where
    receiveAll recv =
      recv >>= \case
        NextElem message -> do
          traverse_ publish (Aeson.decodeStrict' message.payload)
          receiveAll recv
        NoNextElem -> pure ()
