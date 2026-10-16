-- | Client access to the event stream and API of the 'Worker' service, served by @ghc-server@ and @ghc-worker@.
module BuckWorkerProto.Protocol where

import BuckWorkerProto ()
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class (MonadIO)
import Data.ByteString (ByteString)
import Network.GRPC.Client (Connection, rpc)
import Network.GRPC.Client.StreamType.IO (nonStreaming, serverStreaming)
import Network.GRPC.Common (NextElem)
import Network.GRPC.Common.Protobuf (Protobuf, defMessage, (&), (.~))
import Proto.Worker (Worker)
import Proto.Worker_Fields qualified as Fields

-- | Subscribe to the @Events@ stream, passing a function that reads the next event's payload to the continuation.
streamEvents ::
  MonadIO m =>
  MonadMask m =>
  Connection ->
  (m (NextElem ByteString) -> m r) ->
  m r
streamEvents connection use =
  serverStreaming connection (rpc @(Protobuf Worker "events")) defMessage \ recv ->
    use (fmap (.payload) <$> recv)

-- | Send an encoded request to the @Api@ method and return the encoded response.
apiRequest ::
  Connection ->
  ByteString ->
  IO ByteString
apiRequest connection payload =
  (.payload) <$> nonStreaming connection (rpc @(Protobuf Worker "api")) (defMessage & Fields.payload .~ payload)
