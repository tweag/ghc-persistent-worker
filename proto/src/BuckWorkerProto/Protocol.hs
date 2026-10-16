-- | Client access to the event stream and API of the two services that offer them.
--
-- @ghc-server@ sockets serve 'GhcServer', the instrumentation sockets of @ghc-worker@ serve 'Instrument'.
-- Both services have identical @Events@ and @Api@ methods, but the gRPC method paths contain the service name, so a
-- client using the wrong service gets an @UNIMPLEMENTED@ error on the first read.
module BuckWorkerProto.Protocol where

import BuckWorkerProto ()
import BuckWorkerProto.Data.Protocol (Protocol (..))
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class (MonadIO)
import Data.ByteString (ByteString)
import Network.GRPC.Client (Connection, rpc)
import Network.GRPC.Client.StreamType.IO (nonStreaming, serverStreaming)
import Network.GRPC.Common (NextElem)
import Network.GRPC.Common.Protobuf (Protobuf, defMessage, (&), (.~))
import Proto.GhcServer (GhcServer)
import Proto.GhcServer_Fields qualified as GhcServer
import Proto.Instrument (Instrument)
import Proto.Instrument_Fields qualified as Instrument

-- | Subscribe to the @Events@ stream, passing a function that reads the next event's payload to the continuation.
streamEvents ::
  MonadIO m =>
  MonadMask m =>
  Protocol ->
  Connection ->
  (m (NextElem ByteString) -> m r) ->
  m r
streamEvents protocol connection use =
  case protocol of
    ProtocolGhcServer ->
      serverStreaming connection (rpc @(Protobuf GhcServer "events")) defMessage \ recv ->
        use (fmap (.payload) <$> recv)
    ProtocolInstrument ->
      serverStreaming connection (rpc @(Protobuf Instrument "events")) defMessage \ recv ->
        use (fmap (.payload) <$> recv)

-- | Send an encoded request to the @Api@ method and return the encoded response.
apiRequest ::
  Protocol ->
  Connection ->
  ByteString ->
  IO ByteString
apiRequest protocol connection payload =
  case protocol of
    ProtocolGhcServer ->
      (.payload) <$> nonStreaming connection (rpc @(Protobuf GhcServer "api")) (defMessage & GhcServer.payload .~ payload)
    ProtocolInstrument ->
      (.payload) <$> nonStreaming connection (rpc @(Protobuf Instrument "api")) (defMessage & Instrument.payload .~ payload)
