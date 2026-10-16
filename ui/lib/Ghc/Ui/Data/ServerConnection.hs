module Ghc.Ui.Data.ServerConnection where

import BuckWorkerProto.Data.Protocol (Protocol)
import Network.GRPC.Client (Connection)

-- | A connection to a server, together with the gRPC service that the server offers.
data ServerConnection =
  ServerConnection {
    protocol :: Protocol,
    connection :: Connection
  }
