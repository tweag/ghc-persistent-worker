module Ghc.Ui.Data.ServerConnection where

import Network.GRPC.Client (Connection)

-- | A connection to a server offering the 'Proto.Worker.Worker' service.
data ServerConnection =
  ServerConnection {
    connection :: Connection
  }
