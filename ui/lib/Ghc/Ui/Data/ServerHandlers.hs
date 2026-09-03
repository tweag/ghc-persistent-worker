module Ghc.Ui.Data.ServerHandlers where

import GHC.Generics (Generic)
import Ghc.Ui.Data.ServerApi (ServerApi)
import Ghc.Ui.Data.ServerProcess (ServerConfig)
import Network.GRPC.Client (Connection)

data ApiConfig =
  ApiConfig {
     connections :: [Connection],
     sync :: Bool
  }
  deriving stock (Generic)

data ServerHandlers =
  ServerHandlers {
    start :: ServerConfig -> IO (),
    stop :: IO (),
    restart :: IO (),
    shutdown :: IO (),
    api :: ApiConfig -> ServerApi
  }
  deriving stock (Generic)
