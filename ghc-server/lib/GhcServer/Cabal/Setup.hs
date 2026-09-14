module GhcServer.Cabal.Setup where

import qualified Distribution.Client.Main as Cabal

cabalSetup :: [String] -> IO ()
cabalSetup args = Cabal.main ("act-as-setup" : args)
