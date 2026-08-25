module M3 where

import Data.Aeson
import GHC.Generics (Generic)

data Thing =
  Thing {
    size :: Double,
    quality :: Int,
    name :: String
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)
