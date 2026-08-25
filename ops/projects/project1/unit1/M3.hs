module M3 where

import D.M4
import Data.Aeson
import GHC.Generics (Generic)

data Thing =
  Thing {
    size :: Size,
    quality :: Int,
    name :: String
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)
