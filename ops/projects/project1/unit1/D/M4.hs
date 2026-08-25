module D.M4 where

import Data.Aeson
import GHC.Generics (Generic)

newtype Size =
  Size Double
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)
