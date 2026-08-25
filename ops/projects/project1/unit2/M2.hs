module M2 where

import D.M4
import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as Lazy
import M3

payload :: ByteString
payload =
  Lazy.toStrict $ encode Thing {
    size = Size 49.2,
    quality = 99,
    name = "snake"
  }
