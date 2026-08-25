module M2 where

import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as Lazy
import M3

payload :: ByteString
payload =
  Lazy.toStrict $ encode Thing {
    size = 49.2,
    quality = 99,
    name = "snake"
  }
