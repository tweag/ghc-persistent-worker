{-# language MagicHash, UnliftedFFITypes #-}

module L1 where

import Data.Primitive.ByteArray (MutableByteArray (..), MutableByteArray#, newPinnedByteArray)
import Language.Haskell.TH (ExpQ, runIO)
import Language.Haskell.TH.Syntax (lift)

l1_uuid :: ExpQ
l1_uuid = do
  runIO uuid
  lift ()

uuid :: IO ()
uuid = do
  MutableByteArray buf <- newPinnedByteArray 16
  uuid_generate_time buf

foreign import ccall safe "uuid_generate_time"
  uuid_generate_time :: MutableByteArray# s -> IO ()
