{-# language TemplateHaskell #-}

module M1 where

import Data.Aeson (defaultOptions)
import Data.Aeson.TH (deriveJSON)
import qualified Data.ByteString.Char8 as ByteString
import Language.Haskell.TH.Syntax (lift)
import M2 (payload)

data Entity =
  Entity {
    name :: String,
    number :: Int
  }
  deriving (Eq, Show)

deriveJSON defaultOptions ''Entity

-- | Lifting a value derived from the imported 'M2.payload' inside a splice forces GHC to link M2's bytecode
-- during this module's splice execution, which is what populates the worker's bytecode cache.
cached :: String
cached = $(lift (ByteString.unpack payload))

main :: IO String
main =
  pure cached
