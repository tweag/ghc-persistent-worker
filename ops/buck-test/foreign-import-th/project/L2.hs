{-# language TemplateHaskell #-}

module L2 where

import Data.Word
import L1

l2 :: ()
l2 = $(l1_uuid)
