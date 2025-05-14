{-# language TemplateHaskell #-}
module L2 where

import Dep1
import L1

use_1 :: Int
use_1 = $(dep1_1) + $(l1)
