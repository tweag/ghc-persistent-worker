{-# language TemplateHaskell #-}

module M4 where

import Dep1
import Hybrid.M1 (runThExe)
import M3

m4 :: Int
m4 = 1 + 0 + $(m1) + 0 + $(dep1_1)

use :: String
use = $(runThExe)
