{-# language TemplateHaskell #-}
module Lib where

import Dep1

use :: Int
use = 1 + $(dep1_1)
