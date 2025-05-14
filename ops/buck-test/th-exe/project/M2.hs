{-# LANGUAGE TemplateHaskell #-}

module M2 where

import M1

use :: String
use = $(runThExe)

s2 :: String
s2 = s1
