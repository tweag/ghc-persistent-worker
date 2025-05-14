{-# language TemplateHaskell #-}
module M2 where

import Language.Haskell.TH.Syntax (lift)
import M1

splice :: Int
splice = $(lift @_ @Int m1)
