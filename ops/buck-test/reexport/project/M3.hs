{-# language TemplateHaskell #-}

module M3 where

import Language.Haskell.TH.Syntax (lift)
import M2

lifter :: ()
lifter = $(lift user)
