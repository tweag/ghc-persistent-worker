module L1 where

import Language.Haskell.TH (ExpQ)
import Language.Haskell.TH.Syntax (lift)

l1 :: ExpQ
l1 = lift @_ @Int 4
