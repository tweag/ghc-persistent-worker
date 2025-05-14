module M1 where

import Data.Fix
import Data.Functor.Identity

m1 :: Int
m1 = const 1 (refold runIdentity Identity 1)
