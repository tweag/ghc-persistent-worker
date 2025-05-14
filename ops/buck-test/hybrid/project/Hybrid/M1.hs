module Hybrid.M1 where

import Language.Haskell.TH (ExpQ, runIO)
import Language.Haskell.TH.Syntax (lift)
import System.Process (readProcess)

m1 :: ExpQ
m1 = lift @_ @Int (1 + 0)

runThExe :: ExpQ
runThExe = do
  out <- runIO (readProcess "th-exe" [] "")
  lift @_ @String out

s1 :: String
s1 = "s1"
