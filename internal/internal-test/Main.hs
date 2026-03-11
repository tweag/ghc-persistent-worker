module Main where

import BuildPlanTest (test_buildPlan_make, test_buildPlan_oneshot)
import Test.Tasty (TestTree, defaultMain, testGroup)

tests :: TestTree
tests =
  testGroup "all" [
    test_buildPlan_make,
    test_buildPlan_oneshot
  ]

main :: IO ()
main = defaultMain tests
