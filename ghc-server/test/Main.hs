module Main where

import Test.BuildTest (test_serverBuild)
import Test.CabalTest (test_cabalTests)
import Test.CacheTest (test_depLoadOrder)
import Test.ScheduleTest (test_schedule)
import Test.SchedulerTest (test_scheduler)
import Test.Tasty (TestTree, defaultMain, testGroup)

tests :: TestTree
tests =
  testGroup "ghc-server" [test_serverBuild, test_cabalTests, test_depLoadOrder, test_schedule, test_scheduler]

main :: IO ()
main = defaultMain tests
