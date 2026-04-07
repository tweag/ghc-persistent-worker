module Main where

import Test.BuildTest (test_serverBuild)
import Test.CacheTest (test_depLoadOrder)
import Test.ScheduleTest (test_schedule)
import Test.SchedulerTest (test_scheduler)
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main =
defaultMain (testGroup "ghc-server" [test_serverBuild, test_depLoadOrder, test_schedule, test_scheduler])
