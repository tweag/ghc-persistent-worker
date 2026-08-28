{-# LANGUAGE CPP #-}
module Main where

import GhcServer.Build.ExecutorChild (runExecutor)
import GhcServer.Build.ProcessChild (runProcessEval)
import GhcServer.Data.ProcessEval (ProcessEvalOptions (..))
import System.Environment (getArgs)
import System.OsPath.Extra (toOsPath)
import Test.BuildTest (test_serverBuild)
import Test.CabalTest (test_cabalTests)
import Test.CacheTest (test_depLoadOrder)
import Test.CompactBytecodeTest (test_compactBytecode)
import Test.EvictionTest (test_bcoEvictionManual)
import Test.ExecutorPlanTest (test_executorPlan)
import Test.GbcTransportTest (test_gbcTransport)
import Test.ScheduleTest (test_schedule)
import Test.SchedulerTest (test_scheduler)
import Test.SubprocessTest (test_executorEvents, test_executorExecute, test_subprocessExecute)
#if defined(GBC)
import Test.SubprocessTest (test_executorPlanExecute)
#endif
import Test.Tasty (DependencyType (..), TestTree, defaultMain, dependentTestGroup)

tests :: TestTree
tests =
  dependentTestGroup "ghc-server" AllFinish [
    test_serverBuild,
    test_cabalTests,
    test_depLoadOrder,
    test_bcoEvictionManual,
    test_schedule,
    test_scheduler,
    test_compactBytecode,
    test_gbcTransport,
    test_executorPlan,
    test_subprocessExecute,
    test_executorExecute,
    test_executorEvents
#if defined(GBC)
    ,
    test_executorPlanExecute
#endif
    ]

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["eval"] -> runProcessEval ProcessEvalOptions {configFile = Nothing}
    ["executor", "--socket", socket] -> runExecutor (toOsPath socket)
    _ -> defaultMain tests
