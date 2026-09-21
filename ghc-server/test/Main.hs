module Main where

import GhcServer.Build.ProcessChild (runProcessEval)
import GhcServer.Data.ProcessEval (ProcessEvalOptions (..))
import System.Environment (getArgs)
import Test.BuildTest (test_serverBuild)
import Test.CabalTest (test_cabalTests)
import Test.CacheTest (test_depLoadOrder)
import Test.CompactBytecodeTest (test_compactBytecode)
import Test.ForkTest (test_forkShared)
import Test.ScheduleTest (test_schedule)
import Test.SchedulerTest (test_scheduler)
import Test.SubprocessTest (test_subprocessExecute)
import Test.Tasty (DependencyType (..), TestTree, defaultMain, dependentTestGroup)

tests :: TestTree
tests =
  dependentTestGroup "ghc-server" AllFinish [
    test_serverBuild,
    test_cabalTests,
    test_depLoadOrder,
    test_schedule,
    test_scheduler,
  test_forkShared "forkProcess-shared-mmap",
  test_compactBytecode,
  test_subprocessExecute
    -- test_forkMetadata deliberately not wired in: it hangs (see Test.ForkMetadataTest module docs / kb-process-isolation).
    ]

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["eval"] -> runProcessEval ProcessEvalOptions {configFile = Nothing}
    _ -> defaultMain tests
