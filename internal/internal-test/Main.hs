{-# LANGUAGE CPP #-}

module Main where
 
import BuildPlanTest (test_buildPlan)
import CompileHptTest (test_compileHpt)
import ProjectBuildTest (test_projectBuild)
import ScheduleTest (test_sortScheduleOrder)
import Test.Data.Env (testConfigOptions)
import Test.Tasty (TestTree, defaultIngredients, defaultMainWithIngredients, includingOptions, testGroup)
 
fullTest :: Bool

#if defined(MWB) || defined(MWB_2025_10)

fullTest = True

#else

fullTest = False

#endif

tests :: TestTree
tests =
  testGroup "all" $ [
    test_sortScheduleOrder,
    test_projectBuild
  ] <> if fullTest then [
    test_buildPlan,
    test_compileHpt
  ] else []

main :: IO ()
main =
  defaultMainWithIngredients (includingOptions testConfigOptions : defaultIngredients) tests
