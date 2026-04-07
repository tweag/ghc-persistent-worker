-- | Profiling test entry point.
--
-- Compiles a large static project (6 units × 5 modules) through the build pipeline, designed to be run with profiling
-- enabled.
-- Uses the same module structure as the resource test but at larger scale.
module ProfileTest where

import Control.Monad.IO.Class (liftIO)
import Data.Functor ((<&>))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import System.Environment (lookupEnv)
import System.IO (hPutStrLn, stderr)
import Test.Data.BuildSystem (BuildResult (..))
import Test.Data.Env (TestEnv)
import Test.Data.Project (BuildModule, GenUnit (..))
import Test.Env (newSessionEnv, withTestEnv)
import Test.Profile.Build (runProfileBuild)
import Test.Profile.Project (allUnits)
import Test.Run (unitTest)
import Test.Tasty (TestTree, testGroup)

unitCount :: Int
unitCount = 6

modulesPerUnit :: Int
modulesPerUnit = 5

bindingsPerModule :: Int
bindingsPerModule = 20

-- | All units for the profiling test: 6 units × 5 modules, TH enabled, 20 bindings, 2 ext deps.
profileUnits :: [GenUnit BuildModule]
profileUnits =
  allUnits unitCount modulesPerUnit True bindingsPerModule (Set.fromList [0, 1])

-- | Check whether the environment supports running the profiling test.
-- Requires the @resource_test_ext_deps@ env var (set by the @test-ext-deps@ devshell).
checkEnvironment :: IO (Maybe String)
checkEnvironment =
  lookupEnv "resource_test_ext_deps" <&> \case
    Just _ -> Nothing
    Nothing -> Just "resource_test_ext_deps not set (use the test-ext-deps devshell)"

test_profile :: IO TestEnv -> TestTree
test_profile getEnv =
  unitTest "profile-build" do
    maybe run skip =<< liftIO checkEnvironment
  where
    run = do
      env <- liftIO (newSessionEnv =<< getEnv)
      result <- liftIO (runProfileBuild profileUnits env)
      liftIO $ putStrLn $ "Profile build completed: "
        ++ show (length profileUnits) ++ " units, "
        ++ show (sum [length u.modules | u <- profileUnits]) ++ " modules"
      liftIO $ putStrLn $ "Failures: " ++ show (Map.size result.failures)

    skip reason =
      liftIO $ hPutStrLn stderr $ "Skipping profile test: " ++ reason

test_profiling :: TestTree
test_profiling =
  withTestEnv \ getEnv ->
    testGroup "profiling" [
      test_profile getEnv
    ]
