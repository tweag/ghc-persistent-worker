-- | Build runner for the profiling test.
--
-- Runs a full build of the profile test project without allocation measurement.
module Test.Profile.Build where

import Data.Foldable (fold)
import qualified Data.Set as Set
import Test.Build (initialStrategy, runSchedule)
import Test.Data.BuildSystem (BuildResult)
import Test.Data.Env (MaxJobs (..), SessionEnv (..))
import Test.Data.Project (BuildModule (..), GenUnit (..))
import Test.ExtDep (createExtDepPackageDbs)
import Test.Profile.Project (allModuleSources, schedule)
import Test.Source (writeProjectSources)

-- | Run the full build sequentially.
runProfileBuild :: [GenUnit BuildModule] -> SessionEnv -> IO BuildResult
runProfileBuild units env = do
  extDepDbs <- createExtDepPackageDbs env.tempDir allExtDeps
  let envWithExtDeps = env {extDepDbs, extDeps = allExtDeps}
  writeProjectSources envWithExtDeps.sourceDir (allModuleSources units)
  runSchedule (MaxJobs 1) (initialStrategy envWithExtDeps False) Set.empty (schedule units)
  where
    allExtDeps = fold [bm.extDeps | u <- units, bm <- u.modules]
