module Test.ProjectBuild.Property where

import Control.Monad (unless, when)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (for_)
import Data.IORef (writeIORef)
import Data.List (intercalate)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import GHC.Stack (HasCallStack, withFrozenCallStack)
import Hedgehog (MonadTest, PropertyT, annotate, assert, diff)
import System.Directory.OsPath (doesFileExist)
import System.OsPath.Extra (fromOsPath, osp, (<.>), (</>))
import Test.Data.BuildSystem (BuildResult (..))
import Test.Data.Env (SessionEnv (..), TestEnv (..))
import Test.Data.Project (InitialProject (..), ModuleKey (..), ModuleSource (..), TaskKey (..), taskModuleKeys)
import Test.Data.ProjectBuild (ProjectBuild (..), RebuildSet (..), ResumePlan (..))
import Test.Data.Scheduler (Schedule (..), Task (..), unexpectedFailure)
import Test.Path (moduleName, moduleOutputBase, showUnit)

showModuleKey :: ModuleKey -> String
showModuleKey key =
  showUnit key.unit ++ ":" ++ show key.number ++ foldMap showError key.errorVariant
  where
    showError e = "(" ++ show e ++ ")"

showTaskKey :: TaskKey -> String
showTaskKey = \case
  TaskMeta unit -> showUnit unit
  TaskCompile m -> showModuleKey m

showTask :: Task TaskKey a -> String
showTask task
  | Set.null task.deps = showTaskKey task.key
  | otherwise = showTaskKey task.key ++ " <- " ++ intercalate ", " (fmap showTaskKey (Set.toList task.deps))

-- | Shared assertions: no unexpected failures, object files and interfaces exist for succeeded modules.
-- When the build succeeded without errors, also checks completeness (all expected tasks completed).
-- When the assertions fail and 'Test.Data.Env.keepFailedDirs' is enabled, the session's source and temp
-- directories are retained and their paths are printed to the console.
assertBuildResult :: SessionEnv -> ProjectBuild -> BuildResult -> PropertyT IO ()
assertBuildResult sessionEnv project BuildResult {failures, succeeded, completed, hasErrors} = do
  let unexpectedFailures = Map.filter unexpectedFailure failures
  missingFiles <- liftIO checkObjectFiles
  let incomplete = not hasErrors && completed /= project.allKeys
      hasFailure = not (Map.null unexpectedFailures) || not (null missingFiles) || incomplete
  liftIO (retainOnFailure sessionEnv hasFailure)
  annotateFailures unexpectedFailures
  annotateMissingFiles missingFiles
  diff unexpectedFailures (==) Map.empty
  diff missingFiles (==) []
  unless hasErrors do
    diff completed (==) project.allKeys
  where
    annotateFailures fs =
      unless (Map.null fs) do
        annotate "Module failures:"
        for_ (Map.toList fs) \ (key, failure) ->
          annotate $ "  " ++ showTaskKey key ++ ": " ++ show failure

    annotateMissingFiles files =
      unless (null files) do
        annotate "Missing object files:"
        for_ files (annotate . fromOsPath)

    checkObjectFiles =
      concat <$> traverse checkMod (taskModuleKeys succeeded)
      where
        checkMod key = do
          let base = sessionEnv.tempDir </> moduleOutputBase key
              objFile = base <.> [osp|dyn_o|]
              hiFile = base <.> [osp|dyn_hi|]
          objExists <- doesFileExist objFile
          hiExists <- doesFileExist hiFile
          pure $ [objFile | not objExists] ++ [hiFile | not hiExists]

-- | If the build result is a failure and the session's 'TestEnv' has 'keepFailedDirs' enabled, mark the shared
-- root directory for retention (so 'Test.Env.releaseTestEnv' skips deleting it) and print the paths of the
-- source and temp directories used by the failing test case.
retainOnFailure :: SessionEnv -> Bool -> IO ()
retainOnFailure sessionEnv hasFailure =
  when (hasFailure && sessionEnv.shared.keepFailedDirs) do
    writeIORef sessionEnv.shared.retainedDirs True
    putStrLn $ unlines
      [ "Test case failed; retaining project directories for inspection:"
      , "  source: " ++ fromOsPath sessionEnv.sourceDir
      , "  build:  " ++ fromOsPath sessionEnv.tempDir
      ]

annotateRebuildPlan :: ResumePlan -> PropertyT IO ()
annotateRebuildPlan plan =
  when plan.rebuild.hasChanges do
    unless (Map.null plan.moduleMutations) do
      annotate ("Modified modules: " ++ show (moduleName <$> Map.keys plan.moduleMutations))
    unless (Map.null plan.depMutations) do
      annotate ("Added deps: " ++ show (Map.mapKeys moduleName (Set.map moduleName . fst <$> plan.depMutations)))
    annotate ("Rebuild set: " ++ show (Set.map moduleName plan.rebuild.moduleKeys))

showResumePlan :: ResumePlan -> [String]
showResumePlan plan =
  ["fixErrors: " ++ show plan.fixErrors]
  ++ if Map.null plan.moduleMutations then []
     else ["modified: " ++ intercalate ", " (fmap showModuleKey (Map.keys plan.moduleMutations))]
  ++ if Map.null plan.depMutations then []
     else ["addedDeps: " ++ intercalate ", " (fmap showAddedDep (Map.toList plan.depMutations))]
  where
    showAddedDep (key, (extra, _)) =
      showModuleKey key ++ " <- " ++ intercalate ", " (fmap showModuleKey (Set.toList extra))

showProjectBuild :: ProjectBuild -> String
showProjectBuild ProjectBuild {schedule, resumeSchedule, resumePlan, initial} =
  unlines $
    ["Project: " ++ show unitCount ++ " units, " ++ show moduleCount ++ " modules"
      ++ if errorCount == 0 then "" else " (" ++ show errorCount ++ " error)"
    , "Template Haskell: " ++ show thCount ++ " modules"
    ]
    ++ ["", "── first schedule ──"]
    ++ fmap showTask schedule.tasks
    ++ ["", "── resume schedule ──"]
    ++ fmap showTask resumeSchedule.tasks
    ++ ["", "── resume plan ──"]
    ++ showResumePlan resumePlan
  where
    errorCount = Map.size modulesError

    thCount = Map.size (Map.filter (.th) modules)

    InitialProject {unitCount, moduleCount, modulesError, modules} = initial

assertNoFailures ::
  HasCallStack =>
  MonadTest m =>
  String ->
  BuildResult ->
  m ()
assertNoFailures label result =
  withFrozenCallStack do
    unless (Map.null result.failures) do
      annotate (label ++ " failures: " ++ show (Map.keys result.failures))
    assert (Map.null result.failures)
