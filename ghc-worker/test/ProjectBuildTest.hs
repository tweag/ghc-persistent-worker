{-# LANGUAGE QuasiQuotes #-}

module ProjectBuildTest where

import Control.Concurrent.MVar (readMVar)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (traverse_)
import Data.List (isPrefixOf)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import Data.Traversable (for)
import GHC (isExternalName, moduleNameFS)
import GHC.ByteCode.Types (bc_bcos, unlinkedBCOName)
import GHC.Data.FastString (FastString)
import GHC.Data.FlatBag (elemsFlatBag)
import GHC.Linker.Types (linkableBCOs, linkableModule)
import GHC.Types.Name (Name)
import GHC.Unit.Home.Graph (HomeUnitGraph, UnitEnvGraph (..), homeUnitEnv_hpt)
import GHC.Unit.Home.ModInfo (homeModInfoByteCode)
import GHC.Unit.Home.PackageTable (concatHpt)
import GHC.Unit.Types (GenModule (..), Module, UnitId, unitFS)
import GHC.Utils.Outputable (showPprUnsafe)
import Hedgehog (MonadTest, PropertyT, TestT, annotate, assert, forAllWith, property, withTests, (===))
import Test.BuckHashes (writeUnitHashes)
import Test.BuildSystem (mkBuildSystem)
import Test.Data.BuildSystem (BuildResult (..), BuildSystem (..))
import Test.Data.Env (SessionEnv (..), TestConfig (..), TestEnv (..), withTestConfig)
import Test.Data.Project (
  BuildModule (..),
  BuildTask,
  Component (..),
  GenUnit (..),
  InitialProject (..),
  ModuleKey (..),
  ModuleSource (..),
  TaskKey (..),
  )
import Test.Data.ProjectBuild (ProjectBuild (..), RebuildSet (..), ResumePlan (..), scheduleKeys)
import Test.Data.Scheduler (Schedule (..), Task (..))
import Test.Env (newResumeSessionEnv, newSessionEnv, withTestEnv)
import Test.Gen.ProjectBuild (genProjectBuild, metaTask, moduleTask, sortSchedule)
import Test.ProjectBuild.Classify (classifyFirstBuild, classifyProject, classifyResume)
import Test.ProjectBuild.Property (annotateRebuildPlan, assertBuildResult, showProjectBuild)
import Test.Resume (executeResumeBuild, setupResumeBuild)
import Test.Run (unitTest)
import Test.Source (writeProjectSources)
import Test.Tasty (TestTree)
import Test.Tasty.Hedgehog (testProperty)
import Types.Args (Args (..))
import Types.Env (Env (..))
import Types.FeatureFlags (FeatureFlags (..))
import Types.State (WorkerState (..))
import Types.State.Make (MakeState (..))

-- | Extract 'GenUnit' values from unit metadata tasks in the schedule.
scheduleUnits :: Schedule TaskKey Component -> [GenUnit BuildModule]
scheduleUnits schedule =
  [unit | Task {value = ComponentUnit unit} <- schedule.tasks]

-- | Write per-unit buck_source_hashes files if incremental metadata is enabled.
updateActionMetadata :: Bool -> SessionEnv -> Schedule TaskKey Component -> IO ()
updateActionMetadata False _ _ = pure ()
updateActionMetadata True env schedule =
  traverse_ (writeUnitHashes env.tempDir env.sourceDir) (scheduleUnits schedule)

-- | Generate a test case and create temp directories, state, and handlers.
setup :: TestConfig -> TestEnv -> PropertyT IO (ProjectBuild, SessionEnv, BuildSystem)
setup conf env = do
  project <- forAllWith showProjectBuild (genProjectBuild conf)
  sessionEnv <- liftIO (newSessionEnv env)
  pure (project, sessionEnv, mkBuildSystem conf.maxConcurrentJobs project.incrementalBuildPlan sessionEnv)

-- | Write source files to the temp dir and run the initial build.
runInitialBuild :: ProjectBuild -> BuildSystem -> SessionEnv -> PropertyT IO BuildResult
runInitialBuild project buildSys sessionEnv = do
  result <- liftIO do
    writeProjectSources sessionEnv.sourceDir project.initial.modules
    updateActionMetadata project.incrementalBuildPlan sessionEnv project.schedule
    buildSys.runInitialBuild project.schedule
  classifyProject project
  classifyFirstBuild result
  assertBuildResult sessionEnv.tempDir project result
  pure result

-- | Update source files, write Buck cache, and run the resume build.
runResumeBuild :: ProjectBuild -> BuildSystem -> SessionEnv -> BuildResult -> PropertyT IO ()
runResumeBuild build buildSys initialEnv initialResult = do
  cachedSchedule <- liftIO $ setupResumeBuild buildSys initialEnv build initialResult
  liftIO $ updateActionMetadata build.incrementalBuildPlan initialEnv build.resumeSchedule
  resumeEnv <- liftIO $ newResumeSessionEnv initialEnv
  resumeResult <- liftIO $ executeResumeBuild buildSys resumeEnv build initialResult cachedSchedule
  classifyResume build initialResult
  annotateRebuildPlan build.resumePlan
  assertBuildResult resumeEnv.tempDir build resumeResult

prop_projectBuild :: TestConfig -> TestEnv -> PropertyT IO ()
prop_projectBuild conf env = do
  (project, sessionEnv, buildSys) <- setup conf env
  initialResult <- runInitialBuild project buildSys sessionEnv
  runResumeBuild project buildSys sessionEnv initialResult

-- | The options can be overridden on the command line:
-- > cabal test ghc-worker --test-options="--max-units 10 --max-modules-per-unit 8 --max-concurrent-jobs 4 --hedgehog-tests 200"
test_projectBuild :: TestTree
test_projectBuild =
  withTestEnv \ getTestEnv ->
    withTestConfig \ conf ->
      testProperty "multi-unit project build" $ withTests 100 $ property do
        env <- liftIO getTestEnv
        prop_projectBuild conf env

-- | Module keys for the deterministic TH-dependency scenario.
keyA, keyB, keyC :: ModuleKey
keyA = ModuleKey {unit = 0, number = 0, errorVariant = Nothing}
keyB = ModuleKey {unit = 1, number = 0, errorVariant = Nothing}
keyC = ModuleKey {unit = 1, number = 1, errorVariant = Nothing}

modA, modB, modC :: BuildModule
modA = BuildModule {key = keyA, deps = mempty, th = False, bindings = 1, extDeps = mempty}
modB = BuildModule {key = keyB, deps = mempty, th = False, bindings = 1, extDeps = mempty}
modC = BuildModule {key = keyC, deps = Set.fromList [keyA, keyB], th = True, bindings = 1, extDeps = mempty}

thUnit0, thUnit1 :: GenUnit BuildModule
thUnit0 = GenUnit {key = 0, depUnits = mempty, modules = [modA]}
thUnit1 = GenUnit {key = 1, depUnits = Set.singleton 0, modules = [modB, modC]}

thModuleSource :: BuildModule -> ModuleSource
thModuleSource BuildModule {deps, th, bindings, extDeps} =
  ModuleSource {deps = Set.toList deps, th, bindings, extDeps}

thInitial :: InitialProject
thInitial =
  InitialProject
    { modules = mods
    , modulesSuccess = mods
    , modulesError = mempty
    , unitCount = 2
    , moduleCount = 3
    }
  where
    mods =
      Map.fromList
        [(keyA, thModuleSource modA), (keyB, thModuleSource modB), (keyC, thModuleSource modC)]

-- | Initial build: compile A and B only, no compile task for C.
thInitialTasks :: [BuildTask]
thInitialTasks =
  [ metaTask thUnit0 thUnit0.depUnits
  , moduleTask thUnit0 modA
  , metaTask thUnit1 thUnit1.depUnits
  , moduleTask thUnit1 modB
  ]

-- | Resume build: same tasks plus the compile task for C.
thResumeTasks :: [BuildTask]
thResumeTasks = thInitialTasks ++ [moduleTask thUnit1 modC]

-- | Deterministic project build: the initial build compiles A and B, the resume build compiles only the TH module C,
-- restoring A and B from the cache.
thBuild :: ProjectBuild
thBuild =
  ProjectBuild
    { initial = thInitial
    , schedule = fst (sortSchedule thInitialTasks)
    , resumePlan =
        ResumePlan
          { fixErrors = False
          , moduleMutations = mempty
          , depMutations = mempty
          , rebuild = RebuildSet {moduleKeys = mempty, allAffectedKeys = mempty, hasChanges = True}
          }
    , resumeSchedule = resumeSchedule'
    , allKeys = scheduleKeys resumeSchedule'
    , incrementalBuildPlan = False
    }
  where
    resumeSchedule' = fst (sortSchedule thResumeTasks)

-- | Enumerate the loaded bytecode per unit by walking the Home Unit Graph's Home Package Tables.
enumerateBytecode :: HomeUnitGraph -> IO [(UnitId, [(Module, [Name])])]
enumerateBytecode (UnitEnvGraph graph) =
  for (Map.toList graph) \ (uid, hue) -> do
    entries <- concatHpt bytecodeEntry (homeUnitEnv_hpt hue)
    pure (uid, entries)
  where
    bytecodeEntry hmi =
      case homeModInfoByteCode hmi of
        Nothing -> []
        Just lnk -> [(linkableModule lnk, bcoNames lnk)]

    bcoNames lnk =
      [unlinkedBCOName bco | cbc <- linkableBCOs lnk, bco <- elemsFlatBag (bc_bcos cbc)]

assertNoFailures ::
  MonadTest m =>
  String ->
  BuildResult ->
  m ()
assertNoFailures label result = do
  unless (Map.null result.failures) do
    annotate (label ++ " failures: " ++ show (Map.keys result.failures))
  assert (Map.null result.failures)

targetBcos :: [(FastString, FastString, [String])]
targetBcos =
  [
    (
      "unit0",
      "Unit0Module0",
      ["value_0_0"]
    ),
    (
      "unit1",
      "Unit1Module0",
      ["value_1_0"]
    ),
    (
      "unit1",
      "Unit1Module1",
      ["value_1_1"]
    )
  ]

enableLazyByteCode :: TestEnv -> TestEnv
enableLazyByteCode testEnv =
  testEnv {
    baseArgs = testEnv.baseArgs {
      features = testEnv.baseArgs.features {lazyByteCode = True}
    }
  }

loadedBcos :: Env -> IO [(FastString, FastString, [String])]
loadedBcos env = do
  loaded <- liftIO do
    WorkerState {make} <- readMVar env.state
    enumerateBytecode make.hug
  pure [modBcos m ns | (_, mods) <- loaded, (m, ns) <- mods]
  where
    modBcos m ns =
      (unitFS m.moduleUnit, moduleNameFS m.moduleName, mapMaybe interestingName ns)

    interestingName name
      | isExternalName name
      , not (isPrefixOf "$" (showPprUnsafe name))
      = Just (showPprUnsafe name)
      | otherwise
      = Nothing

thDepBytecodeProperty :: TestEnv -> TestT IO ()
thDepBytecodeProperty testEnv = do
  sessionEnv <- liftIO (newSessionEnv (enableLazyByteCode testEnv))
  let buildSys = mkBuildSystem 6 False sessionEnv

  initialResult <- liftIO do
    writeProjectSources sessionEnv.sourceDir thBuild.initial.modules
    buildSys.runInitialBuild thBuild.schedule
  assertNoFailures "initial" initialResult

  cachedSchedule <- liftIO $ setupResumeBuild buildSys sessionEnv thBuild initialResult
  resumeEnv <- liftIO $ newResumeSessionEnv sessionEnv

  -- Make sure we didn't accidentally keep the first build's state
  bcosPre <- liftIO $ loadedBcos resumeEnv.env
  [] === bcosPre

  resumeResult <- liftIO $ executeResumeBuild buildSys resumeEnv thBuild initialResult cachedSchedule
  assertNoFailures "resume" resumeResult

  bcos <- liftIO $ loadedBcos resumeEnv.env
  targetBcos === bcos

test_thDepBytecode :: TestTree
test_thDepBytecode =
  withTestEnv \ getTestEnv ->
    unitTest "TH dependency bytecode restoration" do
      env <- liftIO getTestEnv
      thDepBytecodeProperty env
