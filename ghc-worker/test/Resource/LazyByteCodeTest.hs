module Resource.LazyByteCodeTest where

import Control.Monad.IO.Class (liftIO)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Hedgehog (assert)
import Hedgehog.Internal.Property (TestT)
import ProjectBuildTest (assertNoFailures, enableLazyByteCode, loadedBcos)
import Resource.Measure (assertMeasurements, checkEnvironment)
import System.IO (hPutStrLn, stderr)
import Test.Build (initialStrategy, resumeStrategy)
import Test.BuildSystem (mkBuildSystem)
import Test.Data.Env (SessionEnv (..), TestEnv)
import Test.Data.Project (
  BuildModule (..),
  Component,
  GenUnit (..),
  InitialProject (..),
  ModuleKey (..),
  ModuleSource (..),
  TaskKey (..),
  UnitKey,
  weakenResumeComponent,
  )
import Test.Data.ProjectBuild (ProjectBuild (..), RebuildSet (..), ResumePlan (..), scheduleKeys)
import Test.Data.Scheduler (Schedule (..))
import Test.Env (newResumeSessionEnv, newSessionEnv, withTestEnv)
import Test.Gen.ProjectBuild (metaTask, moduleTask, sortSchedule)
import Test.Resource.Build (phaseName, withMeasuredBuild)
import Test.Resource.Stats (PhaseReference (..), PhaseResult (..))
import Test.Resume (setupResumeBuild, trimResumeSchedule)
import Test.Run (unitTest)
import Test.Source (writeProjectSources)
import Test.Tasty (TestTree)

numMods :: Int
numMods = 20

keys1 :: UnitKey -> [ModuleKey]
keys1 unit =
  [ModuleKey {unit, number, errorVariant = Nothing} | number <- [1 .. numMods]]

keys2 :: UnitKey -> [ModuleKey]
keys2 unit =
  [ModuleKey {unit, number, errorVariant = Nothing} | number <- [numMods + 1 .. 2 * numMods]]

keyTh :: ModuleKey
keyTh =
  ModuleKey {unit = 1, number = 2 * numMods + 1, errorVariant = Nothing}

mods1 :: UnitKey -> [BuildModule]
mods1 unit =
  [BuildModule {key, deps = mempty, th = False, bindings = 1, extDeps = mempty} | key <- keys1 unit]

mods2 :: UnitKey -> [BuildModule]
mods2 unit =
  [BuildModule {key, deps = mempty, th = False, bindings = 1, extDeps = mempty} | key <- keys2 unit]

modTh :: BuildModule
modTh =
  BuildModule {
    key = ModuleKey {unit = 1, number = 2 * numMods + 1, errorVariant = Nothing},
    deps = Set.fromList (keys1 0 ++ keys1 1),
    th = True,
    bindings = 1,
    extDeps = mempty
  }

modNoTh :: BuildModule
modNoTh =
  BuildModule {
    key = ModuleKey {unit = 1, number = 2 * numMods + 2, errorVariant = Nothing},
    deps = Set.fromList (keys2 0 ++ keys2 1),
    th = False,
    bindings = 1,
    extDeps = mempty
  }

unit0 :: GenUnit BuildModule
unit0 =
  GenUnit {key, depUnits = mempty, modules = mods1 key ++ mods2 key}
  where
    key = 0

unit1 :: GenUnit BuildModule
unit1 =
  GenUnit {key, depUnits = [unit0.key], modules = mods1 key ++ mods2 key ++ [modTh, modNoTh]}
  where
    key = 1

units :: [GenUnit BuildModule]
units =
  [unit0, unit1]

modules :: [BuildModule]
modules =
  concatMap (.modules) units

moduleSource :: BuildModule -> ModuleSource
moduleSource BuildModule {deps, th, bindings, extDeps} =
  ModuleSource {deps = Set.toList deps, th, bindings, extDeps}

thInitial :: InitialProject
thInitial =
  InitialProject
    { modules = mods
    , modulesSuccess = mods
    , modulesError = mempty
    , unitCount = length units
    , moduleCount = length modules
    }
  where
    mods = Map.fromList [(m.key, moduleSource m) | m <- modules]

schedule :: Schedule TaskKey Component
schedule =
  fst (sortSchedule (concatMap unitTasks units))
  where
    unitTasks unit = metaTask unit unit.depUnits : [moduleTask unit m | m <- unit.modules]

build :: ProjectBuild
build =
  ProjectBuild {
    initial = thInitial,
    schedule = schedule,
    resumePlan = ResumePlan {
      fixErrors = False,
      moduleMutations = rebuild,
      depMutations = mempty,
      rebuild = RebuildSet {
        moduleKeys = rebuildKeys,
        allAffectedKeys = Set.map TaskCompile rebuildKeys,
        hasChanges = True
      }
    },
    resumeSchedule = schedule,
    allKeys = scheduleKeys schedule,
    incrementalBuildPlan = False
  }
  where
    rebuildKeys = Map.keysSet rebuild
    rebuild = [(modTh.key, moduleSource modTh), (modNoTh.key, moduleSource modNoTh)]

testMemoryBytecode :: TestEnv -> TestT IO ([PhaseResult], [PhaseResult])
testMemoryBytecode testEnv = do
  sessionEnv <- liftIO (newSessionEnv (if False then enableLazyByteCode testEnv else testEnv))
  let buildSys = mkBuildSystem 6 False sessionEnv

  (initialResult, measureInitial) <- liftIO do
    writeProjectSources sessionEnv.sourceDir build.initial.modules
    withMeasuredBuild (initialStrategy sessionEnv False) phaseName [] build.schedule
  assertNoFailures "initial" initialResult

  cachedSchedule <- liftIO $ setupResumeBuild buildSys sessionEnv build initialResult
  resumeEnv <- liftIO $ newResumeSessionEnv sessionEnv

  let (resumeTasks, unmodified) = trimResumeSchedule initialResult build.resumePlan.rebuild cachedSchedule.tasks
  (resumeResult, measureResume) <- liftIO $ withMeasuredBuild (resumeStrategy resumeEnv False False) (phaseName . weakenResumeComponent) unmodified resumeTasks
  assertNoFailures "resume" resumeResult

  bcos <- liftIO $ loadedBcos resumeEnv.env
  assert (not (null bcos))
  pure (measureInitial, measureResume)

targetInit :: [PhaseReference]
targetInit =
  [
    PhaseReference {name, allocatedMB, tolerancePercent = 5}
    | (name, allocatedMB) <- [
      ("unit_0_metadata", 33.6),
      ("unit_1_metadata", 28),
      ("unit_1_compile_41", 73),
      ("unit_1_compile_42", 32)
    ]
  ]

targetResume :: [PhaseReference]
targetResume =
  [
    PhaseReference {name, allocatedMB, tolerancePercent = 5}
    | (name, allocatedMB) <- [
      ("unit_1_metadata", 28),
      ("unit_1_compile_41", 114),
      ("unit_1_compile_42", 41)
    ]
  ]

test_memory_lazyByteCode :: TestTree
test_memory_lazyByteCode =
  withTestEnv \ getEnv ->
    unitTest "lazy bytecode allocations" do
      maybe (run getEnv) skip =<< liftIO checkEnvironment
  where
    run getEnv = do
      env <- liftIO getEnv
      (initial, resume) <- testMemoryBytecode env
      assertMeasurements targetInit initial
      assertMeasurements targetResume resume

    skip reason =
      liftIO $ hPutStrLn stderr $ "Skipping resource test: " ++ reason
