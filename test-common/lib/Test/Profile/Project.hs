-- | Project structure for the profiling test.
--
-- A larger static project with configurable unit and module counts, designed to exercise the compilation pipeline at
-- scale for profiling purposes.
-- Uses the same dependency structure as the resource test: linear unit chain with prefix intra-unit module deps.
module Test.Profile.Project where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Test.Data.Project (
  BuildModule (..),
  Component (..),
  GenUnit (..),
  ModuleKey (..),
  ModuleSource (..),
  TaskKey (..),
  UnitKey (..),
  )
import Test.Data.Scheduler (Schedule (..), Task (..))

-- | All module keys for a unit with the given number of modules.
unitModuleKeys :: Int -> UnitKey -> [ModuleKey]
unitModuleKeys modulesPerUnit unit =
  [ModuleKey {unit, number, errorVariant = Nothing} | number <- [0 .. modulesPerUnit - 1]]

-- | Module dependencies within a unit: each module depends on all prefix modules.
intraUnitDeps :: ModuleKey -> Set ModuleKey
intraUnitDeps ModuleKey {unit, number} =
  Set.fromList [ModuleKey {unit, number = n, errorVariant = Nothing} | n <- [0 .. number - 1]]

-- | Cross-unit module dependencies: all modules from dependency units.
crossUnitDeps :: Int -> Set UnitKey -> Set ModuleKey
crossUnitDeps modulesPerUnit depUnits =
  Set.fromList [mk | u <- Set.toList depUnits, mk <- unitModuleKeys modulesPerUnit u]

-- | All module dependencies for a module.
moduleDeps :: Int -> Set UnitKey -> ModuleKey -> Set ModuleKey
moduleDeps modulesPerUnit depUnits key =
  Set.union (intraUnitDeps key) (crossUnitDeps modulesPerUnit depUnits)

-- | Construct a unit with the given parameters.
mkUnit :: Int -> Bool -> Int -> Set Int -> UnitKey -> Set UnitKey -> GenUnit BuildModule
mkUnit modulesPerUnit th bindings extDeps unitKey depUnits =
  GenUnit {
    key = unitKey,
    depUnits,
    modules = [
      BuildModule {key, deps = moduleDeps modulesPerUnit depUnits key, th, bindings, extDeps}
      | key <- unitModuleKeys modulesPerUnit unitKey
    ]
  }

-- | Build the full list of units in dependency order.
-- Each unit depends on all preceding units (linear chain).
allUnits :: Int -> Int -> Bool -> Int -> Set Int -> [GenUnit BuildModule]
allUnits unitCount modulesPerUnit th bindings extDeps =
  [mkUnit modulesPerUnit th bindings extDeps (UnitKey i) (Set.fromList (fmap UnitKey [0 .. i - 1]))
  | i <- [0 .. unitCount - 1]]

-- | The full module source map for writing project sources.
allModuleSources :: [GenUnit BuildModule] -> Map ModuleKey ModuleSource
allModuleSources units =
  Map.fromList
    [(bm.key, ModuleSource {deps = Set.toList bm.deps, th = bm.th, bindings = bm.bindings, extDeps = bm.extDeps})
    | u <- units, bm <- u.modules]

-- | Build a metadata task for one unit.
metaTask :: GenUnit BuildModule -> Task TaskKey Component
metaTask unit =
  Task {
    key = TaskMeta unit.key,
    deps = Set.map TaskMeta unit.depUnits,
    value = ComponentUnit unit
  }

-- | Build a compile task for one module within a unit.
moduleTask :: UnitKey -> BuildModule -> Task TaskKey Component
moduleTask unitKey BuildModule {key, deps} =
  Task {
    key = TaskCompile key,
    deps = Set.insert (TaskMeta unitKey) (Set.map TaskCompile deps),
    value = ComponentModule key
  }

-- | Tasks for a single unit: one metadata task followed by all module compile tasks.
unitTasks :: GenUnit BuildModule -> [Task TaskKey Component]
unitTasks unit =
  metaTask unit : [moduleTask unit.key bm | bm <- unit.modules]

-- | The full schedule in strict dependency order.
schedule :: [GenUnit BuildModule] -> Schedule TaskKey Component
schedule units =
  Schedule {tasks = concatMap unitTasks units}
