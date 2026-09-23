-- | Compute build tasks for concurrent compilation.
--
-- Compile tasks are created as /pending/ from unit source file paths at batch time,
-- before metadata is known. After metadata completes for a unit,
-- 'resolveFromCachedUnit' produces a resolution map from the written cache that is
-- applied at promotion time by the scheduler.
-- 'promoteEnabled' then activates tasks that are both requested and resolvable,
-- transitively promoting cross-unit dependencies.
{-# LANGUAGE CPP #-}

module GhcServer.Build.Schedule where

import Control.Applicative ((<|>))
import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.=))
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Text as Text
import GHC (ModuleName, mkModuleName, moduleNameString)
import GHC.Unit.Module.Graph (ModuleGraphNode (..), nodeKeyModName, nodeKeyUnitId)
import GHC.Unit.Types (UnitId, unitIdString)
import GhcServer.Data.Unit (Project (..), moduleHiPath, unitId)
import System.OsPath (OsPath)
import Test.Scheduler (Generation, Phase (..), Task (..), initialGeneration)
import Types.Api (ExecutorId, UnitName (..))
import Types.CachedDeps (
  CachedDep (..),
  CachedDeps (..),
  CachedModule (..),
  CachedPackageDep (..),
  CachedUnit (..),
  JsonFs (..),
  )

#if MIN_VERSION_GLASGOW_HASKELL(9,14,0,0)

import GHC.Unit.Module.Graph (mgNodeDependencies)

#else

import GHC.Unit.Module.Graph (nodeDependencies)

#endif

-- | Key for a build task, indexed by 'Phase'.
--
-- @TaskKey \''Pending@: compile tasks are keyed by 'OsPath' (source file path,
-- known at project discovery time).
--
-- @TaskKey \''Resolved@: compile tasks are keyed by 'ModuleName' (determined
-- after metadata).  Dependencies are always expressed in terms of
-- @TaskKey \''Resolved@.
data TaskKey (p :: Phase) where
  -- | Metadata step for a unit.  Valid in any phase.
  MetaTask :: UnitName -> TaskKey p
  -- | Compile step for a source file within a unit.  Pending pool only.
  PendingSource :: UnitName -> OsPath -> TaskKey 'Pending
  -- | Compile step for a module within a unit.  Active\/completed tasks only.
  ResolvedModule :: UnitName -> ModuleName -> TaskKey 'Resolved
  -- | Execute step for a source file within a unit.  Pending pool only.  Resolves to
  -- 'ExecuteModule' once the module's compile task ('ResolvedModule') is known, which
  -- 'ExecuteModule' depends on transitively via the resolution's pending-dep set.
  PendingExecute :: UnitName -> OsPath -> TaskKey 'Pending
  -- | Execute step for a module within a unit.  Active\/completed tasks only.  Depends on
  -- the corresponding 'ResolvedModule' compile task.
  ExecuteModule :: UnitName -> ModuleName -> TaskKey 'Resolved

deriving stock instance Show (TaskKey p)
deriving stock instance Eq (TaskKey p)

instance Ord (TaskKey 'Pending) where
  compare (MetaTask a) (MetaTask b) = compare a b
  compare (MetaTask _) _ = LT
  compare _ (MetaTask _) = GT
  compare (PendingSource u1 p1) (PendingSource u2 p2) = compare (u1, p1) (u2, p2)
  compare (PendingSource _ _) _ = LT
  compare _ (PendingSource _ _) = GT
  compare (PendingExecute u1 p1) (PendingExecute u2 p2) = compare (u1, p1) (u2, p2)

instance Ord (TaskKey 'Resolved) where
  compare (MetaTask a) (MetaTask b) = compare a b
  compare (MetaTask _) _ = LT
  compare _ (MetaTask _) = GT
  compare (ResolvedModule u1 m1) (ResolvedModule u2 m2) = compare (u1, m1) (u2, m2)
  compare (ResolvedModule _ _) _ = LT
  compare _ (ResolvedModule _ _) = GT
  compare (ExecuteModule u1 m1) (ExecuteModule u2 m2) = compare (u1, m1) (u2, m2)

-- | The unit a task key belongs to, in any phase.
taskUnit :: TaskKey p -> UnitName
taskUnit = \case
  MetaTask name -> name
  PendingSource name _ -> name
  ResolvedModule name _ -> name
  PendingExecute name _ -> name
  ExecuteModule name _ -> name

-- | The module a resolved task key refers to, or 'Nothing' for a metadata task.
taskModuleKey :: TaskKey 'Resolved -> Maybe ModuleKey
taskModuleKey = \case
  MetaTask _ -> Nothing
  ResolvedModule unit name -> Just ModuleKey {unit, name}
  ExecuteModule unit name -> Just ModuleKey {unit, name}

-- | Reverse mapping from GHC 'UnitId' to 'UnitName', precomputed from a 'Project'.
unitIdToName :: Project -> Map UnitId UnitName
unitIdToName project =
  Map.fromList [(unitId name, name) | name <- Map.keys project.units]

-- | Look up a 'UnitName' from a 'UnitId', falling back to the raw unit id string.
lookupUnitName :: Map UnitId UnitName -> UnitId -> UnitName
lookupUnitName nameMap uid =
  Map.findWithDefault (UnitName (Text.pack (unitIdString uid))) uid nameMap

-- | Build metadata tasks for the given units.
--
-- Each metadata task depends on the metadata tasks of its home-unit dependencies, given as the
-- second component of each entry.
-- Metadata tasks are created as active (resolved), not pending.
-- The @runMeta@ predicate carries the Phase 0 analysis decision of whether the unit's
-- metadata step must actually run, carried as the task's own 'Task.value' (read directly by
-- dispatch) rather than baked into the key, since it is only meaningful for this task kind.
-- | The scheduler's uniform per-task value type (see 'Test.Scheduler.Task.value'), whose two fields are each
-- meaningful for exactly one 'TaskKey' constructor and a dead placeholder for every other: 'runMeta' for
-- 'MetaTask' (the Phase 0 decision of whether the unit's metadata step must actually run), 'executor' for
-- 'ExecuteModule'\/'PendingExecute' (which persistent executor subprocess, if any, should run this unit's
-- execute tasks -- see 'GhcServer.Build.Executor'). Bundled into one record, rather than a sum type, because the
-- scheduler's generic machinery requires a single uniform value type across every task key.
data TaskValue =
  TaskValue {
    runMeta :: Bool,
    executor :: Maybe ExecutorId
  }
  deriving stock (Eq, Show)

-- | Dead placeholder value for task kinds that don't use either field of 'TaskValue'.
noTaskValue :: TaskValue
noTaskValue = TaskValue {runMeta = False, executor = Nothing}

metadataTasks :: (UnitName -> Bool) -> [(UnitName, [UnitName])] -> [Task TaskKey 'Resolved TaskValue]
metadataTasks runMeta =
  map metaTask
  where
    metaTask (name, depUnits) =
      Task {
        key = MetaTask name,
        deps = Set.fromList [MetaTask dep | dep <- depUnits],
        enabled = True,
        value = noTaskValue {runMeta = runMeta name}
      }

-- | Create pending compile tasks from a unit's source files.
--
-- Each task depends only on its unit's metadata task. Foreign-unit and home-unit
-- module deps are injected later at promotion time using resolution data.
--
-- The @isEnabled@ predicate controls, per source file, whether the task is eligible for
-- promotion. Tasks for implicit dependency units should pass @const False@; the scheduler's
-- 'insertPending' will upgrade the flag with OR if a later batch enables them. A per-source
-- predicate (rather than a single uniform flag) lets callers restrict enabling to specific
-- requested modules -- e.g. 'GhcServer.Build.Classify.compileEnabledSources' -- so that other
-- modules in the same unit are still tracked (for dependency propagation) without being
-- independently promoted.
-- The task's own 'Task.value' is unused ('resolveTask' promotes a pending task by carrying its
-- own 'Task.value' forward, so nothing ever reads this one); it is set to @False@ as a dead
-- placeholder purely to satisfy the uniform value type.
compileTasksFromSources :: UnitName -> (OsPath -> Bool) -> [OsPath] -> [Task TaskKey 'Pending TaskValue]
compileTasksFromSources name isEnabled =
  fmap \ src ->
    Task {
      key = PendingSource name src,
      deps = Set.singleton (MetaTask name),
      enabled = isEnabled src,
      value = noTaskValue
    }

-- | Create pending execute tasks from a unit's source files.
--
-- Each task depends only on its unit's metadata task at creation time; the dependency on the
-- module's own compile task ('ResolvedModule') is added later at promotion time via the pending-dep
-- set produced by 'resolutionsFromModuleMap', mirroring 'compileTasksFromSources'\/'PendingSource'.
--
-- Always enabled: an execute request implies both compiling and running the selected module(s).
-- The @process@ flag (whether this unit's execute tasks should run in a subprocess, see
-- 'GhcServer.Build.Process') is decided once at classification time
-- ('GhcServer.Build.Classify.classifyBuildRequest') and stored directly as the pending task's own
-- 'Task.value', so that 'resolutionsFromModuleMap' can recover it later from the scheduler's pending
-- pool when constructing the corresponding 'ExecuteModule' resolution.
executeTasksFromSources :: UnitName -> Maybe ExecutorId -> [OsPath] -> [Task TaskKey 'Pending TaskValue]
executeTasksFromSources name executor =
  map mkTask
  where
    mkTask src =
      Task {
        key = PendingExecute name src,
        deps = Set.singleton (MetaTask name),
        enabled = True,
        value = noTaskValue {executor}
      }

-- | Resolve dependencies of a module graph node to pending 'TaskKey's.
--
-- Only home-unit dependencies (those present in the module graph) are resolved;
-- external package dependencies are ignored.  The source map provides the
-- 'OsPath' for each home module, needed to construct 'PendingSource' keys.
nodeDepsToTaskKeys ::
  Map UnitId UnitName ->
  Map (UnitId, ModuleName) OsPath ->
  ModuleGraphNode ->
  Set (TaskKey 'Pending)
nodeDepsToTaskKeys nameMap srcMap node =
  Set.fromList
    [PendingSource depName depSrc
#if MIN_VERSION_GLASGOW_HASKELL(9,14,0,0)
      | nk <- mgNodeDependencies True node
#else
      | nk <- nodeDependencies True node
#endif
      , Just depModName <- [nodeKeyModName nk]
      , let depUid = nodeKeyUnitId nk
      , Just depSrc <- [Map.lookup (depUid, depModName) srcMap]
      , let depName = lookupUnitName nameMap depUid
    ]

-- | Resolution map type.
--
-- Maps a pending 'TaskKey' to its resolved key and pending module-level dependencies.  The
-- pending deps are converted to resolved keys during promotion by the scheduler.  There is no
-- value component here: 'resolveTask' promotes a pending task by carrying forward its own
-- 'Task.value' (a 'Bool' whose meaning is contextual on the resolved key's constructor:
-- @process@ for 'ExecuteModule', unused (always 'False') for 'ResolvedModule'), so the resolution
-- entry itself never needs to carry one.
type Resolutions = Map (TaskKey 'Pending) (TaskKey 'Resolved, Set (TaskKey 'Pending))

-- | Key for a module in the build system's module map.
data ModuleKey =
  ModuleKey {
    unit :: UnitName,
    name :: ModuleName
  }
  deriving stock (Eq, Ord, Show)

instance ToJSON ModuleKey where
  toJSON key =
    object ["unit" .= key.unit.text, "name" .= moduleNameString key.name]

instance FromJSON ModuleKey where
  parseJSON =
    withObject "ModuleKey" \ o -> do
      unitStr <- o .: "unit"
      nameStr <- o .: "name"
      pure ModuleKey {unit = UnitName unitStr, name = mkModuleName nameStr}

-- | Per-module build information.
--
-- Stores the module's source file, direct dependencies and the @.dyn_hi@ path.  The scheduler
-- uses @deps@ for ordering; 'buildModuleCachedDeps' assembles the transitive 'CachedDeps' on
-- demand at compile time from these direct deps.
--
-- The module's pending scheduler key is derived from @source@ and the entry's 'ModuleKey' via
-- 'moduleTaskKey' rather than stored, so that no code path has to pattern match a 'TaskKey' it
-- already knows the shape of.
data ModuleInfo =
  ModuleInfo {
    source :: OsPath,
    deps :: Set ModuleKey,
    hiPath :: OsPath
  }

-- | The pending compile task key of a module in the module map.
moduleTaskKey :: ModuleKey -> ModuleInfo -> TaskKey 'Pending
moduleTaskKey key info =
  PendingSource key.unit info.source

-- | The pending execute task key of a module in the module map.
moduleExecuteKey :: ModuleKey -> ModuleInfo -> TaskKey 'Pending
moduleExecuteKey key info =
  PendingExecute key.unit info.source

-- | Build a module map from a 'CachedUnit'.
--
-- Given a 'CachedUnit' (read from @cached_unit.json@), produces a 'Map'
-- 'ModuleKey' 'ModuleInfo' with direct dependencies and @.dyn_hi@ paths.
-- No transitive closure is computed here — that is deferred to
-- 'buildModuleCachedDeps' at compile time.
resolveFromCachedUnit ::
  UnitName ->
  OsPath ->
  CachedUnit ->
  Map ModuleKey ModuleInfo
resolveFromCachedUnit name outputDir cu =
  Map.fromList
    [ (key, ModuleInfo {source = cm.source, deps = directDeps cm, hiPath = moduleHiPath outputDir name modName})
    | (JsonFs modName, cm) <- Map.toList moduleMap
    , let key = ModuleKey {unit = name, name = modName}
    ]
  where
    moduleMap = fromMaybe Map.empty (cu.cache <|> cu.build_plan)

    directDeps :: CachedModule -> Set ModuleKey
    directDeps cm =
      Set.fromList (homeDeps ++ packageDeps')
      where
        homeDeps =
          [ModuleKey {unit = name, name = depMod.raw} | depMod <- cm.modules]

        packageDeps' =
          [ ModuleKey {unit = UnitName (Text.pack (unitIdString pkgId.raw)), name = depMod.raw}
          | CachedPackageDep {id = pkgId, modules = depMods} <- cm.packages
          , depMod <- depMods
          ]

-- | Extension state threaded through the scheduler's @ext@ parameter.
--
-- Accumulates the module map across metadata completion events so that later
-- units can resolve cross-unit dependencies against earlier units' modules.
-- Each entry maps a 'ModuleKey' to its scheduler identity (@'TaskKey' 'Pending'@),
-- direct deps, and @.dyn_hi@ path.
data BuildExt =
  BuildExt {
    -- | Unified module map: scheduler identity, direct deps, and interface path per module.
    -- Built incrementally as each unit's resolutions are computed.
    moduleMap :: Map ModuleKey ModuleInfo,
    -- | Stale closure for the current generation: modules that must be recompiled to satisfy
    -- the request that started it.  Grows as each unit's metadata completes and its Phase 2
    -- analysis runs, so that a later unit's closure can follow edges into an earlier unit's
    -- stale modules.
    --
    -- Reset at generation boundaries (see 'staleGen').  Accumulating it across requests would
    -- make every request inherit its predecessors' staleness, which is precisely how a
    -- long-lived scheduler ends up recompiling the previous request's modules again.
    stale :: Set ModuleKey,
    -- | The generation 'stale' was accumulated for.  When it differs from the scheduler's
    -- current generation, 'stale' is stale in the other sense and must be discarded.
    staleGen :: Generation
  }

-- | Initial (empty) 'BuildExt'.
emptyBuildExt :: BuildExt
emptyBuildExt =
  BuildExt {moduleMap = Map.empty, stale = Set.empty, staleGen = initialGeneration}

-- | Assemble deduplicated, topologically sorted 'CachedDeps' for a module
-- from the full module map.
--
-- Performs a DFS with post-order emission (leaves first) and a visited set
-- for deduplication.  This gives the correct load order for HPT pre-population:
-- a module's dependencies are listed before the module itself.
--
-- The result excludes the target module itself — only its transitive deps.
buildModuleCachedDeps :: Map ModuleKey ModuleInfo -> ModuleKey -> CachedDeps
buildModuleCachedDeps allModules target =
  CachedDeps (snd (go Set.empty roots))
  where
    roots =
      maybe [] (Set.toList . (.deps)) (Map.lookup target allModules)

    go :: Set ModuleKey -> [ModuleKey] -> (Set ModuleKey, [CachedDep])
    go visited [] = (visited, [])
    go visited (k : ks)
      | Set.member k visited = go visited ks
      | otherwise =
        case Map.lookup k allModules of
          Nothing -> go visited' ks
          Just info ->
            let
              (visited'', childDeps) = go visited' (Set.toList info.deps)
              (visited''', siblingDeps) = go visited'' ks
            in
              (visited''', childDeps ++ [mkDep k] ++ siblingDeps)
      where
        visited' = Set.insert k visited

    mkDep key =
      CachedDep {
        name = JsonFs key.name,
        package = JsonFs (unitId key.unit)
      }

-- | Like 'buildModuleCachedDeps', but also appends the target module itself as the final entry, so that a
-- fresh session that never ran this module's own compile task (e.g. a subprocess evaluator, see
-- "GhcServer.Build.Process") can restore its interface and bytecode from cache via the same mechanism used for
-- its dependencies. Idempotent when the module is already present in the HPT (the ordinary in-process case): the
-- cache-loading machinery only loads modules missing from the HPT.
buildModuleCachedDepsWithSelf :: Map ModuleKey ModuleInfo -> ModuleKey -> CachedDeps
buildModuleCachedDepsWithSelf allModules target =
  CachedDeps (deps ++ [CachedDep {name = JsonFs target.name, package = JsonFs (unitId target.unit)}])
  where
    CachedDeps deps = buildModuleCachedDeps allModules target

-- | Derive scheduler 'Resolutions' from a module map, restricted to the stale closure.
--
-- Compile resolutions are created only for modules in the @stale@ set (the Phase 2 closure result) --
-- up-to-date modules keep no resolution, so their pending tasks are never promoted and no compile
-- task ever exists for them.  Dependency sets are likewise filtered to stale modules: a stale
-- module's up-to-date dependencies need no scheduling edge because their artifacts already exist
-- on disk (HPT pre-population loads them via 'buildModuleCachedDeps' at compile time).
--
-- Execute resolutions are created for every module; the dependency on the module's own compile
-- task is only included when that module is stale (otherwise its artifacts are current and the
-- execute task can run immediately). Every execute resolution's value is the corresponding
-- 'PendingExecute' task's own @process@ flag, recovered from the scheduler's pending pool
-- (@pending@) rather than recomputed here -- it was decided once at classification time
-- ('GhcServer.Build.Classify.classifyBuildRequest') and stashed on the pending task by
-- 'executeTasksFromSources'; promotion itself would otherwise discard it (a promoted task's
-- value always comes from its 'Resolution', never from the pending task it replaces), so this
-- is the only place it can be carried forward. A module with no matching 'PendingExecute' entry
-- (never requested for execution) simply gets 'False', which is never consulted anyway.
resolutionsFromModuleMap ::
  Set ModuleKey ->
  Map ModuleKey ModuleInfo ->
  Map ModuleKey ModuleInfo ->
  Resolutions
resolutionsFromModuleMap stale priorModules newModules =
  Map.fromList (compileEntries ++ executeEntries)
  where
    allModules = Map.union newModules priorModules

    compileEntries =
      [ (moduleTaskKey key info, (ResolvedModule key.unit key.name, depTasks info))
      | (key, info) <- Map.toList newModules
      , Set.member key stale
      ]

    executeEntries =
      [ (moduleExecuteKey key info, (ExecuteModule key.unit key.name, execDeps key info))
      | (key, info) <- Map.toList newModules
      ]

    execDeps key info
      | Set.member key stale = Set.singleton (moduleTaskKey key info)
      | otherwise = Set.empty

    depTasks :: ModuleInfo -> Set (TaskKey 'Pending)
    depTasks info =
      Set.fromList
        [ moduleTaskKey depKey depInfo
        | depKey <- Set.toList info.deps
        , Set.member depKey stale
        , Just depInfo <- [Map.lookup depKey allModules]
        ]
