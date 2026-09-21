-- | Build system logic: request expansion, task classification, and result collection.
--
-- This module implements the external build coordinator's perspective — deciding /what/ to
-- build and how to map user requests into scheduler tasks.  It corresponds to the role
-- an external build system (like Buck) plays: expanding targets into effective work items,
-- classifying them into metadata and compile tasks, and interpreting completed scheduler
-- state as a build result.
--
-- This module does not interact with GHC, the worker, or the cache.
module GhcServer.Build.Classify where

import Control.Concurrent.MVar (modifyMVar_)
import Data.Either (partitionEithers)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Text as Text
import GHC (ModuleName)
import GhcServer.Build.Diff (ProcessScope (..), UnitDiff (..), computeUnitDiff)
import GhcServer.Build.Schedule (
  BuildStatus,
  TaskKey (..),
  compileTasksFromSources,
  executeTasksFromSources,
  metadataTasks,
  )
import GhcServer.Data.BuildEnv (BuildEnv (..))
import GhcServer.Data.Request (
  EffectiveUnit (..),
  EffectiveUnits (..),
  ScheduleRequest (..),
  UnitRequest (..),
  UnitScope (..),
  isCompileRequest,
  )
import GhcServer.Data.Unit (ClientModule, Project (..), Unit (..), clientModuleName, unitSources)
import GhcServer.Log (emitLog)
import System.OsPath (OsPath)
import System.OsPath.Extra (fromOsPath)
import Test.Scheduler (Phase (..), Task (..))
import Types.Api (UnitName (..))

-- | Determine the effective request for each unit, expanding an empty 'ScheduleRequest'
-- to 'UnitAll' for every unit and adding transitive deps.
--
-- For explicit requests, the order from the CLI is preserved. Implicit deps are
-- prepended in the order they are discovered by walking explicit targets' dependency trees.
--
-- Each entry is resolved against the project here, so downstream steps operate on 'Unit' values
-- instead of re-querying the project map.  Names with no matching unit are kept aside in
-- 'EffectiveUnits.unknown'.
effectiveRequests :: Project -> ScheduleRequest -> EffectiveUnits
effectiveRequests project request
  | null request.steps =
    EffectiveUnits {
      resolved = [EffectiveUnit {unit, scope = Explicit UnitAll} | unit <- Map.elems project.units],
      unknown = []
    }
  | otherwise =
    EffectiveUnits {
      resolved = implicitEntries ++ explicitEntries,
      unknown = unknownNames
    }
  where
    (unknownNames, explicitEntries) =
      partitionEithers
        [ maybe (Left name) (\ unit -> Right EffectiveUnit {unit, scope = Explicit req}) (Map.lookup name project.units)
        | (name, req) <- request.steps
        ]

    requestMap = Map.fromList request.steps

    implicitEntries =
      [
        EffectiveUnit {unit, scope = ImplicitDep}
        | name <- Set.toAscList (transitiveDeps project (map fst request.steps))
        , not (Map.member name requestMap)
        , Just unit <- [Map.lookup name project.units]
      ]

-- | Classify a 'ScheduleRequest' into active metadata tasks and pending compile tasks.
--
-- Runs the Phase 0 analysis ('computeUnitDiff') for every effective unit: sources are diffed
-- against the stored digest record and the previous module graph is reloaded from disk.  The
-- results are stored in 'BuildEnv.diff' for consumption at metadata-completion time (Phase 2)
-- and digest-commit time.  Whether a unit's metadata step runs is decided here and embedded as
-- the @runMeta@ flag in the task value; dispatch executes it blindly.
--
-- Whether to promote compile tasks is decided by the 'enabled' flag set here (request scope)
-- in combination with the stale closure computed in Phase 2 (only stale modules receive
-- resolutions).
classifyBuildRequest ::
  BuildEnv ->
  ScheduleRequest ->
  IO ([Task TaskKey 'Resolved BuildStatus], [Task TaskKey 'Pending BuildStatus])
classifyBuildRequest env request = do
  diffs <- Map.fromList <$> traverse unitDiff reqs.resolved
  modifyMVar_ env.diff (pure . Map.union diffs)
  let
    runMeta name = maybe True (.runMeta) (Map.lookup name diffs)
    metaTasks = metadataTasks runMeta metaSpecs
  pure (metaTasks, pendingTasks)
  where
    reqs = effectiveRequests env.project request

    -- This unit's @--process@ scope for the current batch (see 'ProcessScope'), recorded in its 'UnitDiff' and
    -- resolved into 'ModuleKey's once its modules are known (see 'GhcServer.Build.Propagate.propagateCompletion').
    -- Only meaningful together with an execute request -- @--process@ on a plain compile/metadata request has
    -- nothing to apply to.
    processScope eu
      | not request.process = ProcessNone
      | otherwise = case eu.scope of
        Explicit UnitExecute -> ProcessAll
        Explicit (UnitExecuteModules mods) -> ProcessModules (Set.fromList mods)
        _ -> ProcessNone

    -- Unknown unit names still get a metadata task, which fails at dispatch with a diagnostic
    -- naming the unit; they have no dependencies to order against.
    metaSpecs =
      [(eu.unit.name, eu.unit.depUnits) | eu <- reqs.resolved]
      ++ [(name, []) | name <- reqs.unknown]

    unitDiff eu = do
      d <- computeUnitDiff env.outputDir request.rebuild (forceAll eu.scope) (processScope eu) eu.unit
      emitLog env.instrChan (Text.unpack eu.unit.name.text ++ ":diff") "debug" $
        "classify: runMeta=" ++ show d.runMeta
          ++ " forceAll=" ++ show d.forceAll
          ++ " changed=" ++ show (map fromOsPath (Set.toList d.changed))
          ++ " oldModules=" ++ show (Map.keys d.oldModules)
      pure (eu.unit.name, d)

    -- @--recompile@ forces explicitly named units' entire module sets into the stale closure.
    forceAll = \case
      Explicit req -> request.recompile && isCompileRequest req
      ImplicitDep -> False

    -- @--recompile@ additionally enables implicit dependency units' compile tasks, so that a
    -- forced rebuild of a target also covers the units it is built against.
    enableImplicitDeps = request.recompile

    pendingTasks = concatMap unitCompileTasks reqs.resolved ++ concatMap unitExecuteTasks reqs.resolved

    unitCompileTasks eu =
      compileTasksFromSources eu.unit.name (compileEnabledSources eu) (unitSources eu.unit)

    -- | Whether a source file should be enabled for compilation, given an effective unit
    -- request.
    --
    -- 'UnitModules'\/'UnitExecuteModules' restrict enabling to the sources of the selected
    -- modules only, mirroring 'selectedSources' below (used for the analogous execute-task
    -- restriction) -- without this, every source of the unit would be enabled regardless of
    -- which modules were actually requested, causing unrelated modules to be scheduled
    -- (and, when the same unit receives multiple concurrent per-module requests, e.g. from
    -- the UI's project-root or unit-header \'build\' action, redundantly re-resolving
    -- already-dispatched modules into duplicate scheduler entries).
    -- Other explicit request kinds enable either all sources or none, uniformly for the
    -- whole unit. Implicit deps are controlled by @--recompile@, as before.
    compileEnabledSources :: EffectiveUnit -> OsPath -> Bool
    compileEnabledSources eu = case eu.scope of
      Explicit (UnitModules mods) -> (`elem` selectedSources eu.unit mods)
      Explicit (UnitExecuteModules mods) -> (`elem` selectedSources eu.unit mods)
      Explicit req -> const (isCompileRequest req)
      ImplicitDep -> const enableImplicitDeps

    -- | Execute tasks are only produced for units explicitly requested with 'UnitExecute'\/
    -- 'UnitExecuteModules' -- implicit transitive deps and other request kinds never trigger execution.
    unitExecuteTasks eu = case eu.scope of
      Explicit UnitExecute -> executeTasksFromSources eu.unit.name (unitSources eu.unit)
      Explicit (UnitExecuteModules mods) -> executeTasksFromSources eu.unit.name (selectedSources eu.unit mods)
      _ -> []

-- | The source files of the unit's modules whose names the client selected.
--
-- Matches on the module names recorded at project discovery time rather than on source file
-- base names, which coincide only for modules that live at the root of a source directory.
selectedSources :: Unit -> [ClientModule] -> [OsPath]
selectedSources unit mods =
  [src | (modName, src) <- unit.modules, elem modName selected]
  where
    selected = clientModuleName <$> mods

-- | Collect a 'BuildResult' from the scheduler's failure set.
collectBuildResult :: Map (TaskKey 'Resolved) String -> BuildResult
collectBuildResult failures =
  BuildResult {
    success = null metaErrs && null compErrs && null execErrs,
    metadataErrors = metaErrs,
    compileErrors = compErrs,
    executeErrors = execErrs
  }
  where
    metaErrs = [(name, msg) | (MetaTask name, msg) <- Map.toList failures]

    compErrs = [(name, modName, msg) | (ResolvedModule name modName, msg) <- Map.toList failures]

    execErrs = [(name, modName, msg) | (ExecuteModule name modName, msg) <- Map.toList failures]

-- | Compute the transitive closure of unit dependencies from a set of root unit names.
transitiveDeps :: Project -> [UnitName] -> Set UnitName
transitiveDeps project =
  foldl' addDeps Set.empty
  where
    addDeps acc name
      | Set.member name acc = acc
      | otherwise = case Map.lookup name project.units of
        Nothing -> acc
        Just unit -> foldl' addDeps (Set.insert name acc) unit.depUnits

-- | A build result.
data BuildResult =
  BuildResult {
    success :: Bool,
    metadataErrors :: [(UnitName, String)],
    compileErrors :: [(UnitName, ModuleName, String)],
    executeErrors :: [(UnitName, ModuleName, String)]
  }
  deriving stock (Show)
