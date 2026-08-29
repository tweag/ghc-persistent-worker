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
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import GHC (ModuleName)
import GhcServer.Build.Diff (UnitDiff (..), computeUnitDiff)
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
  ScheduleRequest (..),
  UnitRequest (..),
  effectiveUnitName,
  isCompileRequest,
  )
import GhcServer.Data.Unit (ClientModule (..), Project (..), Unit (..), UnitName (..))
import GhcServer.Path (fp)
import GhcServer.Scheduler (Phase (..), Task (..))
import System.FilePath (takeBaseName)
import System.OsPath (OsPath)

-- | Determine the effective request for each unit, expanding an empty 'ScheduleRequest'
-- to 'UnitAll' for every unit and adding transitive deps.
--
-- For explicit requests, the order from the CLI is preserved. Implicit deps are
-- prepended in the order they are discovered by walking explicit targets' dependency trees.
effectiveRequests :: Project -> ScheduleRequest -> [EffectiveUnit]
effectiveRequests project request
  | null request.steps = [Explicit name UnitAll | name <- Map.keys project.units]
  | otherwise = implicitEntries ++ map (uncurry Explicit) request.steps
  where
    requestMap = Map.fromList request.steps

    implicitEntries =
      [
        ImplicitDep name
        | name <- Set.toAscList (transitiveDeps project (map fst request.steps))
        , not (Map.member name requestMap)
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
  diffs <- Map.fromList <$> traverse unitDiff reqs
  modifyMVar_ env.diff (pure . Map.union diffs)
  let
    runMeta name = maybe True (.runMeta) (Map.lookup name diffs)
    metaTasks = metadataTasks env.project runMeta request.rebuild (map effectiveUnitName reqs)
  pure (metaTasks, pendingTasks)
  where
    reqs = effectiveRequests env.project request

    unitDiff eu = do
      let name = effectiveUnitName eu
      d <- case Map.lookup name env.project.units of
        Just unit -> computeUnitDiff env.outputDir request.rebuild (forceAll eu) unit
        Nothing -> pure UnitDiff {
          changed = Set.empty,
          newDigests = Map.empty,
          oldModules = Map.empty,
          runMeta = True,
          forceAll = False
        }
      pure (name, d)

    -- @--recompile@ forces explicitly named units' entire module sets into the stale closure.
    forceAll = \case
      Explicit _ req -> request.recompile && isCompileRequest req
      ImplicitDep _ -> False

    rebuild = request.recompile

    pendingTasks = concatMap unitCompileTasks reqs ++ concatMap unitExecuteTasks reqs

    unitCompileTasks eu =
      case Map.lookup (effectiveUnitName eu) env.project.units of
        Just unit ->
          compileTasksFromSources
            (effectiveUnitName eu)
            rebuild
            (compileEnabledSources unit eu)
            unit.sources
        Nothing -> []

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
    -- whole unit. Implicit deps are controlled by the @recompile@ parameter, as before.
    compileEnabledSources :: Unit -> EffectiveUnit -> OsPath -> Bool
    compileEnabledSources unit = \case
      Explicit _ (UnitModules mods) -> (`elem` selectedSources unit mods)
      Explicit _ (UnitExecuteModules mods) -> (`elem` selectedSources unit mods)
      Explicit _ req -> const (isCompileRequest req)
      ImplicitDep _ -> const rebuild

    -- | Execute tasks are only produced for units explicitly requested with 'UnitExecute'\/
    -- 'UnitExecuteModules' -- implicit transitive deps and other request kinds never trigger execution.
    unitExecuteTasks eu =
      case (eu, Map.lookup (effectiveUnitName eu) env.project.units) of
        (Explicit name UnitExecute, Just unit) ->
          executeTasksFromSources name rebuild unit.sources
        (Explicit name (UnitExecuteModules mods), Just unit) ->
          executeTasksFromSources name rebuild (selectedSources unit mods)
        _ -> []

    selectedSources :: Unit -> [ClientModule] -> [OsPath]
    selectedSources unit mods =
      [src | src <- unit.sources, takeBaseName (fp src) `elem` map (.string) mods]

-- | Collect a 'BuildResult' from the scheduler's completed and failure sets.
collectBuildResult :: Set (TaskKey 'Resolved) -> Map (TaskKey 'Resolved) String -> BuildResult
collectBuildResult _completed failures =
  BuildResult {
    success = null metaErrs && null compErrs,
    metadataErrors = metaErrs,
    compileErrors = compErrs
  }
  where
    metaErrs = [(name, msg) | (MetaTask name, msg) <- Map.toList failures]

    compErrs = [(name, modName, msg) | (ResolvedModule name modName, msg) <- Map.toList failures]

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
    compileErrors :: [(UnitName, ModuleName, String)]
  }
  deriving stock (Show)
