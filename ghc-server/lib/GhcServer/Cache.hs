-- | Cache logic for the standalone GHC server.
--
-- After a successful metadata step, cache data is written to @cache/@ so that subsequent builds can restore the
-- 'WorkerState' (HUG, module graph) without rerunning metadata from scratch.
--
-- The cache format mirrors what Buck writes for the persistent worker:
--
-- - Per unit: a 'CachedUnit' JSON with the module graph cache and a @unit_args@ file with GHC CLI flags.
--
-- On restore, 'loadCachedUnits' rebuilds the HUG and module graph from the cached data.
-- @Opt_ForceRecomp@ disabled, so GHC's native recompilation checking skips modules whose @.hi@ files are up to date.
--
-- TODO refactor and rewrite docs
module GhcServer.Cache where

import Control.Monad (filterM, join)
import Control.Monad.Extra (mapMaybeM, whenM, whenMaybeM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT (..))
import qualified Data.Aeson as Aeson
import Data.Aeson (eitherDecodeFileStrict')
import Data.Bifunctor (first)
import qualified Data.Map.Strict as Map
import Data.Map.Strict ((!?))
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Text as Text
import Data.Text (Text)
import GHC.Data.Graph.Directed (Graph, reachablesG, topologicalSortG)
import qualified GHC.Data.Graph.Directed as Graph (Node (..))
import GHC.Unit (ModuleName, stringToUnit)
import GHC.Unit.Types (toUnitId)
import GhcServer.Data.BuildCache (BuildCache (..))
import GhcServer.Data.Unit (Project (..), Unit (..), UnitCache (..), UnitDepNode, UnitName (..), moduleHiPath)
import qualified System.Directory.OsPath as OsPath
import System.Directory.OsPath (createDirectoryIfMissing, doesFileExist)
import qualified System.File.OsPath as OsPath
import System.OsPath (OsPath, (</>))
import System.OsPath.Extra (fromOsPath, osp)
import Types.BuildPlan.Incremental (BuildPlanPath (..))
import Types.ByteString (toUtf8Lazy)
import Types.CachedDeps (CachedBuildPlan (..), CachedBuildPlans (..), CachedUnit (..), JsonFs (..))
import Types.Log (Logger (..))

-- | Write the @dep_units.json@ file and return its path, if there are dep plans to write.
writeDepUnits :: UnitCache -> Maybe CachedBuildPlans -> IO (Maybe OsPath)
writeDepUnits unitCache =
  traverse \ plans -> do
    Aeson.encodeFile (fromOsPath unitCache.depUnitsPath) plans
    pure unitCache.depUnitsPath

-- | Decode the build plan JSON and write @cached_unit.json@ with injected @unit_args@ and @dep_units@ paths.
writeCachedUnit :: UnitCache -> Maybe OsPath -> OsPath -> ExceptT Text IO ()
writeCachedUnit unitCache depsFile buildPlan = do
  cachedUnit <- ExceptT (first decodeError <$> eitherDecodeFileStrict' (fromOsPath buildPlan))
  liftIO $ OsPath.writeFile path $ Aeson.encode (amend cachedUnit)
  where
    amend u =
      u {
        unit_args = Just unitCache.unitArgsPath,
        dep_units = depsFile
      }

    path = unitCache.dir </> [osp|cached_unit.json|]

    decodeError err = Text.pack ("Failed to decode build plan for cache (" ++ fromOsPath buildPlan ++ "): " ++ err)

-- | Write the cache files for a unit after a successful metadata step.
--
-- Writes:
--
-- 1. @unit_args@ GHC CLI flags, one per line.
-- 2. @dep_units.json@ 'CachedBuildPlans' for the unit's transitive dep units (from the pre-computed graph query).
-- 3. @cached_unit.json@ 'CachedUnit' with the @cache@ field from the build plan JSON, the @unit_args@ path, and
--    the @dep_units@ path.
--
-- The 'CachedUnit' is constructed by decoding the build plan JSON that 'computeMetadata' wrote (which contains a
-- @cache@ field compatible with 'CachedUnit'), then setting the @unit_args@ and @dep_units@ paths.
--
-- TODO unused logger
writeUnitCache :: Logger -> UnitCache -> Maybe CachedBuildPlans -> BuildPlanPath -> [String] -> ExceptT Text IO ()
writeUnitCache _logger unitCache depPlans buildPlanPath ghcOptions =
  whenM (liftIO (doesFileExist buildPlanPath.path)) do
    depsFile <- liftIO do
      createDirectoryIfMissing True unitCache.dir
      OsPath.writeFile unitCache.unitArgsPath (toUtf8Lazy (unlines ghcOptions))
      writeDepUnits unitCache depPlans
    writeCachedUnit unitCache depsFile buildPlanPath.path

cacheExists :: UnitCache -> IO Bool
cacheExists unitCache =
  doesFileExist unitCache.cachedUnitPath

-- | Order the transitive dependencies of a unit for loading by 'loadCachedUnits'.
--
-- Returns nodes in dependency order (leaves first): each unit appears after all
-- units it depends on.  This ordering is required because 'loadCachedUnits'
-- processes plans sequentially and each unit's 'initUnits' call expects all of
-- its @-package-id@ targets to already be in the home unit graph.
--
-- 'reachablesG' alone is insufficient: it returns nodes in DFS pre-order
-- (roots first), and reversing that is still wrong for DAGs with shared
-- ancestors — e.g.\ for @unit3 → {unit1, unit2} → unit0@, reversing the DFS
-- pre-order may yield @[unit2, unit0, unit1]@, loading @unit2@ before @unit0@.
-- The full graph's topological sort handles shared ancestors correctly.
depLoadOrder :: Ord key => Graph (Graph.Node key payload) -> Graph.Node key payload -> [Graph.Node key payload]
depLoadOrder depGraph root =
  [ node
  | node <- reverse (topologicalSortG depGraph)
  , Set.member node.node_key reachableNames
  ]
  where
    reachableNames =
      Set.fromList
        [ node.node_key
        | node <- reachablesG depGraph [root]
        , node.node_key /= root.node_key
        ]

-- | Build 'CachedBuildPlans' for a unit's transitive dependency units.
--
-- This is equivalent to what Buck does before executing metadata:
--
-- > transitive_deps.project_as_json("dep_units")
-- > actions.write_json(dep_units_file, dep_units)
--
-- Uses the pre-computed unit dependency graph to query the transitive closure
-- via 'depLoadOrder' and collect cache paths for all dep units that have
-- @cached_unit.json@ files.
buildDepPlans :: Graph UnitDepNode -> Unit -> IO CachedBuildPlans
buildDepPlans depGraph unit =
  CachedBuildPlans . fmap plan <$> filterM (doesFileExist . (.node_payload)) (depLoadOrder depGraph selfNode)
  where
    plan node =
      CachedBuildPlan {
        name = JsonFs (toUnitId (stringToUnit node.node_key.string)),
        build_plan = node.node_payload
      }

    selfNode = Graph.DigraphNode {
      node_payload = unit.cache.cachedUnitPath,
      node_key = unit.name,
      node_dependencies = []
    }


-- | If the unit's @cached_unit.json@ exists from a prior build, return its path.
--
-- This is used before compilation to let 'withGhcMakeModule' restore the home unit via 'loadHomeUnit'.
loadHomeUnitCache :: UnitCache -> IO (Maybe OsPath)
loadHomeUnitCache unitCache =
  whenMaybeM (doesFileExist unitCache.cachedUnitPath) do
    pure unitCache.cachedUnitPath
-- | Check whether a module's interface file (@.dyn_hi@) exists.
--
-- The interface file is the reliable indicator that a module was compiled in a prior build.
interfaceExists :: OsPath -> UnitName -> ModuleName -> IO Bool
interfaceExists outputDir name modName =
  doesFileExist (moduleHiPath outputDir name modName)

-- | Compute the set of all units with cache from a prior build.
cachedUnitsForProject :: Project -> IO (Set UnitName)
cachedUnitsForProject project =
  Set.fromList <$> mapMaybeM existing (Map.elems project.units)
  where
    existing unit =
      whenMaybeM (cacheExists unit.cache) (pure unit.name)

-- | Load the 'CachedUnit' from @cached_unit.json@, if it exists.
loadCachedUnit :: UnitCache -> ExceptT Text IO (Maybe CachedUnit)
loadCachedUnit unitCache =
  whenMaybeM (liftIO (OsPath.doesFileExist unitCache.cachedUnitPath)) do
    ExceptT (first decodeError <$> eitherDecodeFileStrict' path)
  where
    decodeError err = Text.pack ("Failed to decode cached unit " ++ path ++ ": " ++ err)

    path = fromOsPath unitCache.cachedUnitPath

-- | Construct a 'BuildCache' from a 'Project' and output directory.
mkBuildCache :: OsPath -> Project -> BuildCache
mkBuildCache _outputDir project =
  BuildCache {
    unitCached = \ name ->
      maybe (pure False) (cacheExists . (.cache)) (project.units !? name),
    loadUnit = \ name ->
      join <$> traverse (loadCachedUnit . (.cache)) (project.units !? name)
  }
