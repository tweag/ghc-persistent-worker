module GhcServer.Project where

import Control.Monad (when)
import Data.Aeson (eitherDecode')
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, mapMaybe)
import GHC (ModuleName, mkModuleName)
import GHC.Data.Graph.Directed (graphFromEdgedVerticesOrd)
import qualified GHC.Data.Graph.Directed as Graph (Node (..))
import GhcServer.Data.Unit (Project (..), Unit (..), UnitCache (..), UnitDepNode, UnitName (..), mkUnitCache)
import GhcServer.Data.UnitConfig (UnitConfig (..))
import GhcServer.Path (isHaskellSource)
import System.Directory.OsPath (createDirectoryIfMissing, doesFileExist, listDirectory)
import qualified System.File.OsPath as OsPath
import System.OsPath (OsPath, dropExtension, osp, (</>))
import System.OsPath.Extra (fromOsPath)

-- | Build a 'UnitDepNode' for the dependency graph from a 'Unit'.
--
-- The node payload is the unit's own @cached_unit.json@ path; edges point to its direct dep unit names.
unitDepNode :: Unit -> UnitDepNode
unitDepNode unit =
  Graph.DigraphNode {
    node_payload = unit.cache.cachedUnitPath,
    node_key = unit.name,
    node_dependencies = unit.depUnits
  }

-- | Read and parse a @unit.json@ file.
readUnitConfig :: OsPath -> IO UnitConfig
readUnitConfig path = do
  bytes <- OsPath.readFile path
  either fail pure (eitherDecode' bytes)

-- | Derive a module name and source path from a directory entry of a @unit.json@ unit.
--
-- Units without a Cabal description keep all sources directly in the unit directory, so the module
-- name is the file's base name.  Non-Haskell entries are skipped.
unitModule :: OsPath -> OsPath -> Maybe (ModuleName, OsPath)
unitModule dir entry
  | isHaskellSource entry
  = Just (mkModuleName (fromOsPath (dropExtension entry)), dir </> entry)
  | otherwise
  = Nothing

-- | Discover a single unit from a directory that contains a @unit.json@ file.
--
-- Creates the unit's output and temp directories if the unit is found.
discoverUnit :: OsPath -> OsPath -> OsPath -> OsPath -> IO (Maybe Unit)
discoverUnit projectRoot outputDir tmpDir name = do
  doesFileExist configFile >>= \case
    False -> pure Nothing
    True -> do
      createDirectoryIfMissing True (outputDir </> name)
      createDirectoryIfMissing True (tmpDir </> name)
      config <- readUnitConfig configFile
      entries <- listDirectory dir
      let modules = mapMaybe (unitModule dir) entries
          unitName = UnitName (fromOsPath name)
      pure (Just Unit {
        name = unitName,
        dir,
        ghcArgs = config.args,
        modules,
        depUnits = UnitName <$> config.deps,
        extDeps = [],
        cache = mkUnitCache projectRoot unitName
      })
  where
    dir = projectRoot </> name
    configFile = dir </> [osp|unit.json|]

-- | Discover all units in the project root, creating output and temp directories.
--
-- Throws if no units are found.
discoverProject :: OsPath -> OsPath -> OsPath -> IO Project
discoverProject projectRoot outputDir tmpDir = do
  createDirectoryIfMissing True outputDir
  createDirectoryIfMissing True tmpDir
  entries <- listDirectory projectRoot
  units <- catMaybes <$> traverse (discoverUnit projectRoot outputDir tmpDir) entries
  when (null units) do
    fail ("No units found in project root: " ++ fromOsPath projectRoot)
  pure Project {
    units = Map.fromList [(u.name, u) | u <- units],
    depGraph = graphFromEdgedVerticesOrd (map unitDepNode units)
  }
