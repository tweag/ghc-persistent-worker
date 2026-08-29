-- | Incremental recompilation analysis: source diffing, module graph diffing, and staleness closure.
--
-- This module implements the analysis phases of the build pipeline (see the recompilation design):
--
-- * Phase 0: 'computeUnitDiff' hashes every source file of a unit (mtime-gated) and diffs against the
--   digest record stored by the previous successful build, producing the set of changed source paths.
-- * Phase 2: 'moduleGraphDelta' compares the module graph from before the metadata refresh with the
--   refreshed one, catching structural changes (added\/removed imports); 'staleClosure' computes the
--   full downstream reachability of all changed\/structurally-different modules.
--
-- All decisions about what to recompile are made here and in the scheduling layer; dispatch executes
-- the resulting instructions blindly.
module GhcServer.Build.Diff where

import Control.Monad (when)
import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Aeson as Aeson
import Data.String (fromString)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import GHC.Fingerprint (getFileHash)
import GHC.Generics (Generic)
import GhcServer.Build.Schedule (ModuleInfo (..), ModuleKey (..), TaskKey (..), resolveFromCachedUnit)
import GhcServer.Cache (loadCachedUnit)
import GhcServer.Data.Unit (Unit (..), UnitCache (..), UnitName)
import GhcServer.Path (fp)
import System.Directory.OsPath (createDirectoryIfMissing, doesFileExist, getFileSize, getModificationTime, removeFile)
import System.OsPath (OsPath)
import System.OsPath.Extra (fromOsPath, toOsPath)
import Types.BuildPlan.Incremental (BuckHash (..), BuckHashes (..))

-- | Digest of a single source file, stored in the per-unit digest record.
--
-- The @mtime@ gates hashing: when the stored mtime matches the file's current mtime, the stored
-- @digest@ is reused without reading the file.  When the mtime differs, the file is re-hashed and
-- compared by content, so a touch without an edit does not invalidate anything.
data SourceDigest =
  SourceDigest {
    -- | Modification time at hashing time, serialized via 'show' (only compared for equality).
    mtime :: String,
    -- | Content digest in the form @md5:size@.
    digest :: String
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Per-unit digest record: one entry per source file, keyed by absolute path.
type DigestRecord = Map FilePath SourceDigest

-- | The result of the Phase 0\/Phase 2 pre-analysis for one unit, computed once per batch at
-- classification time and consumed when the unit's metadata task completes.
data UnitDiff =
  UnitDiff {
    -- | Source files whose content changed (or that are new) since the last successful build.
    changed :: Set OsPath,
    -- | Fresh digests for all of the unit's sources, to be committed after a successful build.
    newDigests :: DigestRecord,
    -- | The unit's module graph from before the metadata refresh, reloaded from the on-disk
    -- @cached_unit.json@.  Empty for units that have never been built.
    oldModules :: Map ModuleKey ModuleInfo,
    -- | Whether the metadata step must run for this unit (decided here, executed blindly).
    runMeta :: Bool,
    -- | Whether the unit's entire module set is forced into the stale closure (@--recompile@).
    forceAll :: Bool
  }

-- | Hash a single source file, reusing the stored digest when the mtime is unchanged.
digestSource :: DigestRecord -> OsPath -> IO (FilePath, SourceDigest)
digestSource old src = do
  currentMtime <- show <$> getModificationTime src
  case Map.lookup key old of
    Just stored | stored.mtime == currentMtime -> pure (key, stored)
    _ -> do
      hash <- getFileHash (fp src)
      size <- getFileSize src
      pure (key, SourceDigest {mtime = currentMtime, digest = show hash ++ ":" ++ show size})
  where
    key = fp src

-- | Compute fresh digests for a unit's sources and the set of paths whose content changed.
--
-- A source counts as changed when it has no stored digest (new file, or no record from a prior
-- build) or when its content digest differs from the stored one.
digestSources :: DigestRecord -> [OsPath] -> IO (Set OsPath, DigestRecord)
digestSources old srcs = do
  entries <- traverse (digestSource old) srcs
  let
    fresh = Map.fromList entries
    changedPaths = Set.fromList
      [ src
      | src <- srcs
      , let key = fp src
      , ((.digest) <$> Map.lookup key old) /= ((.digest) <$> Map.lookup key fresh)
      ]
  pure (changedPaths, fresh)

-- | Load the unit's digest record from the previous successful build, if present.
loadDigestRecord :: UnitCache -> IO DigestRecord
loadDigestRecord cache =
  doesFileExist cache.sourceDigestsPath >>= \case
    False -> pure Map.empty
    True ->
      Aeson.eitherDecodeFileStrict' (fromOsPath cache.sourceDigestsPath) >>= \case
        Left _ -> pure Map.empty
        Right record -> pure record

-- | Persist a unit's digest record for the next build.
writeDigestRecord :: UnitCache -> DigestRecord -> IO ()
writeDigestRecord cache record =
  Aeson.encodeFile (fromOsPath cache.sourceDigestsPath) record

-- | Write the unit's current source digests in the Buck source-hashes format consumed by the
-- core's incremental metadata path, and return the file's path.
--
-- This replaces the @buck_source_hashes@ env var indirection: the server computes the digests
-- itself (Phase 0) and feeds them to 'Internal.Metadata.computeMetadata' directly.
writeSourceHashes :: UnitCache -> DigestRecord -> IO OsPath
writeSourceHashes cache record = do
  createDirectoryIfMissing True cache.dir
  Aeson.encodeFile (fromOsPath cache.sourceHashesPath) hashes
  pure cache.sourceHashesPath
  where
    hashes = BuckHashes {
      version = 1,
      digests = [BuckHash {path = toOsPath p, digest = fromString d.digest} | (p, d) <- Map.toList record]
    }

-- | Run the Phase 0 analysis for a unit: diff its sources against the stored digest record and
-- reload its previous module graph from disk.
--
-- @rebuild@ discards the stored digest record first, treating every source as changed.
-- @forceAll@ (from @--recompile@ for explicitly named units) is recorded for the closure phase.
computeUnitDiff :: OsPath -> Bool -> Bool -> Unit -> IO UnitDiff
computeUnitDiff outputDir rebuild forceAll unit = do
  when rebuild do
    exists <- doesFileExist unit.cache.sourceDigestsPath
    when exists (removeFile unit.cache.sourceDigestsPath)
  old <- if rebuild then pure Map.empty else loadDigestRecord unit.cache
  (changed, newDigests) <- digestSources old unit.sources
  cached <- doesFileExist unit.cache.cachedUnitPath
  oldModules <- if cached
    then either (const Map.empty) (maybe Map.empty (resolveFromCachedUnit unit.name outputDir)) <$> loadCachedUnit unit.cache
    else pure Map.empty
  pure UnitDiff {
    changed,
    newDigests,
    oldModules,
    runMeta = rebuild || not cached || not (Set.null changed),
    forceAll
  }

-- | Phase 2, first half: modules whose dependency edges differ between the old and refreshed
-- module graphs, including modules that are new in the refreshed graph.
--
-- Removed modules are not reported: a dependent that dropped an import has a differing edge set
-- itself, and a dependent that kept the import cannot pass the metadata step anyway.
moduleGraphDelta :: Map ModuleKey ModuleInfo -> Map ModuleKey ModuleInfo -> Set ModuleKey
moduleGraphDelta old new =
  Set.fromList
    [ key
    | (key, info) <- Map.toList new
    , ((.deps) <$> Map.lookup key old) /= Just info.deps
    ]

-- | Phase 2, second half: downstream reachability closure over the merged module graph.
--
-- Returns the seeds plus every module that transitively depends on a seed.  Edges in the module
-- map point from dependents to dependencies, so the closure walks the reversed edges.
staleClosure :: Set ModuleKey -> Map ModuleKey ModuleInfo -> Set ModuleKey
staleClosure seeds allModules =
  go seeds (Set.toList seeds)
  where
    reverseEdges :: Map ModuleKey [ModuleKey]
    reverseEdges =
      Map.fromListWith (++)
        [ (dep, [key])
        | (key, info) <- Map.toList allModules
        , dep <- Set.toList info.deps
        ]

    go acc [] = acc
    go acc (k : ks) =
      let
        dependents = [d | d <- Map.findWithDefault [] k reverseEdges, not (Set.member d acc)]
      in go (foldr Set.insert acc dependents) (dependents ++ ks)

-- | Map changed source paths to module keys via the refreshed module map's pending task keys.
changedModuleKeys :: Set OsPath -> Map ModuleKey ModuleInfo -> Set ModuleKey
changedModuleKeys changedPaths modules =
  Set.fromList
    [ key
    | (key, info) <- Map.toList modules
    , PendingSource _ src <- [info.task]
    , Set.member src changedPaths
    ]

-- | Commit the digest records of all units that built successfully.
--
-- Called after a batch has drained.  A unit's digests are committed only when it has no failed
-- tasks and all of its stale modules were actually compiled in this session -- otherwise a stale
-- module that was outside the requested scope would be recorded as up to date and never rebuilt.
-- Uncommitted units simply re-detect their changed sources in the next session, which at worst
-- re-runs the (incremental) metadata step.
commitDigests ::
  Map UnitName Unit ->
  Map UnitName UnitDiff ->
  -- | Units with at least one failed task.
  Set UnitName ->
  -- | Modules whose compile task completed successfully.
  Set ModuleKey ->
  -- | The accumulated stale closure.
  Set ModuleKey ->
  IO ()
commitDigests units diffs failedUnits compiledModules stale =
  sequence_
    [ writeDigestRecord unit.cache d.newDigests
    | (name, d) <- Map.toList diffs
    , not (Set.member name failedUnits)
    , Just unit <- [Map.lookup name units]
    , all (`Set.member` compiledModules) [k | k <- Set.toList stale, k.unit == name]
    ]

