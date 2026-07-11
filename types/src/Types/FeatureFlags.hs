module Types.FeatureFlags where

data FeatureFlag =
  FeatureFixedNodesCache
  |
  FeatureFlagParser
  |
  FeatureConcurrentInitUnits
  |
  FeatureInstrument
  |
  FeatureIncrementalBuildPlan
  |
  FeatureLazyByteCode
  deriving stock (Eq, Show)

-- | Runtime feature flags that control alternative implementations.
data FeatureFlags =
  FeatureFlags {
    -- | Use fixed module graph nodes instead of calling 'summariseFile' when restoring from cache.
    fixedNodesCache :: Bool,
    -- | Use the custom flatparse-based flag parser instead of GHC's 'parseDynamicFlags'.
    flagParser :: Bool,
    -- | When restoring units from cache, perform as much work as possible concurrently.
    concurrentInitUnits :: Bool,
    -- | Integrated with accompanying monitoring instrument app
    instrument :: Bool,
    -- | Use incremental metadata (only re-downsweep changed modules).
    incrementalBuildPlan :: Bool,
    lazyByteCode :: Bool,
    -- | Upper bound on the total tracked size (in BCO count, see 'Internal.Cache.Bytecode') of lazily loaded bytecode
    -- kept in the HPT. When set and 'lazyByteCode' is enabled, the least-recently-used entries are unloaded at the
    -- end of each compile job once the tracked total exceeds this limit. 'Nothing' disables unloading entirely.
    --
    -- Not exposed via CLI (no numeric feature-flag parsing exists yet); set directly on 'FeatureFlags' by callers
    -- (e.g. tests) that need it.
    lazyByteCodeCacheLimit :: Maybe Int
  }
  deriving stock (Eq, Show)

defaultFeatureFlags :: FeatureFlags
defaultFeatureFlags =
  FeatureFlags {
    fixedNodesCache = True,
    flagParser = False,
    concurrentInitUnits = True,
    instrument = False,
    incrementalBuildPlan = True,
    lazyByteCode = False,
    lazyByteCodeCacheLimit = Nothing
  }
