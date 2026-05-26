module Types.FeatureFlags where

data FeatureFlag =
  FeatureFixedNodesCache
  |
  FeatureFlagParser
  |
  FeatureConcurrentInitUnits
  |
  FeatureIncrementalBuildPlan
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
    -- | Use incremental metadata (only re-downsweep changed modules).
    incrementalBuildPlan :: Bool
  }
  deriving stock (Eq, Show)

defaultFeatureFlags :: FeatureFlags
defaultFeatureFlags =
  FeatureFlags {
    fixedNodesCache = True,
    flagParser = False,
    concurrentInitUnits = True,
    incrementalBuildPlan = True
  }
