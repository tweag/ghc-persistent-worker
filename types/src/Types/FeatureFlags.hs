module Types.FeatureFlags where

import Data.Char (isDigit)

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
    lazyByteCodeCacheLimit :: Maybe Int
  }
  deriving stock (Eq, Show)

-- | Parse a @--max-bytecode@ CLI argument: a decimal number followed by an optional @M@ or @G@ suffix (mega/giga
-- multiplier), e.g. @"500M"@ or @"2G"@. A bare number is taken literally. Used to set 'lazyByteCodeCacheLimit'.
--
-- Caveat: the configured limit is compared against a BCO-count proxy for bytecode size (see
-- 'Internal.Cache.Bytecode.linkableBcoCount'), not an actual byte count. The @M@/@G@ suffixes are therefore a coarse,
-- conventional scaling (matching what an operator would expect from a memory-budget flag), not a literal
-- bytes-to-BCO-count conversion.
parseByteSize :: String -> Either String Int
parseByteSize s =
  case span isDigit s of
    ("", _) -> Left ("Invalid --max-bytecode value: " ++ s)
    (digits, suffix) ->
      case suffix of
        "" -> Right (read digits)
        "M" -> Right (read digits * 1000000)
        "G" -> Right (read digits * 1000000000)
        _ -> Left ("Invalid --max-bytecode suffix (expected M or G): " ++ suffix)

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
