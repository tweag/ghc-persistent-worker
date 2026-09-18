module Types.FeatureFlags where

import Data.Aeson (FromJSON, ToJSON)
import Data.Binary (Binary)
import Data.Text (Text)
import GHC.Generics (Generic)

data Feature =
  -- | Use fixed module graph nodes instead of calling 'summariseFile' when restoring from cache.
  FeatureFixedNodesCache
  |
  -- | Use the custom flatparse-based flag parser instead of GHC's 'parseDynamicFlags'.
  FeatureFlagParser
  |
  -- | When restoring units from cache, perform as much work as possible concurrently.
  FeatureConcurrentInitUnits
  |
  -- | Run another gRPC server for instrumentation.
  FeatureInstrument
  |
  -- | Use incremental metadata (only re-downsweep changed modules).
  FeatureIncrementalBuildPlan
  |
  -- | Load bytecode on demand when linking splices or evaluating tests.
  FeatureLazyByteCode
  deriving stock (Eq, Show, Ord, Enum, Bounded, Generic)
  deriving anyclass (Binary, FromJSON, ToJSON)

allFeatures :: [Feature]
allFeatures = [minBound .. maxBound]

parseFeatureFlag :: Text -> Either Text Feature
parseFeatureFlag = \case
  "fixed-nodes-cache" -> Right FeatureFixedNodesCache
  "flag-parser" -> Right FeatureFlagParser
  "concurrent-init-units" -> Right FeatureConcurrentInitUnits
  "instrument" -> Right FeatureInstrument
  "incremental-build-plan" -> Right FeatureIncrementalBuildPlan
  "lazy-byte-code" -> Right FeatureLazyByteCode
  flag -> Left ("Invalid feature flag: " <> flag)
