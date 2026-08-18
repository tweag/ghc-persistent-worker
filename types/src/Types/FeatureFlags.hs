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
  |
  -- | Share already-compiled bytecode between the main @ghc-server@ process and its execute-subprocess child
  -- via a @\/dev\/shm@-backed @mmap@ region (see 'GhcServer.Build.SharedBytecode'). When disabled, the
  -- subprocess falls back to the basic approach of restoring cached interfaces\/objects and compiling Core
  -- bindings to bytecode itself (see 'Internal.Cache.Hpt.loadCachedByteCode').
  FeatureSharedMemory
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
  "shared-memory" -> Right FeatureSharedMemory
  flag -> Left ("Invalid feature flag: " <> flag)
