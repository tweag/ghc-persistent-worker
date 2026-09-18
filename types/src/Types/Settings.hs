module Types.Settings where

import Data.Binary (Binary)
import Data.Char (isDigit)
import Data.Either.Extra (maybeToEither)
import qualified Data.Set as Set
import Data.Set (Set)
import GHC.Generics (Generic)
import Text.Read (readMaybe)
import Types.FeatureFlags (Feature (..))

data Settings =
  Settings {
    -- | Enabled features.
    features :: Set Feature,
    -- | Use incremental update of ModuleGraph.
    useIncrModGraph :: Bool,
    -- | Limit the number of BCOs that may reside in the loader state.
    --
    -- When set and 'lazyByteCode' is enabled, the least recently used entries are unloaded at the end of each compile
    -- job once the tracked total exceeds this limit. 'Nothing' disables unloading entirely.
    lazyByteCodeCacheLimit :: Maybe Int
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)

settingsWithFeatures :: Set Feature -> Settings
settingsWithFeatures features =
  Settings {
    features,
    useIncrModGraph = True,
    lazyByteCodeCacheLimit = Nothing
  }

defaultSettings :: Settings
defaultSettings =
  settingsWithFeatures [
    FeatureFixedNodesCache,
    FeatureConcurrentInitUnits,
    FeatureIncrementalBuildPlan,
    FeatureLazyByteCode
  ]

featureOn :: Feature -> Settings -> Bool
featureOn feature Settings {features} = Set.member feature features

setFeature :: Feature -> Bool -> Settings -> Settings
setFeature feature enable settings =
  settings {features = apply feature settings.features}
  where
    apply =
      if enable
      then Set.insert
      else Set.delete

toggleFlag :: Feature -> Settings -> Settings
toggleFlag feature settings =
  setFeature feature (not (featureOn feature settings)) settings

-- | Parse a @--max-bytecode@ CLI argument: a decimal number followed by an optional @k@, @M@ or @G@ suffix.

parseByteSize :: String -> Either String Int
parseByteSize s = do
  number <- maybeToEither invalid (readMaybe digits)
  factor <- parseFactor suffix
  pure (number * factor)
  where
    invalid = "Invalid --max-bytecode value: " ++ s

    parseFactor = \case
      "" -> Right 1
      "k" -> Right 1000
      "M" -> Right 1000000
      "G" -> Right 1000000000
      _ -> Left ("Invalid --max-bytecode suffix (expected k, M or G): " ++ suffix)

    (digits, suffix) = span isDigit s
