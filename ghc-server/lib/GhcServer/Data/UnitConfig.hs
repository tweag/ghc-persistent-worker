-- | JSON-serializable unit configuration read from @unit.json@.
module GhcServer.Data.UnitConfig where

import Data.Aeson (FromJSON (..), ToJSON (..), withObject, (.:?))
import Data.Foldable (fold)
import GHC.Generics (Generic)

-- | The contents of a @unit.json@ file in a unit directory.
data UnitConfig =
  UnitConfig {
    -- | Names of home units that this unit depends on.
    deps :: [String],
    -- | GHC CLI arguments for this unit.
    args :: [String]
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

instance FromJSON UnitConfig where
  parseJSON =
    withObject "UnitConfig" \ o -> do
      deps <- fold <$> o .:? "deps"
      args <- fold <$> o .:? "args"
      pure UnitConfig {..}
