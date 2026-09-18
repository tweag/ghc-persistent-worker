module Ghc.Ui.Data.Settings where

import Brick.Widgets.List (GenericList, list)
import qualified Data.Sequence as Seq
import Data.Sequence (Seq)
import Data.Set (Set)
import GHC.Generics (Generic)
import Ghc.Ui.Data.Name (Name (..))
import Types.FeatureFlags (Feature, allFeatures)

data Setting =
  Setting {
    feature :: Feature,
    enabled :: Bool
  }
  deriving stock (Eq, Show)

data SettingsState =
  SettingsState {
    rows :: GenericList Name Seq Setting
  }
  deriving stock (Generic)

initialState :: SettingsState
initialState =
  SettingsState {
    rows = list Settings (Seq.fromList [Setting {feature, enabled = False} | feature <- allFeatures]) 1
  }

-- | Reset 'enabled' to the features carried by a 'Types.Api.ProjectStructure' event, sent when a client connects.
load :: Set Feature -> SettingsState -> SettingsState
load enabledFeatures state =
  state {rows = update <$> state.rows}
  where
    update Setting {feature} = Setting {feature, enabled = elem feature enabledFeatures}
