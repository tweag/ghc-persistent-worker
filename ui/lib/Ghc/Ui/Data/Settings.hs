module Ghc.Ui.Data.Settings where

import Brick.Widgets.List (GenericList, list)
import qualified Data.Sequence as Seq
import Data.Sequence (Seq)
import Data.Set (Set)
import Data.Text (Text)
import GHC.Generics (Generic)
import Ghc.Ui.Data.Name (Name (..))
import Types.FeatureFlags (Feature, allFeatures)

-- | UI-local toggles that are not synchronized with the server, unlike 'Feature'. Queried directly from
-- 'SettingsState' at the point a task is triggered, rather than pushed via an API request.
data LocalFlag =
  -- | Controls whether an 'Types.Api.Execute' task is triggered with @process = True@, mirroring the client's
  -- @--process@ CLI flag.
  ProcessExecute
  deriving stock (Eq, Show, Enum, Bounded)

localFlagLabel :: LocalFlag -> Text
localFlagLabel = \case
  ProcessExecute -> "Subprocess execution"

-- | Distinguishes server-synced feature flags from purely local UI toggles within the same settings list.
data SettingKind =
  SettingFeature { feature :: Feature }
  |
  SettingLocal { flag :: LocalFlag }
  deriving stock (Eq, Show)

data Setting =
  Setting {
    kind :: SettingKind,
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
    rows = list Settings (Seq.fromList (featureRows ++ localRows)) 1
  }
  where
    featureRows = [Setting {kind = SettingFeature feature, enabled = False} | feature <- allFeatures]
    localRows = [Setting {kind = SettingLocal flag, enabled = False} | flag <- [minBound .. maxBound]]

-- | Reset 'enabled' to the features carried by a 'Types.Api.ProjectStructure' event, sent when a client connects.
-- Local-only rows are left untouched, since the server has no notion of them.
load :: Set Feature -> SettingsState -> SettingsState
load enabledFeatures state =
  state {rows = update <$> state.rows}
  where
    update setting@Setting {kind} = case kind of
      SettingFeature feature -> setting {enabled = elem feature enabledFeatures}
      SettingLocal _ -> setting

-- | Whether the given local flag is currently enabled, queried whenever code needs to consult UI-local state
-- rather than dispatch a server request (e.g. the execute task's @--process@ toggle in 'Ghc.Ui.Event.Main').
localFlagEnabled :: LocalFlag -> SettingsState -> Bool
localFlagEnabled target SettingsState {rows} =
  any matches rows
  where
    matches Setting {kind = SettingLocal flag, enabled} = flag == target && enabled
    matches _ = False
