module Ghc.Ui.Event.Settings where

import Brick (EventM)
import Brick.Widgets.List (listSelectedElementL)
import Control.Lens ((%%=))
import Data.Monoid (First (..))
import Ghc.Ui.Data.Name (Name)
import Ghc.Ui.Data.Settings (Setting (..), SettingKind, SettingsState (..))

-- | Toggle the local checkbox state of the currently selected row, returning the affected 'SettingKind' (if any
-- row was selected) so the caller can decide how to react: dispatch 'Types.Api.ToggleFeature' for a
-- 'Ghc.Ui.Data.Settings.SettingFeature' row, or just leave the local state changed for a
-- 'Ghc.Ui.Data.Settings.SettingLocal' row.
toggleSelected :: EventM Name SettingsState (Maybe SettingKind)
toggleSelected =
  fmap getFirst $ #rows . listSelectedElementL %%= \ Setting {kind, enabled} ->
    (First (Just kind), Setting {kind, enabled = not enabled})
