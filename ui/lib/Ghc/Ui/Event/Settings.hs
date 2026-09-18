module Ghc.Ui.Event.Settings where

import Brick (EventM)
import Brick.Widgets.List (listSelectedElementL)
import Control.Lens ((%%=))
import Data.Monoid (First (..))
import Ghc.Ui.Data.Name (Name)
import Ghc.Ui.Data.Settings (Setting (..), SettingsState (..))
import Types.FeatureFlags (Feature)

-- | Toggle the local checkbox state of the currently selected row, returning the affected flag (if any row
-- was selected) so the caller can dispatch the corresponding 'Types.Api.ToggleFeatureFlag' request.
toggleSelected :: EventM Name SettingsState (Maybe Feature)
toggleSelected =
  fmap getFirst $ #rows . listSelectedElementL %%= \ Setting {feature, enabled} ->
    (First (Just feature), Setting {feature, enabled = not enabled})
