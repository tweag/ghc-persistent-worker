module Ghc.Ui.Render.Settings where

import Brick (Widget, txt, withAttr, (<+>))
import Brick.Widgets.List (renderList)
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import Ghc.Ui.Attr qualified as Attr
import Ghc.Ui.Data.Name (Name (..))
import Ghc.Ui.Data.Settings (Setting (..), SettingsState (..))
import Ghc.Ui.Render.Layout (limitList)
import Ghc.Ui.Render.Section (drawSection)
import Types.Text (showText)

renderSetting :: Setting -> Widget n
renderSetting Setting {feature, enabled} =
  txt (if enabled then "[x] " else "[ ] ") <+> withAttr Attr.nodeLabel (txt (format (showText feature)))
  where
    format label = fromMaybe label (Text.stripPrefix "Feature" label)

renderSettings :: Name -> SettingsState -> Widget Name
renderSettings current SettingsState {rows} =
  drawSection Attr.sectionSettings (withAttr Attr.sectionSettings (txt "Settings")) $
  limitList rows $
  renderList (const renderSetting) (current == Settings) rows
