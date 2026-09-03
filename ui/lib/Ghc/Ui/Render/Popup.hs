module Ghc.Ui.Render.Popup where

import Brick (Widget, hLimitPercent, txt, vLimitPercent)
import Brick.Widgets.Border (borderWithLabel)
import Brick.Widgets.Center (centerLayer)
import Data.Text (Text)
import Ghc.Ui.Data.Name (Name)

popup :: Int -> Text -> Widget Name -> Widget Name
popup size title content =
  centerLayer $
  hLimitPercent size $
  vLimitPercent size $
  borderWithLabel (txt (" " <> title <> " ")) content
