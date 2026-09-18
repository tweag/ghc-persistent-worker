module Ghc.Ui.Render.Sessions where

import Brick (Widget, txt)
import Brick.Widgets.List (renderListWithIndex)
import qualified Data.Text as Text
import Data.Time.Format.ISO8601 (iso8601Show)
import Ghc.Ui.Data.Name (Name)
import Ghc.Ui.Data.Session (SessionState (..))
import Ghc.Ui.Data.Sessions (SessionsState)
import Ghc.Ui.Render.Popup (popup)
import Types.Text (showText)

renderItem :: Int -> Bool -> SessionState -> Widget n
renderItem index selected SessionState {startTime, workers} =
  txt $ mconcat [
    if selected then "> " else "  ",
    "Session " <> showText index <> "  " <> Text.pack (take 19 (iso8601Show startTime)),
    " - ",
    showText (length workers),
    " workers"
  ]

renderSessions :: SessionsState -> Widget Name
renderSessions =
  popup 50 "Select session" . renderListWithIndex renderItem True
