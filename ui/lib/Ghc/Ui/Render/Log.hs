module Ghc.Ui.Render.Log where

import Brick (Padding (..), VScrollBarOrientation (..), Widget, padRight, txt, withVScrollBars)
import Brick.Widgets.List (renderList)
import qualified Data.Text as Text
import Data.Text (Text)
import Data.Time (UTCTime, defaultTimeLocale, formatTime)
import Ghc.Ui.Data.Log (LogMessage (..), LogState)
import Ghc.Ui.Data.Name (Name (Log))
import Ghc.Ui.Render.Popup (popup)

formatTimestamp :: UTCTime -> Text
formatTimestamp =
  Text.pack .
  formatTime defaultTimeLocale "%H:%M:%S%Q"

formatEntry :: LogMessage -> Text
formatEntry LogMessage {category, level, message, time} =
  formatTimestamp time
  <> " ["
  <> level
  <> "] "
  <> category
  <> ": "
  <> message

renderLog :: Name -> LogState -> Widget Name
renderLog current =
  withVScrollBars OnRight .
  renderList renderRow (current == Log)
  where
    renderRow _ e = padRight Max (txt (formatEntry e))

renderLogPopup :: LogState -> Widget Name
renderLogPopup = popup 80 "Server Log" . renderLog Log
