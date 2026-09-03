module Ghc.Ui.Render.OpLog where

import Brick (VScrollBarOrientation (..), Widget, hBox, hLimitPercent, txt, txtWrap, vLimit, withAttr, withVScrollBars)
import Brick.Widgets.Center (centerLayer)
import Brick.Widgets.List (listElements, renderListWithIndex)
import Data.Text (Text)
import Ghc.Ui.Attr qualified as Attr
import Ghc.Ui.Data.Name (Name)
import Ghc.Ui.Data.OpLog (OpLogState (..), OpMessage (..), OpMessages)
import Ghc.Ui.Render.Popup (popup)

-- | Prefix rendered ahead of the latest message line.
indicator :: Text
indicator = "🮥 "

renderMessage :: Int -> Int -> Bool -> OpMessage -> Widget Name
renderMessage total index _ OpMessage {..} =
  if index == total - 1
  then renderLast
  else renderBasic
  where
    renderLast =
      hBox [
        withAttr Attr.opLogIndicator (txt indicator),
        renderBasic
      ]

    -- TODO this should be bad, since lists aren't allowed to have dynamically sized content
    renderBasic = vLimit 3 (withAttr Attr.opLogText (txtWrap message))

renderOpLog :: OpMessages -> Widget Name
renderOpLog messages =
  withVScrollBars OnRight $
  renderListWithIndex (renderMessage (length (listElements messages))) False messages

renderOpLogPopup :: OpLogState -> Widget Name
renderOpLogPopup OpLogState {debugMessages} =
  popup 80 "App Log" $ renderOpLog debugMessages

renderOpLogEmbed :: OpLogState -> Widget Name
renderOpLogEmbed OpLogState {messages} =
  renderOpLog messages

renderOpLogIdle :: OpLogState -> Widget Name
renderOpLogIdle =
  centerLayer . hLimitPercent 50 . vLimit 6 . renderOpLogEmbed
