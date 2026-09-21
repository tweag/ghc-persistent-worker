module Ghc.Ui.Event.OpLog where

import Brick (EventM)
import Control.Lens (use)
import Ghc.Ui.Data.Main (MainEvent (..))
import Ghc.Ui.Data.Name (Name)
import Ghc.Ui.Data.OpLog (OpLogState)
import Ghc.Ui.Data.Session (SessionEvent (..))
import Ghc.Ui.Data.Sessions (SessionsEvent (..))
import Ghc.Ui.Event.Popup (handleListEventOf)
import Ghc.Ui.Monad (MonadUi)
import Ghc.Ui.OpLog (logOpDebug)
import Graphics.Vty (Event)
import qualified Types.Api as Api
import Types.Text (showText)

-- | Ignore events when logging:
-- - 'SetTime' since it is emitted ten times per second
-- - 'OpLogMessage' because that just ends up in the smae log
logMainEvent ::
  MonadUi m =>
  MainEvent ->
  m ()
logMainEvent = \case
  SetTime {} -> pure ()
  OpLogMessage {} -> pure ()
  Sessions {event = Session {event = ApiEvent {event = Api.LogMessage {}}}} -> pure ()
  event -> do
    current <- use #currentFocus
    logOpDebug ("main event in " <> showText current <> ": " <> showText event)

handleOpLogEvent :: Event -> EventM Name OpLogState ()
handleOpLogEvent = handleListEventOf #debugMessages
