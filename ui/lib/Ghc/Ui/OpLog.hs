module Ghc.Ui.OpLog where

import Brick.Widgets.List (listInsert, listMoveToEnd)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Data.Time (getCurrentTime)
import Ghc.Ui.Data.OpLog (OpLevel (..), OpMessage (..))
import Ghc.Ui.Monad (MonadUi)
import Control.Lens ((%=))

logOpLevel ::
  MonadUi m =>
  OpLevel ->
  Text ->
  m ()
logOpLevel level message = do
  time <- liftIO getCurrentTime
  let insert l = listMoveToEnd $ listInsert (length l) OpMessage {..} l
  #opLog . #debugMessages %= insert
  when (level /= OpDebug) do
    #opLog . #messages %= insert

-- | Add a message to the global operation log.
logOp :: MonadUi m => Text -> m ()
logOp = logOpLevel OpInfo

logOpError :: MonadUi m => Text -> m ()
logOpError = logOpLevel OpError

logOpDebug :: MonadUi m => Text -> m ()
logOpDebug = logOpLevel OpDebug
