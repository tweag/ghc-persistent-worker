module Ghc.Ui.Event.Log where

import Brick (EventM)
import Brick.Widgets.List (listElements, listInsert, listMoveToEnd, listSelected)
import Control.Lens ((%=))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Foldable (toList)
import Data.Sequence qualified as Seq
import Data.Sequence (Seq, (|>))
import Data.Text qualified as Text
import Data.Text (Text)
import Data.Time (getCurrentTime)
import Ghc.Ui.Data.Log (LogMessage (..), LogState)
import Ghc.Ui.Data.Main (currentSession)
import Ghc.Ui.Data.Name (Name)
import Ghc.Ui.Data.Session (SessionState)
import Ghc.Ui.Event.Popup (handleListEventOf)
import Ghc.Ui.Monad (MonadUi)
import Ghc.Ui.Render.Log (formatEntry)
import Graphics.Vty (Event (..))

clampSelection :: Seq a -> Int -> Int
clampSelection messages sel =
  if null messages
  then 0
  else max 0 (min (length messages - 1) sel)

insertMessage :: LogMessage -> LogState -> LogState
insertMessage message messages =
  moveIfLatestSelected $
  listInsert (maybe (length messages) succ index) message messages
  where
    index = Seq.findIndexR existingOlder (listElements messages)

    existingOlder candidate = candidate.time <= message.time

    moveIfLatestSelected =
      if latestSelected
      then listMoveToEnd
      else id

    latestSelected = elem (length messages - 1) (listSelected messages)

-- | Number of physical rows an entry renders as (at least 1): the number of lines its formatted text is split
-- into by embedded newlines. Entries are never wrapped on width, only split on literal @'\\n'@s.
entryLineCount :: LogMessage -> Int
entryLineCount = length . Text.lines . formatEntry

-- | The inclusive, 0-based (startLine, endLine) row range each entry occupies in the rendered 'vBox', in order.
entryRanges :: Seq LogMessage -> Seq (Int, Int)
entryRanges = snd . foldl' step (0, Seq.empty) . toList
  where
    step (start, acc) e =
      let h = entryLineCount e
      in (start + h, acc |> (start, start + h - 1))

handleLogEvent :: Event -> EventM Name SessionState ()
handleLogEvent = handleListEventOf #log

logMessageWith ::
  MonadIO m =>
  (LogMessage -> m ()) ->
  Text ->
  Text ->
  Text ->
  m ()
logMessageWith use category level message = do
  time <- liftIO getCurrentTime
  use LogMessage {category, level, message, time}

logMessage ::
  MonadUi m =>
  Text ->
  Text ->
  Text ->
  m ()
logMessage =
  logMessageWith \ msg -> currentSession . #log %= insertMessage msg
