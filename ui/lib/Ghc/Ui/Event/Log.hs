module Ghc.Ui.Event.Log where

import Brick (EventM, lookupViewport, setTop, vScrollBy, viewportScroll, vpSize, vpTop)
import Control.Lens ((%=), (^.))
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State.Class (get, modify)
import Data.Foldable (for_, toList)
import Data.Sequence qualified as Seq
import Data.Sequence (Seq, (<|), (|>))
import Data.Text qualified as Text
import Data.Text (Text)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Ghc.Ui.Data.Log (LogMessage (..), LogState (..))
import Ghc.Ui.Data.Main (currentSession)
import Ghc.Ui.Data.Name (Name (Log))
import Ghc.Ui.Monad (MonadUi)
import Ghc.Ui.Render.Log (formatEntry)
import Graphics.Vty (Event (..), Key (..))

clampSelection :: Seq a -> Int -> Int
clampSelection messages sel =
  if null messages
  then 0
  else max 0 (min (length messages - 1) sel)

insertMessage :: LogMessage -> LogState -> LogState
insertMessage message LogState {..} =
  LogState {
    messages = newMessages,
    selected = clampSelection newMessages selected
  }
  where
    -- TODO insert sorted
    newMessages = Seq.sortOn (.timestampMs) (message <| messages)

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

-- | Move the selection by 'delta' messages (negative moves up), clamping at the ends, then scroll the viewport
-- just enough to bring the newly selected entry back into view if it isn't already.
moveSelection :: Int -> EventM Name LogState ()
moveSelection delta = do
  s <- get
  let newSelected = clampSelection s.messages (s.selected + delta)
  modify \st -> st {selected = newSelected}
  scrollIntoView newSelected

-- | Adjusts the 'LogViewer' viewport's scroll offset, if necessary, so that the entry at 'idx' is fully visible.
-- Does nothing if the viewport hasn't been rendered yet or the index is out of range.
scrollIntoView :: Int -> EventM Name LogState ()
scrollIntoView idx = do
  s <- get
  mvp <- lookupViewport Log
  for_ ((,) <$> Seq.lookup idx (entryRanges s.messages) <*> mvp) $ \((startLine, endLine), vp) -> do
    let top = vp ^. vpTop
        height = snd (vp ^. vpSize)
        itemHeight = endLine - startLine + 1
    if startLine < top
      then setTop (viewportScroll Log) startLine
      else
        if endLine > top + height - 1
          then
            setTop
              (viewportScroll Log)
              -- If the entry itself is taller than the viewport, prefer showing its start (the user can
              -- keep reading the rest with 'd') over showing its tail with the start already scrolled past.
              (if itemHeight <= height then max 0 (endLine - height + 1) else startLine)
          else pure ()

scrollAndClampSelection :: Int -> EventM Name LogState ()
scrollAndClampSelection delta = do
  vScrollBy (viewportScroll Log) delta
  s <- get
  mvp <- lookupViewport Log
  for_ mvp \ vp -> do
    let top = vp ^. vpTop
        height = snd (vp ^. vpSize)
        ranges = entryRanges s.messages
        inView (startLine, endLine) = endLine >= top && startLine <= top + height - 1
        stillVisible = maybe False inView (Seq.lookup s.selected ranges)
    unless stillVisible $
      for_ (Seq.findIndexL inView ranges) \ i -> modify \ st -> st {selected = i}

handleKeyEvent :: Key -> EventM Name LogState ()
handleKeyEvent = \case
  KChar 'j' -> moveSelection 1
  KDown -> moveSelection 1
  KChar 'k' -> moveSelection (-1)
  KUp -> moveSelection (-1)
  KChar 'd' -> scrollAndClampSelection 10
  KChar 'u' -> scrollAndClampSelection (-10)
  _ -> pure ()

handleLogEvent :: Event -> EventM Name LogState ()
handleLogEvent = \case
  EvKey key [] -> handleKeyEvent key
  _ -> pure ()

-- TODO is posix time necessary?
logMessageWith ::
  MonadIO m =>
  (LogMessage -> m ()) ->
  Text ->
  Text ->
  Text ->
  m ()
logMessageWith use category level message = do
  ms <- liftIO $ round . (* 1000) <$> getPOSIXTime
  use LogMessage {category, level, message, timestampMs = ms}

logMessage ::
  MonadUi m =>
  Text ->
  Text ->
  Text ->
  m ()
logMessage =
  logMessageWith \ msg -> currentSession . #log %= insertMessage msg
