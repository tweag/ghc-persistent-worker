-- | Pop-up window (triggered by the capital-@L@ key) displaying the log messages captured from the connected
-- @ghc-server@'s 'Types.Log.Logger' (see @GhcServer.Log.instrumentLogger@). Entries are tagged with the
-- unit/module target that was active when a message was emitted and a millisecond timestamp, and are always
-- displayed sorted by time.
--
-- Filtering by unit/module is supported by 'visibleEntries', but there is no key binding wiring a 'Filter' value
-- other than 'noFilter' yet -- this is deliberately dormant infrastructure for a later UI feature.
--
-- Rendering deliberately avoids Brick.Widgets.List: a log entry's 'message' can contain literal newlines (e.g.
-- multi-line diagnostics forwarded verbatim from the server), so entries don't have a uniform height, but
-- 'Brick.Widgets.List.GenericList' assumes every item occupies exactly 'Brick.Widgets.List.listItemHeight' rows,
-- and a taller entry desyncs its row-offset arithmetic and its selection-anchored auto-scroll, clipping whatever
-- part of the entry falls past the assumed height with no way to scroll further to reach it. Instead, the whole
-- log is rendered as one 'vBox' inside a plain 'viewport', whose scroll position Brick tracks independently of
-- the selected entry: 'j'/'k' (and the arrow keys) move the selection and, only when necessary, nudge the
-- viewport just enough to bring the newly selected entry back on screen; 'd'/'u' scroll the viewport by a single
-- line without touching the selection at all.
module UI.LogViewer where

import Brick.Main (lookupViewport, setTop, vScrollBy, viewportScroll)
import Brick.Types (EventM, ViewportType (Vertical), Widget, vpSize, vpTop)
import Brick.Widgets.Core (Padding (Max), padRight, str, vBox, viewport, withAttr)
import Brick.Widgets.List (listSelectedAttr, listSelectedFocusedAttr)
import Control.Monad.State.Class (get, modify)
import Data.Foldable (for_, toList)
import Data.List (sortOn)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (defaultTimeLocale, formatTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Graphics.Vty qualified as V
import Lens.Micro.Platform ((^.))
import UI.Types (Name (LogViewer))

-- | A single captured log message, as pushed by the server over the instrument channel.
data Entry = Entry
  { target :: Text
  , level :: Text
  , message :: Text
  , timestampMs :: Integer
  }
  deriving stock (Eq, Show)

-- | Restricts the displayed entries to a given unit and/or module. 'Nothing' fields match everything.
data Filter = Filter
  { unit :: Maybe Text
  , moduleName :: Maybe Text
  }
  deriving stock (Eq, Show)

noFilter :: Filter
noFilter = Filter {unit = Nothing, moduleName = Nothing}

-- | Split a target string (@unitName@, @unitName:metadata@, @unitName:moduleName@, or
-- @unitName:moduleName:execute@) into its unit and optional module parts.
targetParts :: Text -> (Text, Maybe Text)
targetParts t = case Text.splitOn (Text.pack ":") t of
  (u : m : _) | m /= Text.pack "metadata" -> (u, Just m)
  (u : _) -> (u, Nothing)
  [] -> (t, Nothing)

matchesFilter :: Filter -> Entry -> Bool
matchesFilter Filter {unit, moduleName} e =
  matchUnit && matchModule
  where
    (u, m) = targetParts e.target

    matchUnit = maybe True (== u) unit

    matchModule = maybe True (\mn -> m == Just mn) moduleName

-- | Entries matching the filter, sorted by timestamp (oldest first).
visibleEntries :: Filter -> [Entry] -> [Entry]
visibleEntries f = sortOn (.timestampMs) . filter (matchesFilter f)

data State = State
  { entries :: Seq Entry
  -- ^ Entries currently matching 'filterBy', sorted oldest first -- the same set 'visibleEntries' would produce,
  -- cached here so 'draw' and the scrolling logic don't need to recompute it.
  , rawEntries :: [Entry]
  , filterBy :: Filter
  , selected :: Int
  -- ^ Index into 'entries' of the currently selected row, moved by 'j'/'k'. Independent of the viewport's
  -- scroll offset, which Brick tracks internally for the 'LogViewer' name and which 'd'/'u' adjust directly.
  }

initialState :: State
initialState =
  State {entries = Seq.empty, rawEntries = [], filterBy = noFilter, selected = 0}

clampSelection :: Seq Entry -> Int -> Int
clampSelection es sel
  | Seq.null es = 0
  | otherwise = max 0 (min (Seq.length es - 1) sel)

-- | Rebuilds 'entries' from 'rawEntries'/'filterBy' and clamps 'selected' to the new bounds. Since new entries
-- are appended at the end (they arrive with increasing timestamps and 'visibleEntries' sorts ascending),
-- 'selected' keeps pointing at the same logical entry across refreshes instead of resetting to the oldest one.
refresh :: State -> State
refresh s =
  let es = Seq.fromList (visibleEntries s.filterBy s.rawEntries)
   in s {entries = es, selected = clampSelection es s.selected}

-- | Record a newly received log entry.
addEntry :: Entry -> State -> State
addEntry e s = refresh s {rawEntries = e : s.rawEntries}

formatTimestamp :: Integer -> String
formatTimestamp ms =
  formatTime defaultTimeLocale "%H:%M:%S%Q" (posixSecondsToUTCTime (fromIntegral ms / 1000))

formatEntry :: Entry -> String
formatEntry Entry {target, level, message, timestampMs} =
  formatTimestamp timestampMs
    ++ " ["
    ++ Text.unpack level
    ++ "] "
    ++ Text.unpack target
    ++ ": "
    ++ Text.unpack message

-- | Number of physical rows an entry renders as (at least 1): the number of lines its formatted text is split
-- into by embedded newlines. Entries are never wrapped on width, only split on literal @'\\n'@s.
entryLineCount :: Entry -> Int
entryLineCount = length . lines . formatEntry

-- | The inclusive, 0-based (startLine, endLine) row range each entry occupies in the rendered 'vBox', in order.
entryRanges :: Seq Entry -> Seq (Int, Int)
entryRanges = snd . foldl' step (0, Seq.empty) . toList
  where
    step (start, acc) e =
      let h = entryLineCount e
       in (start + h, acc Seq.|> (start, start + h - 1))

-- | Move the selection by 'delta' entries (negative moves up), clamping at the ends, then scroll the viewport
-- just enough to bring the newly selected entry back into view if it isn't already.
moveSelection :: Int -> EventM Name State ()
moveSelection delta = do
  s <- get
  let newSelected = clampSelection s.entries (s.selected + delta)
  modify \st -> st {selected = newSelected}
  scrollIntoView newSelected

-- | Adjusts the 'LogViewer' viewport's scroll offset, if necessary, so that the entry at 'idx' is fully visible.
-- Does nothing if the viewport hasn't been rendered yet or the index is out of range.
scrollIntoView :: Int -> EventM Name State ()
scrollIntoView idx = do
  s <- get
  mvp <- lookupViewport LogViewer
  for_ ((,) <$> Seq.lookup idx (entryRanges s.entries) <*> mvp) $ \((startLine, endLine), vp) -> do
    let top = vp ^. vpTop
        height = snd (vp ^. vpSize)
        itemHeight = endLine - startLine + 1
    if startLine < top
      then setTop (viewportScroll LogViewer) startLine
      else
        if endLine > top + height - 1
          then
            setTop
              (viewportScroll LogViewer)
              -- If the entry itself is taller than the viewport, prefer showing its start (the user can
              -- keep reading the rest with 'd') over showing its tail with the start already scrolled past.
              (if itemHeight <= height then max 0 (endLine - height + 1) else startLine)
          else pure ()

-- | Handles input while the log viewer is focused: 'j'/'k' (and the arrow keys) move the selection, 'd'/'u'
-- scroll the viewport by one line independent of the selection. Any other key is ignored here; Esc/@q@/@L@
-- (closing the popup) are handled by the caller before reaching this function.
handleEvent :: V.Event -> EventM Name State ()
handleEvent = \case
  V.EvKey (V.KChar 'j') [] -> moveSelection 1
  V.EvKey V.KDown [] -> moveSelection 1
  V.EvKey (V.KChar 'k') [] -> moveSelection (-1)
  V.EvKey V.KUp [] -> moveSelection (-1)
  V.EvKey (V.KChar 'd') [] -> vScrollBy (viewportScroll LogViewer) 1
  V.EvKey (V.KChar 'u') [] -> vScrollBy (viewportScroll LogViewer) (-1)
  _ -> pure ()

-- | Renders only the list content; the surrounding frame/title is supplied by the caller
-- ('UI.hs'\'s @L@-key dispatch via 'UI.Utils.popup'), so this must not add its own border.
draw :: Name -> State -> Widget Name
draw current State {entries, selected} =
  viewport LogViewer Vertical $
    vBox (zipWith drawRow [0 ..] (toList entries))
  where
    drawRow i e =
      (if i == selected then withAttr selAttr else id) (padRight Max (str (formatEntry e)))

    selAttr = if current == LogViewer then listSelectedFocusedAttr else listSelectedAttr
