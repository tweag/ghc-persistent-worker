-- | Pop-up window (triggered by the capital-@L@ key) displaying the log messages captured from the connected
-- @ghc-server@'s 'Types.Log.Logger' (see @GhcServer.Log.instrumentLogger@). Entries are tagged with the
-- unit\/module target that was active when a message was emitted and a millisecond timestamp, and are always
-- displayed sorted by time.
--
-- Filtering by unit\/module is supported by 'visibleEntries', but there is no key binding wiring a 'Filter' value
-- other than 'noFilter' yet -- this is deliberately dormant infrastructure for a later UI feature.
module UI.LogViewer where

import Brick.Types (Widget)
import Brick.Widgets.Core (Padding (Max), padRight, str)
import Brick.Widgets.List (GenericList, list, renderList)
import Data.List (sortOn)
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (defaultTimeLocale, formatTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Lens.Micro.Platform (Lens', lens)
import UI.Types (Name (LogViewer))

-- | A single captured log message, as pushed by the server over the instrument channel.
data Entry = Entry
  { target :: Text
  , level :: Text
  , message :: Text
  , timestampMs :: Integer
  }
  deriving stock (Eq, Show)

-- | Restricts the displayed entries to a given unit and\/or module. 'Nothing' fields match everything.
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

    matchModule = maybe True (\ mn -> m == Just mn) moduleName

-- | Entries matching the filter, sorted by timestamp (oldest first).
visibleEntries :: Filter -> [Entry] -> [Entry]
visibleEntries f = sortOn (.timestampMs) . filter (matchesFilter f)

data State = State
  { rows :: GenericList Name Seq.Seq Entry
  , rawEntries :: [Entry]
  , filterBy :: Filter
  }

initialState :: State
initialState =
  State {rows = list LogViewer Seq.empty 1, rawEntries = [], filterBy = noFilter}

-- | Lens onto the underlying Brick list, for reuse with 'UI.Utils.handleListEventOf'.
rowsLens :: Lens' State (GenericList Name Seq.Seq Entry)
rowsLens = lens rows (\ s r -> s {rows = r})

refresh :: State -> State
refresh s =
  s {rows = list LogViewer (Seq.fromList (visibleEntries s.filterBy s.rawEntries)) 1}

-- | Record a newly received log entry.
addEntry :: Entry -> State -> State
addEntry e s = refresh s {rawEntries = e : s.rawEntries}

formatTimestamp :: Integer -> String
formatTimestamp ms =
  formatTime defaultTimeLocale "%H:%M:%S%Q" (posixSecondsToUTCTime (fromIntegral ms / 1000))

-- | Renders only the list content; the surrounding frame/title is supplied by the caller
-- ('UI.hs'\'s @L@-key dispatch via 'UI.Utils.popup'), so this must not add its own border.
draw :: Name -> State -> Widget Name
draw current State {rows} =
  renderList drawRow (current == LogViewer) rows
  where
    drawRow _ Entry {target, level, message, timestampMs} =
      padRight
        Max
        ( str
            ( formatTimestamp timestampMs
                ++ " ["
                ++ Text.unpack level
                ++ "] "
                ++ Text.unpack target
                ++ ": "
                ++ Text.unpack message
            )
        )
