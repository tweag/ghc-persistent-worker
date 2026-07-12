-- | Popup browser for the worker's lazily-loaded bytecode cache (see 'Internal.Cache.Bytecode' in the worker).
--
-- The worker only tracks cache entries per home module (not per top-level binding), so "grouping" here means grouping
-- module rows by their owning unit; there is no finer-grained binding-level data available to display. Data is
-- fetched as an explicit snapshot ('Load') rather than streamed, per the prototype scope.
module UI.BytecodeBrowser where

import Brick.Types (EventM, Widget)
import Brick.Widgets.Core (Padding (Max), padRight, str, withAttr, (<+>))
import Brick.Widgets.List (GenericList, list, listSelectedElement, renderList)
import Control.Monad.State (gets, modify)
import Data.List (sortOn)
import Data.Ord (Down (Down))
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text qualified as Text
import Lens.Micro.Platform (Lens', lens)
import UI.Types (Name (BytecodeBrowser), disabledAttr)
import UI.Utils (formatBytes, popup)

-- | A single tracked bytecode cache entry, as reported by the worker's @GetBytecodeState@ RPC.
data Entry = Entry
  { unitId :: Text
  , moduleName :: Text
  , size :: Int
  , lastAccess :: Int
  }
  deriving stock (Eq, Show)

-- | A row in the displayed list: either a group header (the owning unit) or a module entry.
data Row
  = Header Text
  | Item Entry
  deriving stock (Eq, Show)

-- | Toggled with a dedicated key binding; see 'cycleSortOrder'.
data SortOrder
  = MRU
  | LRU
  | Alphabetical
  deriving stock (Eq, Show, Enum, Bounded)

data State = State
  { rows :: GenericList Name Seq.Seq Row
  , sortOrder :: SortOrder
  , rawEntries :: [Entry]
  }

data Event
  = Load [Entry]
  | ToggleSort
  -- | Result of an eviction request, carrying the (unit, maybe module) that was evicted, so the local snapshot can be
  -- updated optimistically without waiting for the next full reload.
  | Evicted Text (Maybe Text)

initialState :: State
initialState =
  State
    { rows = list BytecodeBrowser Seq.empty 1
    , sortOrder = MRU
    , rawEntries = []
    }

cycleSortOrder :: SortOrder -> SortOrder
cycleSortOrder s
  | s == maxBound = minBound
  | otherwise = succ s

sortOrderLabel :: SortOrder -> String
sortOrderLabel = \case
  MRU -> "MRU"
  LRU -> "LRU"
  Alphabetical -> "A-Z"

-- | Arrange entries into grouped, sorted rows according to the current sort order.
--
-- * 'Alphabetical': units and modules ordered by name.
-- * 'MRU' \/ 'LRU': units ordered by their most-recently-used entry's access time (descending \/ ascending), modules
--   within a unit ordered the same way.
buildRows :: SortOrder -> [Entry] -> Seq.Seq Row
buildRows sortOrder entries =
  Seq.fromList $ concatMap renderGroup orderedGroups
  where
    groups = groupByUnit entries
    orderedGroups = case sortOrder of
      Alphabetical -> sortOn fst groups
      MRU -> sortOn (Down . groupRecency) groups
      LRU -> sortOn groupRecency groups
    groupRecency (_, es) = maximum (0 : (lastAccess <$> es))
    renderGroup (uid, es) = Header uid : (Item <$> sortEntries es)
    sortEntries es = case sortOrder of
      Alphabetical -> sortOn moduleName es
      MRU -> sortOn (Down . lastAccess) es
      LRU -> sortOn lastAccess es

groupByUnit :: [Entry] -> [(Text, [Entry])]
groupByUnit entries =
  [(uid, [e | e <- entries, e.unitId == uid]) | uid <- units]
  where
    units = dedup (unitId <$> entries)
    dedup = foldr (\x acc -> if x `elem` acc then acc else x : acc) []

-- | Lens onto the underlying Brick list, for reuse with 'UI.Utils.handleListEventOf'.
rowsLens :: Lens' State (GenericList Name Seq.Seq Row)
rowsLens = lens rows (\s r -> s {rows = r})

draw :: State -> Widget Name
draw State{rows, sortOrder} =
  popup 60 ("Bytecode Cache (" ++ sortOrderLabel sortOrder ++ ")") $ renderList drawRow True rows
 where
  drawRow isSel row =
    (if isSel then withAttr disabledAttr else id) $ drawRow' row
  drawRow' (Header uid) = str (Text.unpack uid)
  drawRow' (Item Entry{..}) =
    str "  "
      <+> padRight Max (str (Text.unpack moduleName))
      <+> str (formatBytes size ++ " BCOs")
      <+> str ("  access #" ++ show lastAccess)

-- | Determine which unit / module the currently selected row refers to, for eviction requests.
selectedTarget :: State -> Maybe (Text, Maybe Text)
selectedTarget State{rows} =
  case snd <$> listSelectedElement rows of
    Just (Header uid) -> Just (uid, Nothing)
    Just (Item e) -> Just (e.unitId, Just e.moduleName)
    Nothing -> Nothing

-- | Rebuild the displayed row list from the current raw entries and sort order.
refreshRows :: EventM Name State ()
refreshRows = do
  order <- gets sortOrder
  entries <- gets rawEntries
  modify \s -> s {rows = list BytecodeBrowser (buildRows order entries) 1}

handleEvent :: Event -> EventM Name State ()
handleEvent (Load entries) = do
  modify \s -> s {rawEntries = entries}
  refreshRows
handleEvent ToggleSort = do
  modify \s -> s {sortOrder = cycleSortOrder s.sortOrder}
  refreshRows
handleEvent (Evicted uid Nothing) = do
  modify \s -> s {rawEntries = filter (\e -> e.unitId /= uid) s.rawEntries}
  refreshRows
handleEvent (Evicted uid (Just modName)) = do
  modify \s -> s {rawEntries = filter (\e -> not (e.unitId == uid && e.moduleName == modName)) s.rawEntries}
  refreshRows
