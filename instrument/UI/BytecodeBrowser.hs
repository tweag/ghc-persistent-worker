-- | Persistent side panel for the worker's lazily-loaded bytecode cache (see 'Internal.Cache.Bytecode' in the
-- worker), embedded next to the task tree rather than shown as a modal popup.
--
-- Modeled after 'UI.TaskTree': units are collapsible headers, modules are their children, toggled with 'ToggleExpand'.
-- Unlike the task tree, entries here are historic -- the worker reports every module that has ever had bytecode
-- tracked (see @'Types.State.Make.bcoHistory'@ in the worker), including ones since evicted, decorated with whether
-- they're currently resident in the worker's loader state and whether an eviction request for them is still
-- pending. Non-resident (evicted) modules are drawn with 'UI.Types.evictedAttr' so they're visually distinguished
-- from currently-loaded ones.
--
-- The worker only tracks cache entries per home module (not per top-level binding), so "grouping" here means grouping
-- module rows by their owning unit; there is no finer-grained binding-level data available to display. Data is
-- fetched as an explicit snapshot ('Load') rather than streamed, per the prototype scope.
module UI.BytecodeBrowser where

import Brick.Types (EventM, Widget)
import Brick.Widgets.Border (borderWithLabel)
import Brick.Widgets.Core (Padding (Max), padRight, str, withAttr, (<+>))
import Brick.Widgets.List (GenericList, list, listSelectedElement, renderList)
import Control.Monad.State (gets, modify)
import Data.List (sortOn)
import Data.Ord (Down (Down))
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Lens.Micro.Platform (Lens', lens)
import UI.Types (Name (BytecodeBrowser), evictedAttr, disabledAttr, pendingEvictionAttr)
import UI.Utils (formatBytes)

-- | A single tracked bytecode cache entry. Historic: may refer to a module that is no longer resident (see 'resident').
data Entry = Entry
  { unitId :: Text
  , moduleName :: Text
  , size :: Int
  , lastAccess :: Int
  , -- | Whether the module is currently present in the worker's loader state (home package table).
    resident :: Bool
  , -- | Whether an eviction request for this module has been sent but not yet applied by the worker.
    pending :: Bool
  }
  deriving stock (Eq, Show)

-- | A row in the displayed tree: a collapsible unit header (carrying its own expanded state for rendering), or a
-- module leaf tagged with whether it is the last child of its unit (to draw the correct L-shaped connector).
data Row
  = Header Text Bool
  | ModuleRow Entry Bool
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
  , expanded :: Set Text
  }

data Event
  = Load [Entry]
  | ToggleSort
  | -- | Toggle the expanded state of the unit owning the currently selected row.
    ToggleExpand
  | -- | Result of an eviction request, carrying the (unit, maybe module) that was evicted, so the local snapshot can
    -- mark the affected entries as pending without waiting for the next full reload.
    Evicted Text (Maybe Text)

initialState :: State
initialState =
  State
    { rows = list BytecodeBrowser Seq.empty 1
    , sortOrder = MRU
    , rawEntries = []
    , expanded = Set.empty
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

-- | The unit name owning a row, whether it's the header itself or one of its module children.
rowUnit :: Row -> Text
rowUnit (Header uid _) = uid
rowUnit (ModuleRow e _) = e.unitId

-- | Arrange entries into grouped, collapsible rows according to the current sort order and expand-state.
--
-- * 'Alphabetical': units and modules ordered by name.
-- * 'MRU' \/ 'LRU': units ordered by their most-recently-used entry's access time (descending \/ ascending), modules
--   within a unit ordered the same way.
buildRows :: Set Text -> SortOrder -> [Entry] -> Seq.Seq Row
buildRows expanded sortOrder entries =
  Seq.fromList $ concatMap renderGroup orderedGroups
  where
    groups = groupByUnit entries
    orderedGroups = case sortOrder of
      Alphabetical -> sortOn fst groups
      MRU -> sortOn (Down . groupRecency) groups
      LRU -> sortOn groupRecency groups
    groupRecency (_, es) = maximum (0 : (lastAccess <$> es))
    renderGroup (uid, es) =
      Header uid (Set.member uid expanded)
        : if Set.member uid expanded then renderModules (sortEntries es) else []
    renderModules es = [ModuleRow e (i == length es) | (i, e) <- zip [1 :: Int ..] es]
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

draw :: Name -> State -> Widget Name
draw current State{rows, sortOrder} =
  borderWithLabel (str (" Bytecode Cache (" ++ sortOrderLabel sortOrder ++ ") ")) $
    renderList drawRow (current == BytecodeBrowser) rows
 where
  drawRow isSel row =
    (if isSel then withAttr disabledAttr else id) $ drawRow' row
  drawRow' (Header uid expanded') =
    str ((if expanded' then "\9662 " else "\9656 ") ++ Text.unpack uid)
  drawRow' (ModuleRow e isLast) =
    (if e.resident then id else withAttr evictedAttr) $
      (if e.pending then withAttr pendingEvictionAttr else id) $
        str ("  " ++ (if isLast then "\9492\9472 " else "\9500\9472 "))
          <+> padRight Max (str (Text.unpack e.moduleName))
          <+> str (formatBytes e.size ++ " BCOs")
          <+> str ("  access #" ++ show e.lastAccess)
          <+> str (if e.pending then "  (evicting)" else if e.resident then "" else "  (evicted)")

-- | Determine which unit / module the currently selected row refers to, for eviction requests. A 'Header' targets
-- the whole unit (all of its modules); a 'ModuleRow' targets just that module.
selectedTarget :: State -> Maybe (Text, Maybe Text)
selectedTarget State{rows} =
  case snd <$> listSelectedElement rows of
    Just (Header uid _) -> Just (uid, Nothing)
    Just (ModuleRow e _) -> Just (e.unitId, Just e.moduleName)
    Nothing -> Nothing

-- | Rebuild the displayed row list from the current raw entries, sort order and expand-state.
refreshRows :: EventM Name State ()
refreshRows = do
  ex <- gets expanded
  order <- gets sortOrder
  entries <- gets rawEntries
  modify \s -> s {rows = list BytecodeBrowser (buildRows ex order entries) 1}

-- | Mark entries matching the given predicate as having a pending eviction request, without removing them: eviction
-- is deferred worker-side (see 'GhcWorker.Grpc.evictBytecode'), so the entry stays resident until the next full
-- reload confirms it's actually gone.
markPending :: (Entry -> Bool) -> [Entry] -> [Entry]
markPending matches = map \e -> if matches e then e {pending = True} else e

handleEvent :: Event -> EventM Name State ()
handleEvent (Load entries) = do
  modify \s -> s {rawEntries = entries}
  refreshRows
handleEvent ToggleSort = do
  modify \s -> s {sortOrder = cycleSortOrder s.sortOrder}
  refreshRows
handleEvent ToggleExpand = do
  msel <- gets (fmap snd . listSelectedElement . rows)
  case msel of
    Nothing -> pure ()
    Just row -> do
      let uid = rowUnit row
      modify \s -> s {expanded = toggle uid s.expanded}
      refreshRows
 where
  toggle x s = if Set.member x s then Set.delete x s else Set.insert x s
handleEvent (Evicted uid Nothing) = do
  modify \s -> s {rawEntries = markPending (\e -> e.unitId == uid) s.rawEntries}
  refreshRows
handleEvent (Evicted uid (Just modName)) = do
  modify \s -> s {rawEntries = markPending (\e -> e.unitId == uid && e.moduleName == modName) s.rawEntries}
  refreshRows
