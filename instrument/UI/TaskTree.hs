{-# LANGUAGE NoFieldSelectors #-}

-- | Persistent view of the project's units and modules, populated from the 'Types.Instrument.ProjectStructure'
-- instrumentation event that a @ghc-server@ instance sends as soon as a client connects.
--
-- Unlike the compile-result-based module list this view replaces, it does not depend on any compilation having
-- happened: it reflects the project layout discovered by @ghc-server@ at startup (unit names and their module
-- source files). Units are rendered as collapsible headers; 'ToggleExpand' reveals or hides their module rows,
-- drawn with an L-shaped tree connector.
--
-- Also merges in the bytecode cache's per-module stats (formerly a separate side-by-side panel, 'UI.BytecodeBrowser'):
-- since the cache is tracked at exactly the same unit\/module granularity as this tree, a module's bytecode stats
-- (size, last access, resident\/pending-eviction state) are now rendered as an extra display line directly under
-- its row instead of being duplicated in a structurally identical second tree. This also lets eviction be targeted
-- at any scope this tree already supports selecting, including the whole project (see 'selectedEvictTarget'),
-- which the old panel had no way to express.
module UI.TaskTree where

import Brick.Types (EventM, Widget)
import Brick.Widgets.Core (str, vBox, withAttr, (<+>))
import Brick.Widgets.List (GenericList, list, listElements, listMoveTo, listSelectedElement, renderList)
import Control.Monad.State (gets, modify)
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Map.Strict (Map)
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Data.Set (Set)
import Data.Text qualified as Text
import Data.Text (Text)
import Lens.Micro.Platform (Lens', lens)
import UI.Types (Name (TaskTree), builtMarker, evictedAttr, failedMarker, moduleNameAttr, nodeLabelAttr, pendingEvictionAttr, sectionProjectAttr, taskFailedAttr, taskSucceededAttr, taskTimeAttr)
import UI.Utils (drawSection, formatBytes)

-- | A unit and its module names, as reported by 'Types.Instrument.ProjectStructure'.
data Entry = Entry
  { unitName :: Text
  , modules :: [Text]
  }
  deriving stock (Eq, Show)

-- | Bytecode-cache stats for a single module, reported via 'Types.Instrument.BytecodeSnapshot' and merged into
-- the tree keyed by @(unitName, moduleName)@ -- see 'draw's @bcoLine@.
data BcoInfo = BcoInfo
  { size :: Int
  , lastAccess :: Int
  , -- | Whether the module is currently present in the worker's loader state (home package table).
    resident :: Bool
  , -- | Whether an eviction request for this module has been sent but not yet applied by the worker.
    pending :: Bool
  }
  deriving stock (Eq, Show)

-- | A row in the displayed tree: the project-root node (selectable, but not collapsible -- unlike unit headers,
-- "expanding" it doesn't hide/reveal anything, since its children are simply the existing top-level unit
-- headers), a collapsible unit header (carrying its own expanded state for rendering), or a module leaf tagged
-- with whether it is the last child of its unit (to draw the correct L-shaped connector).
data Row
  = Root
  | Header { unit :: Text, expanded :: Bool, isLast :: Bool }
  | ModuleRow { unit :: Text, name :: Text, isLast :: Bool, isLastUnit :: Bool }
  deriving stock (Eq, Show)

data State = State
  { rows :: GenericList Name Seq.Seq Row
  , units :: [Entry]
  , expandedUnits :: Set Text
  , built :: Set Text
  , failed :: Set Text
  , bco :: Map (Text, Text) BcoInfo
  }

data Event
  = Load [Entry]
  | -- | Toggle the expanded state of the unit owning the currently selected row.
    ToggleExpand
  | -- | Mark a unit\/module target (@unitName:metadata@ or @unitName:moduleName@) as successfully built.
    -- Also clears any prior failure marker for the same target, since a later success supersedes it.
    MarkBuilt Text
  | -- | Mark a unit\/module target as having failed to build. Also clears any prior success marker for the
    -- same target.
    MarkFailed Text
  | -- | Clear all success\/failure markers for a clean target -- the same three-shape grammar as
    -- 'selectedCleanTarget': the sentinel @"*"@ (every marker), a bare unit name (every marker whose target is
    -- prefixed with @unitName:@), or @unitName:moduleName@ (just that single marker).
    ClearMarks Text
  | -- | Replace the tracked bytecode-cache stats with a fresh snapshot (unit, module, BCO count, last-access
    -- counter, resident, pending-eviction), reported via 'Types.Instrument.BytecodeSnapshot'.
    LoadBco [(Text, Text, Int, Int, Bool, Bool)]
  | -- | Mark bytecode entries matching the given eviction scope (the sentinel @"*"@\/a bare unit name\/@Just@ a
    -- single module, mirroring 'selectedEvictTarget's shapes) as having a pending eviction request, without
    -- removing them -- eviction is deferred worker-side, so entries stay resident until a later 'LoadBco'
    -- confirms they're actually gone.
    EvictedBco Text (Maybe Text)

initialState :: State
initialState =
  State
    { rows = list TaskTree Seq.empty 1
    , units = []
    , expandedUnits = Set.empty
    , built = Set.empty
    , failed = Set.empty
    , bco = Map.empty
    }

-- | The unit name owning a row, whether it's the header itself or one of its module children. The project-root
-- row has no owning unit; it maps to the empty string, which never matches a real unit name (used only by
-- 'handleEvent's 'ToggleExpand', for which selecting the root is a harmless no-op).
rowUnit :: Row -> Text
rowUnit Root = Text.empty
rowUnit (Header {unit}) = unit
rowUnit (ModuleRow {unit}) = unit

-- | Arrange units into a flat row sequence: the project-root node, followed by a header per unit and its module
-- rows when expanded.
buildRows :: Set Text -> [Entry] -> Seq.Seq Row
buildRows expanded units =
  Seq.fromList (Root : concatMap renderUnit (zip [1 ..] units))
 where
  renderUnit (i, Entry{unitName, modules}) =
    Header {unit = unitName, expanded = Set.member unitName expanded, isLast = (i == length units)}
      : if Set.member unitName expanded then renderModules (i == length units) unitName modules else []
  renderModules isLastUnit uid mods =
    [ModuleRow {unit = uid, name = m, isLast = (i == length mods), isLastUnit} | (i, m) <- zip [1 :: Int ..] mods]

-- | Lens onto the underlying Brick list, for reuse with 'UI.Utils.handleListEventOf'.
rowsLens :: Lens' State (GenericList Name Seq.Seq Row)
rowsLens = lens (.rows) (\s r -> s{rows = r})

-- | Rebuild the displayed row list from the current units and expand-state.
refreshRows :: EventM Name State ()
refreshRows = do
  ex <- gets (.expandedUnits)
  us <- gets (.units)
  modify \s -> s{rows = list TaskTree (buildRows ex us) 1}

handleEvent :: Event -> EventM Name State ()
handleEvent (Load units) = do
  modify \s -> s{units}
  refreshRows
handleEvent ToggleExpand = do
  msel <- gets (fmap snd . listSelectedElement . (.rows))
  case msel of
    Nothing -> pure ()
    Just row -> do
      let uid = rowUnit row
      wasExpanded <- gets (Set.member uid . (.expandedUnits))
      modify \s -> s {expandedUnits = toggle uid s.expandedUnits}
      refreshRows
      -- Expanding a unit should focus its first module row; collapsing it should keep the selection on the
      -- unit's own header row -- neither happens automatically, since 'refreshRows'' fresh 'list' call always
      -- defaults the selection to the top of the tree.
      if wasExpanded then focusHeader uid else focusFirstModule uid
 where
  toggle x s = if Set.member x s then Set.delete x s else Set.insert x s
handleEvent (MarkBuilt target) =
  modify \s -> s{built = Set.insert target s.built, failed = Set.delete target s.failed}
handleEvent (MarkFailed target) =
  modify \s -> s{failed = Set.insert target s.failed, built = Set.delete target s.built}
handleEvent (ClearMarks target) =
  modify \s -> s{built = Set.filter (not . matches) s.built, failed = Set.filter (not . matches) s.failed}
 where
  matches t
    | target == Text.pack "*" = True
    | Text.isInfixOf (Text.pack ":") target = t == target
    | otherwise = (target <> Text.pack ":") `Text.isPrefixOf` t
handleEvent (LoadBco entries) =
  modify \s ->
    s{bco = Map.fromList [((u, m), BcoInfo sz la res pend) | (u, m, sz, la, res, pend) <- entries]}
handleEvent (EvictedBco uid mmod) =
  modify \s -> s{bco = Map.mapWithKey markIfMatch s.bco}
 where
  markIfMatch (u, m) info
    | matches u m = info{pending = True}
    | otherwise = info
  matches u m = (uid == Text.pack "*" || u == uid) && maybe True (== m) mmod

-- | The compile targets for the currently selected row, used by the 'b' build action (changed: no longer
-- includes metadata, see 'selectedMetadataTargets' for that).
--
-- Selecting the project-root node schedules a compile job for every module of every unit in the project.
-- Selecting a unit header schedules a compile job for every one of its modules (@unitName:moduleName@ for each);
-- selecting one of its module children targets only that module's compilation.
selectedCompileTargets :: State -> Maybe [Text]
selectedCompileTargets State{rows, units} = do
  (_, row) <- listSelectedElement rows
  case row of
    Root -> pure [e.unitName <> ":" <> m | e <- units, m <- e.modules]
    Header uid _ _ -> do
      entry <- List.find ((== uid) . (.unitName)) units
      pure [uid <> ":" <> m | m <- entry.modules]
    ModuleRow {unit, name} -> Just [unit <> ":" <> name]

-- | The metadata targets for the currently selected row, used by the 'm' key (added alongside the 'b' key's
-- change to schedule per-module compiles instead of metadata). Selecting the project-root node targets every
-- unit's metadata step; selecting a header or module row always targets just the owning unit's metadata step,
-- regardless of which kind of row is selected.
selectedMetadataTargets :: State -> Maybe [Text]
selectedMetadataTargets State{rows, units} = do
  (_, row) <- listSelectedElement rows
  case row of
    Root -> pure [e.unitName <> ":metadata" | e <- units]
    _ -> pure [rowUnit row <> ":metadata"]

-- | The target text for the 'x' execute action, used verbatim as the 'TaskTrigger''s @target@ field for an
-- @Execute@-kind task (see 'GhcServer.Grpc.triggerTask'):
--
-- * Project-root node selected -- the sentinel @"*"@, requesting execution of every module of every unit.
-- * Unit header row selected -- the bare unit name, requesting execution of every module of that unit.
-- * Module row selected -- @unitName:moduleName@, requesting execution of only that module (satisfying the
--   requirement that triggering execution on a module executes only that module).
selectedExecuteTarget :: State -> Maybe Text
selectedExecuteTarget State{rows} = do
  (_, row) <- listSelectedElement rows
  case row of
    Root -> Just (Text.pack "*")
    Header uid _ _ -> Just uid
    ModuleRow {unit, name} -> Just (unit <> ":" <> name)

-- | The eviction scope for the currently selected row, used by the 'e' key -- the sentinel @"*"@ evicts every
-- unit's bytecode (the project view's replacement for the old bytecode browser's unit-only targeting), a
-- 'Header' row evicts every module of that unit, and a 'ModuleRow' evicts just that single module.
selectedEvictTarget :: State -> Maybe (Text, Maybe Text)
selectedEvictTarget State{rows} = do
  (_, row) <- listSelectedElement rows
  case row of
    Root -> Just (Text.pack "*", Nothing)
    Header uid _ _ -> Just (uid, Nothing)
    ModuleRow {unit, name} -> Just (unit, Just name)

-- | Move the list's selection to a row matching the given predicate, if any -- a no-op if no row matches.
focusRow :: (Row -> Bool) -> EventM Name State ()
focusRow p =
  modify \s -> case Seq.findIndexL p (listElements s.rows) of
    Just i -> s{rows = listMoveTo i s.rows}
    Nothing -> s

-- | Move the list's selection to the first 'ModuleRow' belonging to the given unit, if any (a no-op if the
-- unit has no modules or isn't currently expanded). Used by 'handleEvent' to focus a unit's first module right
-- after expanding it.
focusFirstModule :: Text -> EventM Name State ()
focusFirstModule uid = focusRow isFirstModuleOf
 where
  isFirstModuleOf (ModuleRow {unit}) = uid == unit
  isFirstModuleOf _ = False

-- | Move the list's selection to the given unit's own 'Header' row, if any. Used by 'handleEvent' to keep the
-- selection on the unit that was just collapsed, instead of leaving it wherever 'refreshRows'' fresh 'list'
-- call defaults it to (the top of the tree).
focusHeader :: Text -> EventM Name State ()
focusHeader uid = focusRow isHeaderOf
 where
  isHeaderOf (Header u' _ _) = uid == u'
  isHeaderOf _ = False

-- | The target text for the 'C' clean-selected action, using the same three shapes as
-- 'selectedExecuteTarget': the project-root sentinel @"*"@, a bare unit name, or @unitName:moduleName@.
-- Kept as its own function (rather than reusing 'selectedExecuteTarget') since clean's targeting is a
-- separate feature whose semantics may diverge from execute's in the future.
selectedCleanTarget :: State -> Maybe Text
selectedCleanTarget State{rows} = do
  (_, row) <- listSelectedElement rows
  case row of
    Root -> Just (Text.pack "*")
    Header uid _ _ -> Just uid
    ModuleRow {unit, name} -> Just (unit <> ":" <> name)

-- | Header line replacing the border that used to delimit this panel; see 'UI.Types.sectionProjectAttr'.
-- Uses 'UI.Utils.drawSection's permanent placeholder rectangle for visual structure.
draw :: Name -> State -> Widget Name
draw current State{rows, built, failed, bco} =
  drawSection sectionProjectAttr (withAttr sectionProjectAttr (str "Project")) $
    renderList (const drawRow') (current == TaskTree) rows
 where
  drawRow' Root =
    str "\9632 <project>"
  drawRow' (Header uid expanded' isLast) =
    withAttr nodeLabelAttr (str ((if isLast then "\9492\9472 " else "\9500\9472 ") ++ (if expanded' then "\9662 " else "\9656 ") ++ Text.unpack uid)) <+> mark (uid <> ":metadata")
  drawRow' (ModuleRow {unit, name, isLast, isLastUnit}) =
    let moduleLine =
          str (if isLastUnit then " " else "\9474")
            <+> str ("  " ++ (if isLast then "\9492\9472 " else "\9500\9472 "))
            <+> withAttr moduleNameAttr (str (Text.unpack name))
            <+> mark (unit <> ":" <> name)
     in case Map.lookup (unit, name) bco of
          Nothing -> moduleLine
          Just info -> vBox [moduleLine, bcoLine isLast info]

  -- The bytecode-cache stats line for a module (formerly the old bytecode browser's row), indented as a child
  -- of its owning module row. The leading connector column continues the module row's tree line (a vertical
  -- bar) when the module isn't the last child of its unit, so a following sibling's own connector still reads
  -- as attached to the same trunk instead of appearing interrupted by this extra line.
  --
  -- The whole line is wrapped in exactly one top-level 'withAttr' call, picking one of 'UI.Types.taskTimeAttr'
  -- (normal), 'UI.Types.evictedAttr' (non-resident) or 'UI.Types.pendingEvictionAttr' (pending eviction
  -- request, suffixed "(evicting)" vs "(evicted)" for non-pending evicted entries) -- rather than nesting
  -- independent 'withAttr' calls for each condition, since 'withAttr' fully replaces the current attribute
  -- name instead of composing with an enclosing one (see its haddock): nesting them meant the innermost
  -- applied name (e.g. 'pendingEvictionAttr', which only sets an italic style) completely discarded
  -- 'taskTimeAttr'\'s dim style rather than adding to it, reverting the line to the default (non-dim,
  -- non-colored) attribute as soon as an eviction was requested. All three names are now self-contained
  -- (registered with their own complete style in 'UI.appAttrMap') so picking exactly one is sufficient.
  bcoLine isLast BcoInfo{size, lastAccess, resident, pending} =
    withAttr (bcoAttr resident pending) $
      str (if isLast then "      " else "  \9474   ")
        <+> str (formatBytes size ++ " BCOs")
        <+> str
          ( "  access #"
              ++ show lastAccess
              ++ if pending then "  (evicting)" else if resident then "" else "  (evicted)"
          )

  bcoAttr resident pending
    | pending = pendingEvictionAttr
    | not resident = evictedAttr
    | otherwise = taskTimeAttr

  -- The built/failed markers are a leading space plus a plain check mark\/ballot X ('UI.Types.builtMarker'\/
  -- 'UI.Types.failedMarker'), single-width characters in virtually every terminal font, so no explicit-width
  -- workaround is needed here. Colored the same way 'UI.ActiveTasks'' outcome markers are (green\/red), reusing
  -- the same attributes rather than duplicating them.
  mark target
    | Set.member target built = withAttr taskSucceededAttr (str (Text.unpack builtMarker))
    | Set.member target failed = withAttr taskFailedAttr (str (Text.unpack failedMarker))
    | otherwise = str ""
