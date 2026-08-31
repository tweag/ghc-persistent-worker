-- | Persistent view of the project's units and modules, populated from the 'Types.Instrument.ProjectStructure'
-- instrumentation event that a @ghc-server@ instance sends as soon as a client connects.
--
-- Unlike the compile-result-based module list this view replaces, it does not depend on any compilation having
-- happened: it reflects the project layout discovered by @ghc-server@ at startup (unit names and their module
-- source files). Units are rendered as collapsible headers; 'ToggleExpand' reveals or hides their module rows,
-- drawn with an L-shaped tree connector.
module UI.TaskTree where

import Brick.Types (EventM, Widget)
import Brick.Widgets.Core (str, withAttr, (<+>))
import Brick.Widgets.List (GenericList, list, listElements, listMoveTo, listSelectedElement, renderList)
import Control.Monad.State (gets, modify)
import Data.List qualified as List
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Lens.Micro.Platform (Lens', lens)
import UI.Types (Name (TaskTree), builtMarker, disabledAttr, failedMarker, taskFailedAttr, taskSucceededAttr)

-- | A unit and its module names, as reported by 'Types.Instrument.ProjectStructure'.
data Entry = Entry
  { unitName :: Text
  , modules :: [Text]
  }
  deriving stock (Eq, Show)

-- | A row in the displayed tree: the project-root node (selectable, but not collapsible -- unlike unit headers,
-- "expanding" it doesn't hide/reveal anything, since its children are simply the existing top-level unit
-- headers), a collapsible unit header (carrying its own expanded state for rendering), or a module leaf tagged
-- with whether it is the last child of its unit (to draw the correct L-shaped connector).
data Row
  = Root
  | Header Text Bool
  | ModuleRow Text Text Bool
  deriving stock (Eq, Show)

data State = State
  { rows :: GenericList Name Seq.Seq Row
  , units :: [Entry]
  , expanded :: Set Text
  , built :: Set Text
  , failed :: Set Text
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

initialState :: State
initialState =
  State
    { rows = list TaskTree Seq.empty 1
    , units = []
    , expanded = Set.empty
    , built = Set.empty
    , failed = Set.empty
    }

-- | The unit name owning a row, whether it's the header itself or one of its module children. The project-root
-- row has no owning unit; it maps to the empty string, which never matches a real unit name (used only by
-- 'handleEvent's 'ToggleExpand', for which selecting the root is a harmless no-op).
rowUnit :: Row -> Text
rowUnit Root = Text.empty
rowUnit (Header uid _) = uid
rowUnit (ModuleRow uid _ _) = uid

-- | Arrange units into a flat row sequence: the project-root node, followed by a header per unit and its module
-- rows when expanded.
buildRows :: Set Text -> [Entry] -> Seq.Seq Row
buildRows expanded units =
  Seq.fromList (Root : concatMap renderUnit units)
 where
  renderUnit Entry{unitName, modules} =
    Header unitName (Set.member unitName expanded)
      : if Set.member unitName expanded then renderModules unitName modules else []
  renderModules uid mods =
    [ModuleRow uid m (i == length mods) | (i, m) <- zip [1 :: Int ..] mods]

-- | Lens onto the underlying Brick list, for reuse with 'UI.Utils.handleListEventOf'.
rowsLens :: Lens' State (GenericList Name Seq.Seq Row)
rowsLens = lens rows (\s r -> s{rows = r})

-- | Rebuild the displayed row list from the current units and expand-state.
refreshRows :: EventM Name State ()
refreshRows = do
  ex <- gets expanded
  us <- gets units
  modify \s -> s{rows = list TaskTree (buildRows ex us) 1}

handleEvent :: Event -> EventM Name State ()
handleEvent (Load units) = do
  modify \s -> s{units}
  refreshRows
handleEvent ToggleExpand = do
  msel <- gets (fmap snd . listSelectedElement . rows)
  case msel of
    Nothing -> pure ()
    Just row -> do
      let uid = rowUnit row
      wasExpanded <- gets (Set.member uid . expanded)
      modify \s -> s{expanded = toggle uid s.expanded}
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
    Root -> pure [unitName e <> ":" <> m | e <- units, m <- modules e]
    Header uid _ -> do
      entry <- List.find ((== uid) . unitName) units
      pure [uid <> ":" <> m | m <- modules entry]
    ModuleRow uid m _ -> Just [uid <> ":" <> m]

-- | The metadata targets for the currently selected row, used by the 'm' key (added alongside the 'b' key's
-- change to schedule per-module compiles instead of metadata). Selecting the project-root node targets every
-- unit's metadata step; selecting a header or module row always targets just the owning unit's metadata step,
-- regardless of which kind of row is selected.
selectedMetadataTargets :: State -> Maybe [Text]
selectedMetadataTargets State{rows, units} = do
  (_, row) <- listSelectedElement rows
  case row of
    Root -> pure [unitName e <> ":metadata" | e <- units]
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
    Header uid _ -> Just uid
    ModuleRow uid m _ -> Just (uid <> ":" <> m)

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
  isFirstModuleOf (ModuleRow u' _ _) = uid == u'
  isFirstModuleOf _ = False

-- | Move the list's selection to the given unit's own 'Header' row, if any. Used by 'handleEvent' to keep the
-- selection on the unit that was just collapsed, instead of leaving it wherever 'refreshRows'' fresh 'list'
-- call defaults it to (the top of the tree).
focusHeader :: Text -> EventM Name State ()
focusHeader uid = focusRow isHeaderOf
 where
  isHeaderOf (Header u' _) = uid == u'
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
    Header uid _ -> Just uid
    ModuleRow uid m _ -> Just (uid <> ":" <> m)

draw :: Name -> State -> Widget Name
draw current State{rows, built, failed} = renderList drawRow (current == TaskTree) rows
 where
  drawRow isSel row = (if isSel then withAttr disabledAttr else id) (drawRow' row)
  drawRow' Root =
    str "\9632 <project>"
  drawRow' (Header uid expanded') =
    str ((if expanded' then "\9662 " else "\9656 ") ++ Text.unpack uid) <+> mark (uid <> ":metadata")
  drawRow' (ModuleRow uid m isLast) =
    str ("  " ++ (if isLast then "\9492\9472 " else "\9500\9472 ") ++ Text.unpack m) <+> mark (uid <> ":" <> m)

  -- The built/failed markers are a leading space plus a plain check mark\/ballot X ('UI.Types.builtMarker'\/
  -- 'UI.Types.failedMarker'), single-width characters in virtually every terminal font, so no explicit-width
  -- workaround is needed here. Colored the same way 'UI.ActiveTasks'' outcome markers are (green\/red), reusing
  -- the same attributes rather than duplicating them.
  mark target
    | Set.member target built = withAttr taskSucceededAttr (str (Text.unpack builtMarker))
    | Set.member target failed = withAttr taskFailedAttr (str (Text.unpack failedMarker))
    | otherwise = str ""
