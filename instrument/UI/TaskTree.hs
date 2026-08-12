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
import Brick.Widgets.List (GenericList, list, listSelectedElement, renderList)
import Control.Monad.State (gets, modify)
import Data.List qualified as List
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Lens.Micro.Platform (Lens', lens)
import UI.Types (Name (TaskTree), builtMarker, disabledAttr)
import UI.Utils (wideStr)

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
  }

data Event
  = Load [Entry]
  | -- | Toggle the expanded state of the unit owning the currently selected row.
    ToggleExpand
  | -- | Mark a unit\/module target (@unitName:metadata@ or @unitName:moduleName@) as successfully built.
    MarkBuilt Text

initialState :: State
initialState =
  State
    { rows = list TaskTree Seq.empty 1
    , units = []
    , expanded = Set.empty
    , built = Set.empty
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
      modify \s -> s{expanded = toggle uid s.expanded}
      refreshRows
 where
  toggle x s = if Set.member x s then Set.delete x s else Set.insert x s
handleEvent (MarkBuilt target) =
  modify \s -> s{built = Set.insert target s.built}

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

-- | The target text for the 'x' execute action, used verbatim as the @triggerExecute@ RPC's target field (see
-- 'GhcServer.Grpc.triggerExecute'):
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

draw :: Name -> State -> Widget Name
draw current State{rows, built} = renderList drawRow (current == TaskTree) rows
 where
  drawRow isSel row = (if isSel then withAttr disabledAttr else id) (drawRow' row)
  drawRow' Root =
    str "\9632 <project>"
  drawRow' (Header uid expanded') =
    str ((if expanded' then "\9662 " else "\9656 ") ++ Text.unpack uid) <+> mark (uid <> ":metadata")
  drawRow' (ModuleRow uid m isLast) =
    str ("  " ++ (if isLast then "\9492\9472 " else "\9500\9472 ") ++ Text.unpack m) <+> mark (uid <> ":" <> m)

  -- The built marker is a leading space plus a checkmark emoji ('UI.Types.builtMarker'). 'wideStr' builds
  -- it through vty's low-level 'HorizText' constructor with an explicit display width of 3 (1 for the
  -- space, 2 for the emoji, which terminals render double-width even though vty's East-Asian-Width-based
  -- 'textWidth' reports it as 1), bypassing wcwidth entirely.
  mark target =
    if Set.member target built
      then wideStr 3 (Text.unpack builtMarker)
      else str ""
