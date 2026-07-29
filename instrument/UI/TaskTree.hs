-- | Persistent view of the project's units and modules, populated from the 'Types.Instrument.ProjectStructure'
-- instrumentation event that a @ghc-server@ instance sends as soon as a client connects.
--
-- Unlike the compile-result-based module list this view replaces, it does not depend on any compilation having
-- happened: it reflects the project layout discovered by @ghc-server@ at startup (unit names and their module
-- source files). Units are rendered as collapsible headers; 'ToggleExpand' reveals or hides their module rows,
-- drawn with an L-shaped tree connector.
module UI.TaskTree where

import Brick.Types (EventM, Widget)
import Brick.Widgets.Core (str, withAttr)
import Brick.Widgets.List (GenericList, list, listSelectedElement, renderList)
import Control.Monad.State (gets, modify)
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Lens.Micro.Platform (Lens', lens)
import UI.Types (Name (TaskTree), disabledAttr)

-- | A unit and its module names, as reported by 'Types.Instrument.ProjectStructure'.
data Entry = Entry
  { unitName :: Text
  , modules :: [Text]
  }
  deriving stock (Eq, Show)

-- | A row in the displayed tree: a collapsible unit header (carrying its own expanded state for rendering), or a
-- module leaf tagged with whether it is the last child of its unit (to draw the correct L-shaped connector).
data Row
  = Header Text Bool
  | ModuleRow Text Text Bool
  deriving stock (Eq, Show)

data State = State
  { rows :: GenericList Name Seq.Seq Row
  , units :: [Entry]
  , expanded :: Set Text
  }

data Event
  = Load [Entry]
  | -- | Toggle the expanded state of the unit owning the currently selected row.
    ToggleExpand

initialState :: State
initialState =
  State
    { rows = list TaskTree Seq.empty 1
    , units = []
    , expanded = Set.empty
    }

-- | The unit name owning a row, whether it's the header itself or one of its module children.
rowUnit :: Row -> Text
rowUnit (Header uid _) = uid
rowUnit (ModuleRow uid _ _) = uid

-- | Arrange units into a flat row sequence: a header per unit, followed by its module rows when expanded.
buildRows :: Set Text -> [Entry] -> Seq.Seq Row
buildRows expanded units =
  Seq.fromList (concatMap renderUnit units)
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

-- | The unit name owning the currently selected row, used to target the 'b' build (metadata) action regardless of
-- whether a header or one of its module children is selected.
selectedUnit :: State -> Maybe Text
selectedUnit State{rows} = rowUnit . snd <$> listSelectedElement rows

draw :: Name -> State -> Widget Name
draw current State{rows} = renderList drawRow (current == TaskTree) rows
 where
  drawRow isSel row = (if isSel then withAttr disabledAttr else id) (drawRow' row)
  drawRow' (Header uid expanded') =
    str ((if expanded' then "\9662 " else "\9656 ") ++ Text.unpack uid)
  drawRow' (ModuleRow _ m isLast) =
    str ("  " ++ (if isLast then "\9492\9472 " else "\9500\9472 ") ++ Text.unpack m)
