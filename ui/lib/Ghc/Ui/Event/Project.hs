-- | View of the project's units and modules, populated from an event sent by @ghc-server@.
module Ghc.Ui.Event.Project where

import Brick.Widgets.List (list, listFindBy, listSelectedElement, listSelectedElementL)
import Control.Lens (Prism', at, contains, ifiltered, itraversed, preuse, prism', sans, use, (%=), (.=), (?=))
import Control.Monad.State (MonadState)
import Data.Foldable (traverse_)
import Data.Functor ((<&>))
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Sequence qualified as Seq
import Data.Sequence (Seq)
import Data.Set qualified as Set
import Data.Set (Set)
import Ghc.Ui.Data.Name (Name (..))
import Ghc.Ui.Data.Project (ProjectState (..), ProjectUnit (..), Row (..))
import Types.Api (
  HomeModule (..),
  ModuleName,
  Target (..),
  TrackedBytecode (..),
  UnitName,
  homeModuleMatchTarget,
  targetContains,
  )

-- | The unit name owning a row.
rowUnit :: Row -> Maybe UnitName
rowUnit = \case
  Root -> Nothing
  Header {unit} -> Just unit
  ModuleRow {key} -> Just key.unit

moduleRows :: UnitName -> Bool -> [ModuleName] -> [Row]
moduleRows unit isLastUnit modules =
  zipWith moduleRow [1 ..] modules
  where
    moduleRow index name =
      ModuleRow {key = HomeModule {unit, name}, isLast = index == count, isLastUnit}

    count = length modules

buildRows :: Set UnitName -> [ProjectUnit] -> Seq Row
buildRows expandedUnits units =
  Seq.fromList (Root : concat (zipWith unitRow [1 ..] units))
  where
    unitRow index ProjectUnit {unit, modules} =
      Header {..} :
      if expanded then moduleRows unit isLast modules else []
      where
        expanded = Set.member unit expandedUnits
        isLast = index == count

    count = length units

-- | Rebuild the displayed row list from the current units and expand-state.
--
-- TODO is this the right way, or does Brick have some tools for inserting sequences?
-- Other than that, expanding or collapsing could also be handled incrementally directly on the Seq.
refreshRows ::
  MonadState ProjectState m =>
  m ()
refreshRows = do
  expanded <- use #expandedUnits
  units <- use #units
  #rows .= list Project (buildRows expanded units) 1

-- | Move the list's selection to a row matching the given predicate.
focusRow ::
  MonadState ProjectState m =>
  (Row -> Bool) ->
  m ()
focusRow predicate =
  #rows %= listFindBy predicate

-- | Move the list's selection to the given unit's 'Header' row.
focusHeader ::
  MonadState ProjectState m =>
  UnitName ->
  m ()
focusHeader targetUnit =
  focusRow \case
    Header {unit} -> unit == targetUnit
    _ -> False

load ::
  MonadState ProjectState m =>
  [ProjectUnit] ->
  m ()
load units = do
  #units .= units
  refreshRows

rowUnitL :: Prism' Row UnitName
rowUnitL =
  prism' (const Root) \case
    Header {unit} -> Just unit
    ModuleRow {key = HomeModule {unit}} -> Just unit
    Root -> Nothing

toggleExpand :: MonadState ProjectState m => m ()
toggleExpand =
  preuse (#rows . listSelectedElementL . rowUnitL) >>= traverse_ \ unit -> do
    #expandedUnits . contains unit %= not
    refreshRows
    focusHeader unit

-- TODO move built/failed into HomeModule (or Row or some new type)
markBuilt ::
  MonadState ProjectState m =>
  Target ->
  m ()
markBuilt target = do
  #built . at target ?= ()
  #failed %= sans target

markFailed ::
  MonadState ProjectState m =>
  Target ->
  m ()
markFailed target = do
  #failed . at target ?= ()
  #built %= sans target

clearMarks ::
  MonadState ProjectState m =>
  Target ->
  m ()
clearMarks target = do
  #built %= Set.filter (not . matches)
  #failed %= Set.filter (not . matches)
 where
  matches = targetContains target

updateBytecode ::
  MonadState ProjectState m =>
  [TrackedBytecode] ->
  m ()
updateBytecode entries =
  #bco .= Map.fromList [(entry.key, entry) | entry <- entries]

evictedBco ::
  MonadState ProjectState m =>
  Target ->
  m ()
evictedBco target =
  #bco . itraversed . ifiltered match . #pendingEviction .= True
  where
    match key _ = homeModuleMatchTarget key target

-- | The compile targets for the currently selected row, used by the 'b' build action (changed: no longer
-- includes metadata, see 'selectedMetadataTargets' for that).
--
-- Selecting the project-root node schedules a compile job for every module of every unit in the project.
-- Selecting a unit header schedules a compile job for every one of its modules (@unitName:moduleName@ for each);
-- selecting one of its module children targets only that module's compilation.
--
-- TODO can't this pass TargetProject?
selectedCompileTargets :: ProjectState -> Maybe [Target]
selectedCompileTargets ProjectState {rows, units} = do
  listSelectedElement rows >>= \case
    (_, Root) -> Just [TargetModule {key = HomeModule {..}} | ProjectUnit {unit, modules} <- units, name <- modules]
    (_, Header {unit}) -> do
      entry <- List.find ((== unit) . (.unit)) units
      pure [TargetModule {key = HomeModule {..}} | name <- entry.modules]
    (_, ModuleRow {key}) -> Just [TargetModule {key}]

selectedMetadataTargets :: ProjectState -> Maybe [Target]
selectedMetadataTargets ProjectState {rows, units} = do
  listSelectedElement rows >>= \case
    (_, Root) -> pure [TargetUnit {name = unit} | ProjectUnit {unit} <- units]
    (_, row) -> rowUnit row <&> \ name -> [TargetUnit {name}]

selectedExecuteTarget :: ProjectState -> Maybe Target
selectedExecuteTarget ProjectState {rows} = do
  listSelectedElement rows >>= \case
    (_, Root) -> Just TargetProject
    (_, Header {unit}) -> Just TargetUnit {name = unit}
    (_, ModuleRow {key}) -> Just TargetModule {key}

selectedEvictTarget :: ProjectState -> Maybe Target
selectedEvictTarget ProjectState {rows} = do
  listSelectedElement rows >>= \case
    (_, Root) -> Just TargetProject
    (_, Header {unit}) -> Just TargetUnit {name = unit}
    (_, ModuleRow {key}) -> Just TargetModule {key}

selectedCleanTarget :: ProjectState -> Maybe Target
selectedCleanTarget ProjectState {rows} = do
  listSelectedElement rows >>= \case
    (_, Root) -> Just TargetProject
    (_, Header {unit}) -> Just TargetUnit {name = unit}
    (_, ModuleRow {key}) -> Just TargetModule {key}
