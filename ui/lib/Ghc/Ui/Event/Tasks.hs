module Ghc.Ui.Event.Tasks where

import Brick (EventM, lookupViewport, setTop, viewportScroll, vpTop)
import Brick.Widgets.List (listElementsL, listSelectedElementL, listSelectedL)
import Control.Lens (modifying, preuse, use, (%=), (.=), (^.))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State (MonadState)
import Data.Functor ((<&>))
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Sequence qualified as Seq
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Time (getCurrentTime)
import Ghc.Ui.Data.Name (Name (Tasks))
import Ghc.Ui.Data.Tasks (Outcome (..), PhaseInfo (..), Task (..), TasksRow (..), TasksState, newTask)
import Ghc.Ui.Data.WorkerId (WorkerId)
import Types.Api (ProcessStats, Target)

-- | Insert a row at index @i@ of the given (pre-fetched) element sequence, preserving the logical selection
-- (mirroring the previous element it pointed to) the same way 'addTask' always did, and -- new -- keeping the
-- viewport pinned to the top if it was already there before the insertion. Without this, a caller scrolled all
-- the way up (watching the list from its oldest visible entry) would have that view silently shifted down by
-- one row every time a new entry appears above it, even though nothing asked the viewport to move.
insertRow :: Int -> TasksRow -> Seq TasksRow -> EventM Name TasksState ()
insertRow i row rows = do
  atTop <- maybe True ((== 0) . (^. vpTop)) <$> lookupViewport Tasks
  listElementsL .= Seq.insertAt i row rows
  if atTop
    then do
      -- Pin the selection to the freshly inserted row too, not just the viewport: 'renderList' wraps the
      -- selected row in Brick's 'visible' combinator, which forces the viewport back to wherever the
      -- selection is on every render. Leaving the selection on its old (now shifted) row would fight the
      -- 'setTop' below as soon as that row scrolls out of view, which is why the previous version of this
      -- function only kept the view pinned for a single insertion.
      listSelectedL .= Just i
      setTop (viewportScroll Tasks) 0
    else
      modifying listSelectedL (Just . maybe i (\i' -> if i' >= i then i' + 1 else i'))

addTask :: Target -> WorkerId -> Bool -> Bool -> Int -> EventM Name TasksState ()
addTask name wid debuggable process requestId = do
  task <- liftIO $ newTask name wid debuggable process requestId
  rows <- use listElementsL
  let i = if debuggable then 0 else fromMaybe 0 (Seq.findIndexL (not . debuggableRow) rows)
  insertRow i (TaskRow task) rows
 where
  debuggableRow (TaskRow task) = task.debuggable
  debuggableRow (Separator _) = False

-- | Insert a separator row at the top of the list, marking the boundary of a just-completed build request (see
-- 'Types.Api.Event'\'s @RequestCompleted@).
addSeparator :: Text -> EventM Name TasksState ()
addSeparator msg = do
  rows <- use listElementsL
  insertRow 0 (Separator msg) rows

-- | Mark the task with the given request id as finished with the given outcome, keeping it in the list
-- indefinitely instead of removing it. Matches by request id rather than by target text: distinct 'Build'\
-- /'TriggerExecute' dispatches for the same target (e.g. a direct request and a transitive dependency of
-- another request) each get their own row, and matching by target alone risked completing the wrong one.
completeTask ::
  MonadIO m =>
  MonadState TasksState m =>
  Int ->
  Outcome ->
  Maybe ProcessStats ->
  m ()
completeTask requestId outcome stats = do
  time <- liftIO getCurrentTime
  listElementsL %= fmap \case
    TaskRow task | task.requestId == requestId ->
      TaskRow task {outcome = Just outcome, endTime = Just time, stats}
    row -> row

getSelectedTarget ::
  MonadState TasksState m =>
  m (Maybe (WorkerId, Target))
getSelectedTarget = do
  preuse listSelectedElementL <&> \case
    Just (TaskRow Task {worker, target}) -> Just (worker, target)
    _ -> Nothing

-- | Records the start of a phase on the matching task: sets it as the task's current phase, and, if it hasn't
-- been seen before, adds it to 'phases' with a fresh order (the number of phases already recorded) and a zero
-- duration, to be filled in once the matching 'phaseEnd' arrives.
--
-- TODO lensify the request ID selector
--
-- TODO Also use listModify etc instead
phaseStart :: Int -> String -> EventM Name TasksState ()
phaseStart requestId phase =
  listElementsL %= fmap \case
    TaskRow task | task.requestId == requestId ->
      TaskRow task {
        phase = Just phase,
        phases = Map.insertWith (const id) phase (PhaseInfo {order = length task.phases, durationMs = 0}) task.phases
      }
    row -> row

-- | Records the duration of the task's current phase (see 'phaseStart'), identified via 'phase' since
-- 'Types.Api.PhaseEnd' doesn't itself carry a phase name.
phaseEnd :: Int -> Word -> EventM Name TasksState ()
phaseEnd requestId duration =
  listElementsL %= fmap \case
    TaskRow task | task.requestId == requestId ->
      case task.phase of
        Just phase -> TaskRow task {phases = Map.adjust (\info -> info {durationMs = duration}) phase task.phases}
        Nothing -> TaskRow task
    row -> row
