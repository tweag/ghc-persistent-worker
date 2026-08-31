module UI.ActiveTasks where

import Brick.AttrMap (AttrName)
import Brick.Main (lookupViewport, setTop, viewportScroll)
import Brick.Types (EventM, Widget, vpTop)
import Brick.Widgets.Core (Padding (..), padLeft, padRight, str, strWrap, vBox, vLimit, withAttr, (<+>))
import Brick.Widgets.List (GenericList, list, listElementsL, listSelectedElementL, listSelectedL, renderList)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Time (UTCTime, diffUTCTime, getCurrentTime, nominalDiffTimeToSeconds)
import Lens.Micro.Platform (modifying, preuse, use, (.=), (^.))
import Types.Target (TargetSpec (..), renderTargetSpec)
import UI.Types (Name (ActiveTasks), WorkerId, canDebugAttr, disabledAttr, taskFailedAttr, taskRunningAttr, taskSucceededAttr)
import UI.Utils (formatPico, popup)

type State = GenericList Name Seq.Seq Row

initialState :: State
initialState = list ActiveTasks Seq.empty 1

-- | The outcome of a task, once it has finished. A task with no outcome yet is still running. A successful
-- execute task (see 'Types.Instrument.Event'\'s @CompileEnd@ @result@ field) carries the exfiltrated @main@
-- return value, when one was available; compile\/metadata tasks always carry 'Nothing' here.
data Outcome
  = Succeeded (Maybe String)
  | Failed String

data Task = Task
  { _taskTarget :: TargetSpec
  , _taskStartTime :: UTCTime
  , _taskEndTime :: Maybe UTCTime
  , _outcome :: Maybe Outcome
  , _fromWorker :: WorkerId
  , _canDebug :: Bool
  }

-- | A row in the displayed list: either a task, or a separator marking the boundary of a completed build
-- request (see 'Types.Instrument.Event'\'s @RequestCompleted@). 'Nothing' renders as a plain separator line;
-- @Just msg@ additionally shows @msg@ (e.g. "All targets up to date") for a request that produced no new task
-- completions.
data Row
  = TaskRow Task
  | Separator (Maybe String)

-- | The task a row carries, if it is a 'TaskRow'.
rowTask :: Row -> Maybe Task
rowTask (TaskRow t) = Just t
rowTask (Separator _) = Nothing

-- | The marker string and attribute used to indicate a task's current state.
--
-- Plain, single-width characters (an ellipsis for "still running", a check mark for success, a ballot X for
-- failure) rather than emoji-presentation glyphs, so no 'UI.Utils.wideStr' explicit-width workaround is needed.
stateMarker :: Task -> (String, AttrName)
stateMarker Task{_outcome} =
  case _outcome of
    Nothing -> ("...", taskRunningAttr)
    Just (Succeeded _) -> ("\10004", taskSucceededAttr) -- \x2714 heavy check mark
    Just (Failed _) -> ("\10008", taskFailedAttr) -- \x2718 heavy ballot X

draw :: Name -> UTCTime -> State -> Widget Name
draw current now = renderList drawRow (current == ActiveTasks)
 where
  drawRow _ (Separator msg) =
    withAttr disabledAttr $ str (maybe (replicate 40 '\9472') (\m -> "\9472\9472 " ++ m ++ " \9472\9472") msg)
  drawRow _ (TaskRow task@Task{_taskTarget = name, ..}) =
    let (emoji, attr) = stateMarker task
        elapsed = nominalDiffTimeToSeconds (max 0 (diffUTCTime (fromMaybe now _taskEndTime) _taskStartTime))
        status = case _outcome of
          Just (Failed _) -> "Failure"
          _ -> formatPico elapsed
        header =
          (if _canDebug then withAttr canDebugAttr else id) $
            -- The markers ('stateMarker') are plain single-width characters, so no explicit-width
            -- workaround ('UI.Utils.wideStr') is needed here, unlike the emoji they replaced. Only the
            -- marker itself is colored by the outcome attribute -- the elapsed-time\/status text stays
            -- uncolored.
            withAttr attr (str emoji) <+> str " " <+> padRight Max (str (renderTargetSpec name)) <+> str status
        result = case _outcome of
          Just (Succeeded (Just r)) -> Just r
          _ -> Nothing
     in vBox (header : maybe [] (pure . drawResult) result)

  -- A successful execute task's exfiltrated result, rendered on the lines following its row: wrapped to the
  -- available width, truncated to 4 lines, indented by two cells, and left uncolored (unlike the marker/status
  -- above it).
  drawResult r = padLeft (Pad 2) (vLimit 4 (strWrap r))

drawTaskDetails :: Task -> Widget Name
drawTaskDetails Task{_taskTarget = name, ..} =
  popup 70 (renderTargetSpec name) $
    strWrap $ case _outcome of
      Just (Failed content) -> content
      Just (Succeeded (Just result)) -> "Result: " ++ result
      _ -> ""

-- | Insert a row at index @i@ of the given (pre-fetched) element sequence, preserving the logical selection
-- (mirroring the previous element it pointed to) the same way 'addTask' always did, and -- new -- keeping the
-- viewport pinned to the top if it was already there before the insertion. Without this, a caller scrolled all
-- the way up (watching the list from its oldest visible entry) would have that view silently shifted down by
-- one row every time a new entry appears above it, even though nothing asked the viewport to move.
insertRow :: Int -> Row -> Seq Row -> EventM Name State ()
insertRow i row rows = do
  atTop <- maybe True ((== 0) . (^. vpTop)) <$> lookupViewport ActiveTasks
  listElementsL .= Seq.insertAt i row rows
  modifying listSelectedL (Just . maybe i (\i' -> if i' >= i then i' + 1 else i'))
  when atTop $ setTop (viewportScroll ActiveTasks) 0

addTask :: TargetSpec -> WorkerId -> Bool -> EventM Name State ()
addTask name wid canDebug = do
  time <- liftIO getCurrentTime
  rows <- use listElementsL
  let i = if canDebug then 0 else fromMaybe 0 (Seq.findIndexL (not . isCanDebugRow) rows)
  insertRow i (TaskRow (Task name time Nothing Nothing wid canDebug)) rows
 where
  isCanDebugRow (TaskRow t) = _canDebug t
  isCanDebugRow (Separator _) = False

-- | Insert a separator row at the top of the list, marking the boundary of a just-completed build request (see
-- 'Types.Instrument.Event'\'s @RequestCompleted@).
addSeparator :: Maybe String -> EventM Name State ()
addSeparator msg = do
  rows <- use listElementsL
  insertRow 0 (Separator msg) rows

-- | Mark all tasks matching the given target as finished with the given outcome, keeping them in the list
-- indefinitely instead of removing them. Updates every match rather than just the first, since duplicate
-- entries for the same target can occur (e.g. retries), and picking only the first risks completing an
-- already-finished row instead of the one the outcome actually belongs to.
completeTask :: TargetSpec -> Outcome -> EventM Name State ()
completeTask target outcome = do
  time <- liftIO getCurrentTime
  let complete row = case row of
        TaskRow t | _taskTarget t == target -> TaskRow t{_outcome = Just outcome, _taskEndTime = Just time}
        _ -> row
  modifying listElementsL (fmap complete)

getSelectedTarget :: EventM Name State (Maybe (WorkerId, TargetSpec))
getSelectedTarget = do
  mrow <- preuse listSelectedElementL
  pure $ mrow >>= \row -> (\Task{_fromWorker = wid, _taskTarget = target} -> (wid, target)) <$> rowTask row
