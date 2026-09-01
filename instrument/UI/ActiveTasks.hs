module UI.ActiveTasks where

import Brick.AttrMap (AttrName)
import Brick.Main (lookupViewport, setTop, viewportScroll)
import Brick.Types (EventM, Widget, vpTop)
import Brick.Widgets.Core (Padding (..), padLeft, str, strWrap, txt, vBox, vLimit, withAttr, (<+>))
import Brick.Widgets.List (GenericList, list, listElementsL, listSelectedElementL, listSelectedL, renderList)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.List (sortOn)
import Data.Map.Strict qualified as Map
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import Data.Sequence qualified as Seq
import Data.Sequence (Seq)
import Data.Time (UTCTime, defaultTimeLocale, diffUTCTime, formatTime, getCurrentTime, nominalDiffTimeToSeconds)
import Lens.Micro.Platform (modifying, preuse, use, (.=), (^.))
import Types.Target (TargetSpec (..), renderTargetSpec)
import qualified UI.OpLog as OpLog
import UI.Types (
  Name (ActiveTasks),
  WorkerId,
  canDebugAttr,
  disabledAttr,
  opLogIndicatorAttr,
  sectionActiveTasksAttr,
  taskFailedAttr,
  taskNameAttr,
  taskPhaseAttr,
  taskResultAttr,
  taskRunningAttr,
  taskSucceededAttr,
  taskTimeAttr,
  )
import UI.Utils (drawSection, formatPico, popup, styledTarget)

type State = GenericList Name Seq.Seq Row

initialState :: State
initialState = list ActiveTasks Seq.empty 1

-- | The outcome of a task, once it has finished. A task with no outcome yet is still running. A successful
-- execute task (see 'Types.Instrument.Event'\'s @CompileEnd@ @result@ field) carries the exfiltrated @main@
-- return value, when one was available; compile\/metadata tasks always carry 'Nothing' here.
data Outcome
  = Succeeded (Maybe String)
  | Failed String

-- | A single phase's recorded stats for a task (see 'Task'\'s @_phases@ field): the order in which the phase
-- was first started, used to list phases chronologically in 'drawTaskDetails', and its most recently reported
-- duration (see 'Types.Instrument.PhaseEnd'). Zero until the corresponding 'phaseEnd' call arrives.
data PhaseInfo = PhaseInfo
  { _phaseOrder :: Int
  , _phaseDurationMs :: Word
  }

data Task = Task
  { _taskTarget :: TargetSpec
  , _taskStartTime :: UTCTime
  , _taskEndTime :: Maybe UTCTime
  , _outcome :: Maybe Outcome
  , _fromWorker :: WorkerId
  , _canDebug :: Bool
  , _phase :: Maybe String
  , _phases :: Map String PhaseInfo
  -- | The id allocated by the @instrument@ UI for the request that spawned this task (see
  -- 'UI.allocRequestId'), echoed back by the server in every 'Types.Instrument.Event' belonging to it.
  -- Identifies this row unambiguously, so 'completeTask'\/'phaseStart'\/'phaseEnd' can match the exact task
  -- instance instead of matching by target text, which collides when the same target is dispatched more than
  -- once (e.g. as both a direct request and a transitive dependency of another request).
  , _taskRequestId :: Int
  }

newTask ::
  MonadIO m =>
  TargetSpec ->
  WorkerId ->
  Bool ->
  Int ->
  m Task
newTask _taskTarget _fromWorker _canDebug _taskRequestId = do
  _taskStartTime <- liftIO getCurrentTime
  pure Task {
    _taskTarget,
    _taskStartTime,
    _taskEndTime = Nothing,
    _outcome = Nothing,
    _fromWorker,
    _canDebug,
    _phase = Nothing,
    _phases = [],
    _taskRequestId
  }

-- | A row in the displayed list: either a task, or a separator marking the boundary of a completed build
-- request (see 'Types.Instrument.Event'\'s @RequestCompleted@), always carrying a message (e.g. "All tasks
-- concluded") since the scheduler-queue-exhaustion redesign made this a rare, always-meaningful event rather
-- than one fired per request.
data Row
  = TaskRow Task
  | Separator String

-- | The task a row carries, if it is a 'TaskRow'.
rowTask :: Row -> Maybe Task
rowTask (TaskRow t) = Just t
rowTask (Separator _) = Nothing

-- | The marker string and attribute used to indicate a task's current state.
--
-- Plain, single-width characters (an ellipsis for "still running", a check mark for success, a ballot X for
-- failure) rather than emoji-presentation glyphs, so no 'UI.Utils.wideStr' explicit-width workaround is needed.
stateMarker :: Task -> (String, AttrName)
stateMarker Task{_outcome, _phase} =
  case (_outcome, _phase) of
    (Nothing, Just phase) -> (phase, taskPhaseAttr)
    (Nothing, Nothing) -> ("...", taskRunningAttr)
    (Just (Succeeded _), _) -> ("\10004", taskSucceededAttr) -- \x2714 heavy check mark
    (Just (Failed _), _) -> ("\10008", taskFailedAttr) -- \x2718 heavy ballot X

-- | Header line replacing the border that used to delimit this panel; see 'UI.Types.sectionActiveTasksAttr'.
-- Uses 'UI.Utils.drawSection's permanent placeholder rectangle for visual structure.
draw :: Name -> UTCTime -> State -> Widget Name
draw current now st =
  drawSection sectionActiveTasksAttr (withAttr sectionActiveTasksAttr (str "Active Tasks")) $
    renderList drawRow (current == ActiveTasks) st
 where
  drawRow _ (Separator msg) =
    withAttr disabledAttr $ str ("\9472\9472 " ++ msg ++ " \9472\9472")
  drawRow _ (TaskRow task@Task{_taskTarget = name, ..}) =
    let (status, attr) = stateMarker task
        elapsed = nominalDiffTimeToSeconds (max 0 (diffUTCTime (fromMaybe now _taskEndTime) _taskStartTime))
        progress = case _outcome of
          Just (Failed _) -> "Failure"
          _ -> formatPico elapsed
        timestamp = withAttr taskTimeAttr (str (formatTime defaultTimeLocale "%H:%M:%S" _taskStartTime ++ " "))
        header =
          (if _canDebug then withAttr canDebugAttr else id) $
            -- The timestamp (subdued\/dim, mirroring 'progressLine' below) leads the label; the marker
            -- ('stateMarker') is a plain single-width character, moved to the end of the row instead of
            -- leading it. The target name itself is rendered via 'UI.Utils.styledTarget' for the
            -- module\/metadata syntax highlighting, with 'taskNameAttr' as its default for the unrecognized
            -- (unit-name) part.
            timestamp
              <+> withAttr taskNameAttr (styledTarget (renderTargetSpec name))
              <+> str " "
              <+> withAttr attr (str status)
        -- Status (elapsed time or "Failure") is rendered on its own indented line below the target name,
        -- rather than right-aligned on the same line: right-aligning it made it hard to visually associate
        -- with the target it belongs to, especially once lines wrap or targets vary in length, and there is
        -- no need for rows to stretch to the panel's full width just to right-align one word. This mirrors
        -- how an execute task's result is already shown on its own line below ('drawResult').
        progressLine = padLeft (Pad 2) (withAttr taskTimeAttr (str progress))
        result = case _outcome of
          Just (Succeeded (Just r)) -> Just r
          _ -> Nothing
     in vBox ([header, progressLine] ++ maybe [] (pure . drawResult) result)

  -- A successful execute task's exfiltrated result, rendered on the lines following its row: wrapped to the
  -- available width, truncated to 4 lines, indented by two cells, and left uncolored (unlike the marker/status
  -- above it).
  drawResult r = padLeft (Pad 2) (vLimit 4 (withAttr opLogIndicatorAttr (txt OpLog.indicator) <+> withAttr taskResultAttr (strWrap r)))

-- | The task's target name, outcome (if finished), and recorded phases (see 'Task'\'s @_phases@ field), in a
-- single fixed-size popup (merging what used to be two separate popups bound to 'p'\/'Enter' -- they largely
-- duplicated each other's purpose, and neither made sense without the other: the outcome view had no way to
-- show timing, and the phase view had no way to show the failure\/result text). The target name is always
-- shown first, unconditionally -- not just in the popup's border label -- so a metadata task with neither an
-- outcome yet nor any recorded phases still shows something rather than an empty body.
drawTaskDetails :: Task -> Widget Name
drawTaskDetails Task{_taskTarget = name, _phases, ..} =
  popup 30 (renderTargetSpec name) $
    vBox $
      withAttr taskNameAttr (styledTarget (renderTargetSpec name))
        : outcomeLines
        ++ phaseLines
 where
  outcomeLines = case _outcome of
    Just (Failed content) -> [strWrap content]
    Just (Succeeded (Just result)) -> [strWrap ("Result: " ++ result)]
    _ -> []
  phaseLines
    | Map.null _phases = []
    | otherwise = str " " : map drawPhase (sortOn (_phaseOrder . snd) (Map.toList _phases))
  drawPhase (phase, PhaseInfo{_phaseDurationMs}) =
    str phase <+> str (replicate 2 ' ') <+> withAttr taskTimeAttr (str (show _phaseDurationMs ++ "ms"))

-- | Insert a row at index @i@ of the given (pre-fetched) element sequence, preserving the logical selection
-- (mirroring the previous element it pointed to) the same way 'addTask' always did, and -- new -- keeping the
-- viewport pinned to the top if it was already there before the insertion. Without this, a caller scrolled all
-- the way up (watching the list from its oldest visible entry) would have that view silently shifted down by
-- one row every time a new entry appears above it, even though nothing asked the viewport to move.
insertRow :: Int -> Row -> Seq Row -> EventM Name State ()
insertRow i row rows = do
  atTop <- maybe True ((== 0) . (^. vpTop)) <$> lookupViewport ActiveTasks
  listElementsL .= Seq.insertAt i row rows
  if atTop
    then do
      -- Pin the selection to the freshly inserted row too, not just the viewport: 'renderList' wraps the
      -- selected row in Brick's 'visible' combinator, which forces the viewport back to wherever the
      -- selection is on every render. Leaving the selection on its old (now shifted) row would fight the
      -- 'setTop' below as soon as that row scrolls out of view, which is why the previous version of this
      -- function only kept the view pinned for a single insertion.
      listSelectedL .= Just i
      setTop (viewportScroll ActiveTasks) 0
    else
      modifying listSelectedL (Just . maybe i (\i' -> if i' >= i then i' + 1 else i'))

addTask :: TargetSpec -> WorkerId -> Bool -> Int -> EventM Name State ()
addTask name wid canDebug requestId = do
  task <- liftIO $ newTask name wid canDebug requestId
  rows <- use listElementsL
  let i = if canDebug then 0 else fromMaybe 0 (Seq.findIndexL (not . isCanDebugRow) rows)
  insertRow i (TaskRow task) rows
 where
  isCanDebugRow (TaskRow t) = _canDebug t
  isCanDebugRow (Separator _) = False

-- | Insert a separator row at the top of the list, marking the boundary of a just-completed build request (see
-- 'Types.Instrument.Event'\'s @RequestCompleted@).
addSeparator :: String -> EventM Name State ()
addSeparator msg = do
  rows <- use listElementsL
  insertRow 0 (Separator msg) rows

-- | Mark the task with the given request id as finished with the given outcome, keeping it in the list
-- indefinitely instead of removing it. Matches by request id rather than by target text: distinct 'TriggerBuild'\
-- /'TriggerExecute' dispatches for the same target (e.g. a direct request and a transitive dependency of
-- another request) each get their own row, and matching by target alone risked completing the wrong one.
completeTask :: Int -> Outcome -> EventM Name State ()
completeTask requestId outcome = do
  time <- liftIO getCurrentTime
  let complete row = case row of
        TaskRow t | _taskRequestId t == requestId -> TaskRow t{_outcome = Just outcome, _taskEndTime = Just time}
        _ -> row
  modifying listElementsL (fmap complete)

getSelectedTarget :: EventM Name State (Maybe (WorkerId, TargetSpec))
getSelectedTarget = do
  mrow <- preuse listSelectedElementL
  pure $ mrow >>= \row -> (\Task{_fromWorker = wid, _taskTarget = target} -> (wid, target)) <$> rowTask row

-- | Records the start of a phase on the matching task: sets it as the task's current phase, and, if it hasn't
-- been seen before, adds it to '_phases' with a fresh order (the number of phases already recorded) and a zero
-- duration, to be filled in once the matching 'phaseEnd' arrives.
phaseStart :: Int -> String -> EventM Name State ()
phaseStart requestId phase =
  modifying listElementsL $ fmap \case
    TaskRow t | _taskRequestId t == requestId ->
      TaskRow
        t
          { _phase = Just phase
          , _phases = Map.insertWith (\_new old -> old) phase (PhaseInfo (Map.size (_phases t)) 0) (_phases t)
          }
    row -> row

-- | Records the duration of the task's current phase (see 'phaseStart'), identified via '_phase' since
-- 'Types.Instrument.PhaseEnd' doesn't itself carry a phase name.
phaseEnd :: Int -> Word -> EventM Name State ()
phaseEnd requestId duration =
  modifying listElementsL $ fmap \case
    TaskRow t | _taskRequestId t == requestId ->
      case _phase t of
        Just phase -> TaskRow t {_phases = Map.adjust (\info -> info {_phaseDurationMs = duration}) phase (_phases t)}
        Nothing -> TaskRow t
    row -> row
