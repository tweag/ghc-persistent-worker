module UI.ActiveTasks where

import Brick.AttrMap (AttrName)
import Brick.Types (EventM, Widget)
import Brick.Widgets.Core (Padding (..), padRight, str, strWrap, withAttr, (<+>))
import Brick.Widgets.List (GenericList, list, listElementsL, listSelectedElementL, listSelectedL, renderList)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)
import Data.Sequence qualified as Seq
import Data.Time (UTCTime, diffUTCTime, getCurrentTime, nominalDiffTimeToSeconds)
import Lens.Micro.Platform (modifying, preuse, use, (.=))
import Types.Target (TargetSpec (..), renderTargetSpec)
import UI.Types (Name (ActiveTasks), WorkerId, canDebugAttr, taskFailedAttr, taskRunningAttr, taskSucceededAttr)
import UI.Utils (formatPico, popup, wideStr)

type State = GenericList Name Seq.Seq Task

initialState :: State
initialState = list ActiveTasks Seq.empty 1

-- | The outcome of a task, once it has finished. A task with no outcome yet is still running.
data Outcome
  = Succeeded
  | Failed String

data Task = Task
  { _taskTarget :: TargetSpec
  , _taskStartTime :: UTCTime
  , _taskEndTime :: Maybe UTCTime
  , _outcome :: Maybe Outcome
  , _fromWorker :: WorkerId
  , _canDebug :: Bool
  }

-- | The emoji and attribute used to indicate a task's current state.
stateMarker :: Task -> (String, AttrName)
stateMarker Task{_outcome} =
  case _outcome of
    Nothing -> ("\9203", taskRunningAttr) -- \x23F3 hourglass
    Just Succeeded -> ("\9989", taskSucceededAttr) -- \x2705 check mark
    Just (Failed _) -> ("\10060", taskFailedAttr) -- \x274C cross mark

draw :: Name -> UTCTime -> State -> Widget Name
draw current now = renderList drawTask (current == ActiveTasks)
 where
  drawTask _ task@Task{_taskTarget = name, ..} =
    let (emoji, attr) = stateMarker task
        elapsed = nominalDiffTimeToSeconds (max 0 (diffUTCTime (fromMaybe now _taskEndTime) _taskStartTime))
        status = case _outcome of
          Nothing -> formatPico elapsed
          Just Succeeded -> formatPico elapsed
          Just (Failed _) -> "Failure"
     in (if _canDebug then withAttr canDebugAttr else id) $
          withAttr attr $
            -- The emoji markers (\9203/\9989/\10060) are 'Emoji_Presentation=Yes' codepoints: terminals
            -- almost universally render them as double-width using an emoji font, but vty's 'textWidth'
            -- (derived from Unicode's East Asian Width property, which doesn't model emoji presentation)
            -- reports them as a single column. 'wideStr' builds the emoji through vty's low-level
            -- 'HorizText' constructor with an explicit display width of 2, bypassing wcwidth entirely.
            wideStr 2 emoji <+> str " " <+> padRight Max (str (renderTargetSpec name)) <+> str status

drawTaskDetails :: Task -> Widget Name
drawTaskDetails Task{_taskTarget = name, ..} =
  popup 70 (renderTargetSpec name) $
    strWrap $ case _outcome of
      Just (Failed content) -> content
      _ -> ""

addTask :: TargetSpec -> WorkerId -> Bool -> EventM Name State ()
addTask name wid canDebug = do
  time <- liftIO getCurrentTime
  tasks <- use listElementsL
  let i = if canDebug then 0 else fromMaybe 0 (Seq.findIndexL (not . _canDebug) tasks)
  listElementsL .= Seq.insertAt i (Task name time Nothing Nothing wid canDebug) tasks
  modifying listSelectedL (Just . maybe i (\i' -> if i' >= i then i' + 1 else i'))

-- | Mark a task as finished with the given outcome, keeping it in the list indefinitely instead of removing it.
completeTask :: TargetSpec -> Outcome -> EventM Name State ()
completeTask target outcome = do
  time <- liftIO getCurrentTime
  tasks <- use listElementsL
  case Seq.findIndexL ((== target) . _taskTarget) tasks of
    Just i -> listElementsL .= Seq.adjust' (\t -> t{_outcome = Just outcome, _taskEndTime = Just time}) i tasks
    Nothing -> pure ()

getSelectedTarget :: EventM Name State (Maybe (WorkerId, TargetSpec))
getSelectedTarget = do
  mtask <- preuse listSelectedElementL
  pure $ (\Task{_fromWorker = wid, _taskTarget = target} -> (wid, target)) <$> mtask
