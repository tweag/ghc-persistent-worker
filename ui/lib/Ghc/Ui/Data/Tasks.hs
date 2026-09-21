module Ghc.Ui.Data.Tasks where

import Brick.Widgets.List (GenericList, list)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Map (Map)
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Time (UTCTime, getCurrentTime)
import Ghc.Ui.Data.Name (Name (Tasks))
import Ghc.Ui.Data.WorkerId (WorkerId)
import Types.Api (ProcessStats, Target)

-- | The outcome of a task, once it has finished. A task with no outcome yet is still running. A successful
-- execute task (see 'Types.Api.Event'\'s @CompileEnd@ @result@ field) carries the exfiltrated @main@
-- return value, when one was available; compile\/metadata tasks always carry 'Nothing' here.
data Outcome =
  Succeeded (Maybe String)
  |
  Failed String

-- | A single phase's recorded stats for a task (see 'Task'\'s @phases@ field): the order in which the phase
-- was first started, used to list phases chronologically in 'drawTaskDetails', and its most recently reported
-- duration (see 'Types.Api.PhaseEnd'). Zero until the corresponding 'phaseEnd' call arrives.
data PhaseInfo =
  PhaseInfo {
    order :: Int,
    durationMs :: Word
  }

data Task =
  Task {
    target :: Target,
    startTime :: UTCTime,
    endTime :: Maybe UTCTime,
    outcome :: Maybe Outcome,
    worker :: WorkerId,
    debuggable :: Bool,
    -- | Whether this task instance ran (or is running) in a self-relaunched subprocess (see
    -- 'GhcServer.Build.Process'). Always 'False' except for execute tasks dispatched with @--process@.
    process :: Bool,
    -- | RTS memory stats reported by a subprocess execute task's child process (see 'Types.Api.ProcessStats'),
    -- populated once the task's 'CompileEnd' event arrives. 'Nothing' before then, and always 'Nothing' for
    -- tasks that didn't run in a subprocess.
    stats :: Maybe ProcessStats,
    phase :: Maybe String,
    phases :: Map String PhaseInfo,
    -- | The id allocated by the @instrument@ UI for the request that spawned this task (see
    -- 'UI.allocRequestId'), echoed back by the server in every 'Types.Api.Event' belonging to it.
    -- Identifies this row unambiguously, so 'completeTask'\/'phaseStart'\/'phaseEnd' can match the exact task
    -- instance instead of matching by target text, which collides when the same target is dispatched more than
    -- once (e.g. as both a direct request and a transitive dependency of another request).
    requestId :: Int
  }

newTask ::
  MonadIO m =>
  Target ->
  WorkerId ->
  Bool ->
  Bool ->
  Int ->
  m Task
newTask target worker debuggable process requestId = do
  startTime <- liftIO getCurrentTime
  pure Task {
    target,
    startTime,
    endTime = Nothing,
    outcome = Nothing,
    worker,
    debuggable,
    process,
    stats = Nothing,
    phase = Nothing,
    phases = [],
    requestId
  }

data TasksRow =
  TaskRow Task
  |
  Separator Text

rowTask :: TasksRow -> Maybe Task
rowTask (TaskRow t) = Just t
rowTask (Separator _) = Nothing

type TasksState = GenericList Name Seq TasksRow

initialState :: TasksState
initialState = list Tasks [] 1
