{-# LANGUAGE TemplateHaskell #-}

module UI.Session where

import Brick.Types (EventM, Widget)
import Brick.Widgets.Border (hBorder)
import Brick.Widgets.Core (Padding (Pad), hBox, hLimitPercent, padBottom, padLeft, padRight, padTop, str, vBox)
import Data.Map qualified as Map
import Data.Text qualified as Text
import Data.Time (UTCTime)
import Lens.Micro.Platform (each, filtered, makeLenses, modifying, use, zoom)
import Network.GRPC.Client (Connection)
import Types.Instrument qualified as Instr
import Types.Target (TargetSpec (..))
import UI.ActiveTasks qualified as ActiveTasks
import UI.LogViewer qualified as LogViewer
import UI.OpLog qualified as OpLog
import UI.TaskTree qualified as TaskTree
import UI.Types (Name, WorkerId)
import UI.Utils (formatBytes, formatPs, stripEscSeqs)

newtype Id = Id {unId :: Text.Text}
  deriving stock (Eq, Ord, Show)

data State = Session
  { _title :: String
  , _workers :: [Worker]
  , _activeTasks :: ActiveTasks.State
  , _taskTree :: TaskTree.State
  , _logViewer :: LogViewer.State
  , _sesStartTime :: UTCTime
  , _sesEndTime :: Maybe UTCTime
  , _finishedWorkerStats :: Stats
  }

data Worker = Worker
  { _workerId :: WorkerId
  , _connection :: Connection
  , _stats :: Stats
  }

data Stats = Stats
  { _memory :: Map.Map String Int -- in bytes
  , _gc_cpu_ns :: Int
  , _cpu_ns :: Int
  }
instance Semigroup Stats where
  Stats m1 gc1 cpu1 <> Stats m2 gc2 cpu2 =
    Stats (Map.unionWith (+) m1 m2) (gc1 + gc2) (cpu1 + cpu2)
instance Monoid Stats where
  mempty = Stats mempty 0 0

makeLenses ''State
makeLenses ''Worker

data Event
  = InstrEvent WorkerId Instr.Event

mkSession :: String -> UTCTime -> State
mkSession _title _startTime =
  Session
    { _title
    , _workers = []
    , _activeTasks = ActiveTasks.initialState
    , _taskTree = TaskTree.initialState
    , _logViewer = LogViewer.initialState
    , _sesStartTime = _startTime
    , _sesEndTime = Nothing
    , _finishedWorkerStats = mempty
    }

-- | Convert a pushed 'Instr.BytecodeSnapshot's cache-entry list into the plain tuple shape 'TaskTree.LoadBco'
-- expects (the project view now displays these merged into the tree rather than in a separate panel).
toBcoRows :: [Instr.BcoEntryInfo] -> [(Text.Text, Text.Text, Int, Int, Bool, Bool)]
toBcoRows entries =
  [ (Text.pack e.unitId, Text.pack e.moduleName, e.size, e.lastAccess, e.resident, e.pendingEviction)
  | e <- entries
  ]

-- | Draws the session panel: the project task tree on the left and the active-tasks list on the right (the
-- bytecode-cache browser that used to occupy a third column has been merged into the project tree, see
-- 'UI.TaskTree'), then the worker stats footer, and finally the operational message log (see 'UI.OpLog'),
-- capped to its 5 most recent entries and flexibly sized (no minimum height, growing up to that cap). The two
-- top panels (project\/active tasks) are delimited by colored headers and a whitespace gutter rather than
-- borders, and are inset on all four sides by a two-cell margin -- 'hBorder' is reserved for the boundaries
-- around the two bottom panels (the stats footer and the operational log, plus the key-legend bar drawn by
-- the caller, see 'UI.drawUI'), which stay flush with the screen edges.
draw :: Name -> UTCTime -> OpLog.State -> State -> Widget Name
draw current now opLog Session{..} =
  vBox
    [ padLeft (Pad 2) $
        padRight (Pad 2) $
          padTop (Pad 2) $
            padBottom (Pad 2) $
              hBox
                [ hLimitPercent 50 $ padRight (Pad 3) $ TaskTree.draw current _taskTree
                , ActiveTasks.draw current now _activeTasks
                ]
    , hBorder
    , drawStats (length _workers) (foldMap _stats _workers <> _finishedWorkerStats)
    , hBorder
    , OpLog.draw 5 opLog
    ]

drawStats :: Int -> Stats -> Widget Name
drawStats workerCount Stats{..} =
  vBox
    [ str $
        " Worker count: "
          ++ show workerCount
          ++ " | Memory:"
          ++ concatMap
            (\(k, v) -> " " ++ k ++ "=" ++ formatBytes v)
            (Map.toList _memory)
    , str $
        " CPU Time: "
          ++ formatPs (1000 * _cpu_ns)
          ++ " | GC Time: "
          ++ formatPs (1000 * _gc_cpu_ns)
    ]

handleEvent :: Event -> EventM Name State ()
handleEvent (InstrEvent wid evt) =
  case evt of
    Instr.CompileStart {..} -> do
      zoom activeTasks $ ActiveTasks.addTask (TargetUnknown target) wid canDebug
    Instr.CompileEnd {..} -> do
      let content = stripEscSeqs stderr
          rawTarget = if target == "" then takeWhile (/= ':') content else target
          target' = TargetUnknown rawTarget
      if exitCode == 0
        then do
          zoom activeTasks $ ActiveTasks.completeTask target' (ActiveTasks.Succeeded result)
          zoom taskTree $ TaskTree.handleEvent $ TaskTree.MarkBuilt (Text.pack rawTarget)
        else do
          zoom activeTasks $ ActiveTasks.completeTask target' (ActiveTasks.Failed content)
          zoom taskTree $ TaskTree.handleEvent $ TaskTree.MarkFailed (Text.pack rawTarget)
    Instr.Stats {..} -> do
        modifying (workers . each . filtered (\w -> w._workerId == wid) . stats) \st ->
          st
            { _memory = memory
            , _gc_cpu_ns = gcCpuNs
            , _cpu_ns = cpuNs
            }
    Instr.ProjectStructure {..} ->
      zoom taskTree $
        TaskTree.handleEvent $
          TaskTree.Load
            [ TaskTree.Entry (Text.pack u.unitName) (Text.pack <$> u.modules)
            | u <- units
            ]
    Instr.Halt -> pure ()
    Instr.RequestCompleted {..} ->
      zoom activeTasks $ ActiveTasks.addSeparator statusMessage
    Instr.LogMessage {..} ->
      modifying logViewer $
        LogViewer.addEntry
          LogViewer.Entry
            { target = Text.pack target
            , level = Text.pack level
            , message = Text.pack message
            , timestampMs
            }
    Instr.BytecodeSnapshot {..} ->
      zoom taskTree (TaskTree.handleEvent (TaskTree.LoadBco (toBcoRows entries)))

removeWorker :: WorkerId -> EventM Name State ()
removeWorker wid = do
  st <- use (workers . each . filtered (\w -> w._workerId == wid) . stats)
  modifying finishedWorkerStats (<> st{_memory = mempty})
  modifying workers (filter (\w -> w._workerId /= wid))
