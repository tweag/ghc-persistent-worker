{-# LANGUAGE TemplateHaskell #-}

module UI.Session where

import Brick.Types (EventM, Widget)
import Brick.Widgets.Border (borderWithLabel, hBorder, vBorder)
import Brick.Widgets.Core (hBox, hLimitPercent, str, vBox, vLimitPercent)
import Data.Map qualified as Map
import Data.Text qualified as Text
import Data.Time (UTCTime)
import Lens.Micro.Platform (each, filtered, makeLenses, modifying, use, zoom)
import Network.GRPC.Client (Connection)
import Types.Instrument qualified as Instr
import Types.Target (TargetSpec (..))
import UI.ActiveTasks qualified as ActiveTasks
import UI.BytecodeBrowser qualified as BytecodeBrowser
import UI.LogViewer qualified as LogViewer
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

-- | Draws the session panel: active tasks on top, then a horizontal split of the project task tree (left) and the
-- bytecode-cache browser (right, replacing the previous popup dialog), then the worker stats footer.
draw :: Name -> UTCTime -> BytecodeBrowser.State -> State -> Widget Name
draw current now bco Session{..} =
  borderWithLabel (str $ " GHC Persistent Worker  " ++ _title ++ " ") $
    vBox
      [ vLimitPercent 30 $ ActiveTasks.draw current now _activeTasks
      , hBorder
      , hBox
          [ hLimitPercent 50 $ TaskTree.draw current _taskTree
          , vBorder
          , BytecodeBrowser.draw current bco
          ]
      , hBorder
      , drawStats (length _workers) (foldMap _stats _workers <> _finishedWorkerStats)
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
          zoom activeTasks $ ActiveTasks.completeTask target' ActiveTasks.Succeeded
          zoom taskTree $ TaskTree.handleEvent $ TaskTree.MarkBuilt (Text.pack rawTarget)
        else zoom activeTasks $ ActiveTasks.completeTask target' (ActiveTasks.Failed content)
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
    Instr.LogMessage {..} ->
      modifying logViewer $
        LogViewer.addEntry
          LogViewer.Entry
            { target = Text.pack target
            , level = Text.pack level
            , message = Text.pack message
            , timestampMs
            }

removeWorker :: WorkerId -> EventM Name State ()
removeWorker wid = do
  st <- use (workers . each . filtered (\w -> w._workerId == wid) . stats)
  modifying finishedWorkerStats (<> st{_memory = mempty})
  modifying workers (filter (\w -> w._workerId /= wid))
