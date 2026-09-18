module Ghc.Ui.Data.Session where

import Data.Map qualified as Map
import Data.Map (Map)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Ghc.Ui.Data.Log qualified as Log
import Ghc.Ui.Data.Log (LogState)
import Ghc.Ui.Data.Project qualified as Project
import Ghc.Ui.Data.Project (ProjectState)
import Ghc.Ui.Data.Settings qualified as Settings
import Ghc.Ui.Data.Settings (SettingsState)
import qualified Ghc.Ui.Data.Tasks as Tasks
import Ghc.Ui.Data.Tasks (TasksState)
import Ghc.Ui.Data.WorkerId (WorkerId)
import Network.GRPC.Client (Connection)
import Types.Api qualified as Api

newtype SessionId =
  SessionId { text :: Text }
  deriving stock (Eq, Ord, Show)

data Worker =
  Worker {
    workerId :: WorkerId,
    connection :: Connection,
    stats :: Stats
  }
  deriving stock (Generic)

data Stats =
  Stats {
    memory :: Map Text Int, -- in bytes
    gc_cpu_ns :: Int,
    cpu_ns :: Int
  }

instance Semigroup Stats where
  l <> r =
    Stats {
      memory = Map.unionWith (+) l.memory r.memory,
      gc_cpu_ns = l.gc_cpu_ns + r.gc_cpu_ns,
      cpu_ns = l.cpu_ns + r.cpu_ns
    }

instance Monoid Stats where
  mempty =
    Stats {
      memory = [],
      gc_cpu_ns = 0,
      cpu_ns = 0
    }

data SessionState =
  SessionState {
    sessionId :: SessionId,
    workers :: Map WorkerId Worker,
    tasks :: TasksState,
    project :: ProjectState,
    settings :: SettingsState,
    log :: LogState,
    startTime :: UTCTime,
    endTime :: Maybe UTCTime,
    finishedWorkerStats :: Stats
  }
  deriving stock (Generic)

data SessionEvent =
  ApiEvent {
    worker :: WorkerId,
    event :: Api.Event
  }
  deriving stock (Show)

initialState :: SessionId -> UTCTime -> SessionState
initialState sessionId startTime =
  SessionState {
    sessionId,
    workers = [],
    tasks = Tasks.initialState,
    project = Project.initialState,
    settings = Settings.initialState,
    log = Log.initialState,
    startTime = startTime,
    endTime = Nothing,
    finishedWorkerStats = mempty
  }
