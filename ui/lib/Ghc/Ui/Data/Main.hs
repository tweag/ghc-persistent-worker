module Ghc.Ui.Data.Main where

import Brick.Forms (Form, newForm)
import Brick.Widgets.List (listSelectedElementL)
import Control.Lens (Traversal')
import Data.Text (Text)
import Data.Time (UTCTime (..), fromGregorian)
import GHC.Generics (Generic)
import Ghc.Ui.Data.Name (Name (..))
import qualified Ghc.Ui.Data.OpLog as OpLog
import Ghc.Ui.Data.OpLog (OpLevel, OpLogState)
import Ghc.Ui.Data.ServerProcess (ServerConfig, ServerRoot, newServerConfig, serverConfigFields)
import Ghc.Ui.Data.Session (SessionState)
import qualified Ghc.Ui.Data.Sessions as Sessions
import Ghc.Ui.Data.Sessions (SessionsEvent, SessionsState)
import Types.Api (Target)

data MainEvent =
  SetTime { time :: UTCTime }
  |
  Sessions { event :: SessionsEvent }
  |
  ServerStopped { failedPath :: Maybe ServerRoot, stderr :: Text }
  |
  OpLogMessage { level :: OpLevel, message :: Text }
  |
  ShutdownComplete
  |
  CleanCompleted { target :: Target }
  deriving stock (Show)

data MainState =
  MainState {
    sessions :: SessionsState,
    serverForm :: Form ServerConfig MainEvent Name,
    currentFocus :: Name,
    previousFocus :: Name,
    currentTime :: UTCTime,
    opLog :: OpLogState
  }
  deriving stock (Generic)

initialState :: MainState
initialState =
  MainState {
    sessions = Sessions.initialState,
    currentFocus = Global,
    previousFocus = Global,
    currentTime = UTCTime (fromGregorian 1970 1 1) 0,
    serverForm = newForm serverConfigFields newServerConfig,
    opLog = OpLog.initialState
  }

currentSession :: Traversal' MainState SessionState
currentSession = #sessions . listSelectedElementL
