module Ghc.Ui.Data.Sessions where

import Brick.Widgets.List (GenericList, list)
import Data.Sequence (Seq)
import Data.Time (UTCTime)
import Ghc.Ui.Data.Name (Name (Sessions))
import Ghc.Ui.Data.Session (SessionEvent, SessionId, SessionState (..))
import Ghc.Ui.Data.WorkerId (WorkerId)
import Network.GRPC.Client (Connection)

type SessionsState = GenericList Name Seq (SessionId, SessionState)

newtype GrpcConnection =
  GrpcConnection Connection

instance Show GrpcConnection where
  show _ = "GrpcConnection"

data SessionsEvent =
  StartSession {
    sessionId :: SessionId,
    startTime :: UTCTime
  }
  |
  EndSession { sessionId :: SessionId }
  |
  Session {
    sessionId :: SessionId,
    event :: SessionEvent
  }
  |
  AddWorker {
    sessionId :: SessionId,
    workerId :: WorkerId,
    startTime :: UTCTime,
    connection :: GrpcConnection
  }
  |
  RemoveWorker {
    sessionId :: SessionId,
    workerId :: WorkerId
  }
  deriving stock (Show)

initialState :: SessionsState
initialState = list Sessions [] 1
