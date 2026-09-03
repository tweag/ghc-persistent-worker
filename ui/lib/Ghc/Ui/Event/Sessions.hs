module Ghc.Ui.Event.Sessions where

import Brick (EventM, zoom)
import Brick.Widgets.List (listElementsL, listInsert, listMoveToEnd)
import Control.Lens (Traversal', _2, at, each, filtered, preuse, (%=), (?=))
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (modify')
import Data.Maybe (isNothing)
import Data.Time (UTCTime, getCurrentTime)
import Ghc.Ui.Data.Name (Name)
import qualified Ghc.Ui.Data.Session as Session
import Ghc.Ui.Data.Session (SessionId, SessionState (..), Worker (..))
import Ghc.Ui.Data.Sessions (GrpcConnection (..), SessionsEvent (..), SessionsState)
import Ghc.Ui.Data.WorkerId (WorkerId)
import qualified Ghc.Ui.Event.Session as Session
import Ghc.Ui.Event.Session (handleSessionEvent)
import Network.GRPC.Client (Connection)

sessionLens :: SessionId -> Traversal' SessionsState SessionState
sessionLens sessionId =
  listElementsL . each . filtered ((== sessionId) . fst) . _2

addWorker :: SessionId -> WorkerId -> UTCTime -> Connection -> EventM Name SessionsState ()
addWorker sessionId workerId startTime connection = do
  session <- preuse (sessionLens sessionId)
  when (isNothing session) do
    startSession sessionId startTime
  zoom (sessionLens sessionId) do
    #workers . at workerId ?= Worker {workerId, connection, stats = mempty}
    #startTime %= min startTime

startSession :: SessionId -> UTCTime -> EventM Name SessionsState ()
startSession sessionId startTime =
  modify' \ sessions ->
    listMoveToEnd (listInsert (length sessions) (sessionId, Session.initialState startTime) sessions)

handleSessionsEvent :: SessionsEvent -> EventM Name SessionsState ()
handleSessionsEvent = \case
  AddWorker sessionId workerId time (GrpcConnection connection) ->
    addWorker sessionId workerId time connection

  RemoveWorker sessionId wid ->
    zoom (sessionLens sessionId) do
      Session.removeWorker wid

  StartSession sessionId startTime ->
    startSession sessionId startTime

  EndSession sessionId -> do
    end <- liftIO getCurrentTime
    sessionLens sessionId . #endTime ?= end

  Session sessionId event ->
    zoom (sessionLens sessionId) (handleSessionEvent event)
