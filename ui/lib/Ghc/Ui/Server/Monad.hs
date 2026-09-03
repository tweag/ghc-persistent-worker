module Ghc.Ui.Server.Monad where

import Brick.BChan (BChan, writeBChan, writeBChanNonBlocking)
import Control.Concurrent (MVar)
import Control.Concurrent.Lifted (modifyMVar, modifyMVar_)
import Control.Monad (join, void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader (..), ReaderT (..))
import Data.Text (Text)
import GHC.Generics (Generic)
import Ghc.Ui.Data.Main (MainEvent (..))
import Ghc.Ui.Data.ServerProcess (ServerProcess)
import System.OsPath (OsPath)

data ServerEnv =
  ServerEnv {
    executable :: Maybe OsPath,
    events :: BChan MainEvent,
    process :: MVar (Maybe ServerProcess)
  }
  deriving stock (Generic)

type ServerM a = ReaderT ServerEnv IO a

lowerServer :: ((forall b . ServerM b -> IO b) -> IO a) -> ServerM a
lowerServer f = do
  env <- ask
  liftIO (f (flip runReaderT env))

sendEvent ::
  MonadIO m =>
  MainEvent ->
  ReaderT ServerEnv m ()
sendEvent event = do
  ServerEnv {events} <- ask
  liftIO $ writeBChan events event

trySendEvent ::
  MonadIO m =>
  MainEvent ->
  ReaderT ServerEnv m ()
trySendEvent event = do
  ServerEnv {events} <- ask
  void $ liftIO $ writeBChanNonBlocking events event

logInfo ::
  MonadIO m =>
  Text ->
  Text ->
  ReaderT ServerEnv m ()
logInfo name message =
  trySendEvent ProcessLog {level = "info", ..}

logError ::
  MonadIO m =>
  Text ->
  Text ->
  ReaderT ServerEnv m ()
logError name message =
  trySendEvent ProcessLog {level = "error", ..}

logOp ::
  MonadIO m =>
  Text ->
  ReaderT ServerEnv m ()
logOp message =
  trySendEvent (OpLogMessage message)

trackProcess ::
  (Maybe ServerProcess -> ServerM ServerProcess) ->
  ServerM ()
trackProcess update = do
  ServerEnv {process} <- ask
  modifyMVar_ process (fmap Just . update)

withProcess ::
  (ServerProcess -> ServerM (Maybe ServerProcess, Maybe a)) ->
  ServerM (Maybe a)
withProcess use = do
  ServerEnv {process} <- ask
  modifyMVar process \case
    Nothing -> pure (Nothing, Nothing)
    Just p -> use p

withProcess_ ::
  (ServerProcess -> ServerM (Maybe ServerProcess)) ->
  ServerM ()
withProcess_ use = do
  ServerEnv {process} <- ask
  modifyMVar_ process (fmap join . traverse use)
