module Ghc.Ui.Run where

import Brick (customMainWithDefaultVty)
import Brick.BChan (newBChan)
import Control.Applicative ((<|>))
import Control.Concurrent (newMVar, threadDelay)
import Control.Concurrent.Async.Lifted (async)
import Control.Exception (IOException, bracket, displayException, try)
import Control.Monad (filterM, forever, guard, void)
import Control.Monad.Catch (finally)
import Control.Monad.Extra (whenM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask, runReaderT)
import Data.Foldable (traverse_)
import Data.Text qualified as Text
import Data.Time (getCurrentTime)
import Ghc.Ui.App (app)
import Ghc.Ui.Cli (Options (..), optionsInfo)
import qualified Ghc.Ui.Data.Main as Main
import Ghc.Ui.Data.Main (MainEvent (..))
import Ghc.Ui.Server.Monad (ServerEnv (..), ServerM, logError, logInfo, lowerServer, trySendEvent)
import Ghc.Ui.Server.Process (isServerUp)
import Ghc.Ui.Server.Start (defaultSocketPath, serverConnect, serverHandlers)
import Ghc.Ui.Server.Stop (stopServer)
import Graphics.Vty (Vty (shutdown))
import Internal.Watch (watchAddedDirs, workerDirectory)
import Options.Applicative (execParser)
import System.Directory.OsPath (
  createDirectoryIfMissing,
  doesDirectoryExist,
  doesPathExist,
  getCurrentDirectory,
  listDirectory,
  )
import System.Environment (lookupEnv)
import System.FSNotify (WatchManager, withManager)
import System.OsPath (OsPath, osp, (</>))
import System.OsPath.Extra (toOsPath)
import System.OsString (isInfixOf)
import Types.Orchestration (primarySocketName)

newtype WorkerPath =
  WorkerPath { path :: OsPath }
  deriving stock (Eq, Show)

envWorkerPath :: IO WorkerPath
envWorkerPath =
  WorkerPath <$> workerDirectory

-- | A socket given via @INSTRUMENT_SOCKET@, or else the default socket in the current directory if a server is
-- listening on it.
resolveSocket :: IO (Maybe OsPath)
resolveSocket = do
  cwd <- getCurrentDirectory
  fromEnv <- fmap toOsPath <$> lookupEnv "INSTRUMENT_SOCKET"
  defaultUp <- isServerUp (defaultSocketPath cwd)
  pure (fromEnv <|> (defaultSocketPath cwd <$ guard defaultUp))

runClock :: ServerM ()
runClock =
  void $ async $ forever do
    time <- liftIO getCurrentTime
    trySendEvent (SetTime time)
    liftIO $ threadDelay 100_000

connectAsync :: OsPath -> ServerM ()
connectAsync socket =
  void $ async $ serverConnect Nothing socket

-- | Find already running workers if the socket directory exists, or create the directory otherwise. Reports
-- a failure creating the directory (e.g. permission denied) to the op log instead of discarding it, since a
-- missing worker directory otherwise manifests only as "no sessions ever show up", with no visible reason.
discover :: WorkerPath -> ServerM ()
discover workers = do
  whenM (liftIO (doesDirectoryExist workers.path)) do
    dirs <- liftIO $ listDirectory workers.path
    sockets <- liftIO $ filterM doesPathExist [workers.path </> dir </> primarySocketName | dir <- dirs]
    traverse_ (serverConnect Nothing) sockets
  created <- liftIO $ try @IOException (createDirectoryIfMissing True workers.path)
  either createDirFailed pure created
  where
    createDirFailed err =
      logError ("Failed to create worker directory: " <> Text.pack (displayException err))

-- | Start an inotify watcher to detect newly started workers.
runWorkerWatcher :: WorkerPath -> WatchManager -> ServerM (IO ())
runWorkerWatcher workers manager = do
  lowerServer \ lower ->
    watchAddedDirs manager workers.path \ dir ->
      if not (isInfixOf [osp|log|] dir)
      then lower do
        logInfo "Detected new worker"
        connectAsync (dir </> primarySocketName)
      else pure ()

withWorkerWatcher ::
  WorkerPath ->
  ServerM a ->
  ServerM a
withWorkerWatcher workers ma = do
  lowerServer \ lower ->
    withManager \ manager ->
      bracket (lower (runWorkerWatcher workers manager)) id (const (lower ma))

withoutSpecified :: WorkerPath -> ServerM a -> ServerM a
withoutSpecified workers ma = do
  discover workers
  withWorkerWatcher workers ma

withSpecified :: OsPath -> ServerM a -> ServerM a
withSpecified socket ma = do
  connectAsync socket
  ma

runBrick :: ServerM ()
runBrick = do
  server <- serverHandlers
  (_, vty) <- flip finally stopServer do
    ServerEnv {events} <- ask
    liftIO $ customMainWithDefaultVty (Just events) (app server) Main.initialState
  liftIO vty.shutdown

main :: IO ()
main = do
  workers <- envWorkerPath
  specifiedSocket <- resolveSocket
  events <- newBChan 1000
  process <- newMVar Nothing
  options <- execParser optionsInfo
  flip runReaderT ServerEnv {executable = options.serverExe, events, process} do
    runClock
    maybe (withoutSpecified workers) withSpecified specifiedSocket runBrick
