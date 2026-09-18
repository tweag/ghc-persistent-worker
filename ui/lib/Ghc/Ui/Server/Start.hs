module Ghc.Ui.Server.Start where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async.Lifted (async, cancel)
import Control.Exception (SomeException)
import Control.Monad (void)
import Control.Monad.Catch (bracket, catch)
import Control.Monad.Extra (ifM, unlessM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT (..), ask)
import Data.Binary (decode)
import Data.ByteString (fromStrict)
import Data.Foldable (for_, traverse_)
import Data.Functor ((<&>))
import Data.IORef (IORef)
import Data.IORef.Extra (atomicModifyIORef'_)
import Data.IORef.Lifted (newIORef, readIORef)
import qualified Data.Text as Text
import Data.Text (Text)
import Ghc.Ui.Data.Main (MainEvent (..))
import Ghc.Ui.Data.OpLog (OpLevel (..))
import Ghc.Ui.Data.ServerHandlers (ServerHandlers (..))
import Ghc.Ui.Data.ServerProcess (
  ServerConfig (..),
  ServerProcess (..),
  ServerRoot,
  ServerStatus (..),
  canonicalServerRoot,
  describeServerRoot,
  )
import Ghc.Ui.Data.Session (SessionEvent (..), SessionId (..))
import qualified Ghc.Ui.Data.Sessions as Sessions
import Ghc.Ui.Data.Sessions (GrpcConnection (..))
import Ghc.Ui.Data.WorkerId (WorkerId (WorkerId))
import Ghc.Ui.Server.Api (serverApi)
import Ghc.Ui.Server.Monad (
  ServerEnv (..),
  ServerM,
  logError,
  logInfo,
  logOp,
  sendEvent,
  trackProcess,
  trySendEvent,
  withProcess_,
  )
import Ghc.Ui.Server.Process (isServerUp, resolveServerExe, startServerProcess)
import Ghc.Ui.Server.Stop (requestShutdown, stopServer)
import Network.GRPC.Client (Server (ServerUnix), closeConnection, openConnection, rpc)
import Network.GRPC.Client.StreamType.IO (serverStreaming)
import Network.GRPC.Common (NextElem, def)
import Network.GRPC.Common.NextElem (whileNext_)
import Network.GRPC.Common.Protobuf (Proto, Protobuf, defMessage)
import Proto.GhcServer (Encoded, GhcServer)
import System.Directory.OsPath (getModificationTime)
import System.Exit (ExitCode (..))
import System.IO qualified as IO
import System.IO (Handle)
import System.OsPath (OsPath, osp, (</>))
import System.OsPath.Extra (fromOsPath)
import System.Process.Typed (Process, getStderr, getStdout, waitExitCode)

-- | The default gRPC socket path for a given project root.
defaultSocketPath :: OsPath -> OsPath
defaultSocketPath root = root </> [osp|socket/server.sock|]

serverListen ::
  SessionId ->
  WorkerId ->
  (ReaderT ServerEnv IO (NextElem (Proto Encoded))) ->
  ServerM ()
serverListen sessionId workerId recv =
  whileNext_ recv \ message ->
    sendEvent $
    Sessions $
    Sessions.Session sessionId (ApiEvent workerId (decode (fromStrict message.payload)))

serverConnect ::
  Maybe ServerProcess ->
  OsPath ->
  ServerM ()
serverConnect process socket =
 spin (5 :: Int)
 where
    spin = \case
      0 -> sendEvent $ Sessions $ Sessions.RemoveWorker sessionId workerId
      n ->
        bracket (liftIO (openConnection def server)) (liftIO . closeConnection) \ connection ->
          catch (tryConnect connection) (retry n)

    tryConnect connection =
      serverStreaming connection (rpc @(Protobuf GhcServer "events")) defMessage \ recv -> do
        traverse_ updateProcess process
        time <- liftIO $ getModificationTime socket
        sendEvent $ Sessions $ Sessions.AddWorker sessionId workerId time (GrpcConnection connection)
        serverListen sessionId workerId recv

    updateProcess new =
      trackProcess $ pure . \case
        Nothing -> new
        Just ServerProcess {status = ServerInactive} -> new
        Just ServerProcess {status = ServerStarting} -> new
        Just p -> p

    -- TODO why SomeException?
    retry n (_ :: SomeException) = do
      liftIO $ threadDelay 100_000
      spin (n - 1)

    server = ServerUnix (fromOsPath socket)

    -- TODO: This is a hack, ids should be sent over grpc
    (sessionId', workerId') = break (== '_') (fromOsPath socket)

    sessionId = SessionId $ Text.pack sessionId'

    workerId = WorkerId $ Text.pack workerId'

listenSubprocess :: OsPath -> ServerM ()
listenSubprocess socket = do
  liftIO (isServerUp socket) >>= \case
    True -> serverConnect Nothing socket
    False -> do
      liftIO (threadDelay 100_000)
      listenSubprocess socket

streamLines :: Text -> Handle -> Maybe (IORef [Text]) -> ServerM ()
streamLines streamName h macc =
  spin
  where
    spin =
      unlessM (liftIO (IO.hIsEOF h)) do
        line <- Text.pack <$> liftIO (IO.hGetLine h)
        for_ macc \ acc ->
          liftIO $ atomicModifyIORef'_ acc (line :)
        logOp OpDebug ("server " <> streamName <> ": " <> line)
        spin

watchProcess ::
  ServerRoot ->
  Process () Handle Handle ->
  IORef [Text] ->
  ServerProcess ->
  ServerM ()
watchProcess root processHandle stderrLines result = do
  exitCode <- liftIO $ waitExitCode processHandle
  withProcess_ \case
    ServerProcess {status = ServerStarted {listener}} -> do
      cancel listener
      captured <- liftIO $ reverse <$> readIORef stderrLines
      trySendEvent $ ServerStopped {failedPath = checkFailure exitCode, stderr = Text.unlines captured}
      pure (Just result)
    _ ->
      pure (Just result)
  where
    checkFailure = \case
      ExitSuccess -> Nothing
      ExitFailure _ -> Just root

attachSubprocess ::
  ServerConfig ->
  OsPath ->
  Process () Handle Handle ->
  ServerM ServerProcess
attachSubprocess config socket process = do
  listener <- async (listenSubprocess socket)
  stderrLines <- newIORef []
  stdoutReader <- async $ streamLines "stdout" (getStdout process) Nothing
  stderrReader <- async $ streamLines "stderr" (getStderr process) (Just stderrLines)
  void $ async $ watchProcess config.root process stderrLines (withStatus ServerInactive)
  pure (withStatus ServerStarted {..})
  where
    withStatus status = ServerProcess {config, status}

ensureServerOnSocket :: OsPath -> OsPath -> ServerConfig -> ServerM ServerProcess
ensureServerOnSocket path socket config@ServerConfig {root, options} =
  ifM (isServerUp socket) connectExisting startNew
  where
    connectExisting = do
      logInfo ("Connecting to preexisting ghc-server process in " <> desc)
      void $ async $ serverConnect (Just (withStatus ServerConnected)) socket
      pure (withStatus ServerStarting)

    startNew = do
      env <- ask
      liftIO (resolveServerExe env.executable) >>= \case
        Left err -> startFailed err
        Right exe -> do
          logInfo ("Starting ghc-server in " <> desc)
          handles <- liftIO $ startServerProcess exe path options
          attachSubprocess config socket handles

    startFailed err = do
      trySendEvent $ ServerStopped {failedPath = Just root, stderr = err}
      pure (withStatus ServerInactive)

    withStatus status =
      ServerProcess {config = ServerConfig {root, options}, status}

    desc = describeServerRoot root

ensureServer :: ServerConfig -> ServerM ServerProcess
ensureServer config = do
  path <- liftIO $ canonicalServerRoot config.root
  ensureServerOnSocket path (defaultSocketPath path) config

startServer :: ServerConfig -> ServerM ()
startServer config =
  trackProcess \case
    Just p@ServerProcess {status = ServerStarted {}} ->
      p <$ alreadyStarted
    Just p@ServerProcess {status = ServerStarting} ->
      p <$ alreadyStarted
    _ ->
      ensureServer config
  where
    alreadyStarted = logError "Multiple servers started simultaneously, aborting"

-- | Reuse the path and options used when starting the server initially.
restartServer :: ServerM ()
restartServer = do
  logInfo "Attempting to restart the server"
  stopServer >>= \case
    Just config ->
      startServer config
    Nothing ->
      logError "No ghc-server configuration from previous process available for restart"

serverHandlers :: ServerM ServerHandlers
serverHandlers =
  ask <&> \ env ->
    let run = flip runReaderT env
    in ServerHandlers {
      start = run . startServer,
      stop = run (void stopServer),
      restart = run restartServer,
      shutdown = run requestShutdown,
      api = serverApi env
    }
