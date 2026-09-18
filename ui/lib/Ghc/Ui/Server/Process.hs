module Ghc.Ui.Server.Process where

import BuckWorkerProto ()
import Control.Concurrent.Async.Lifted (race_)
import Control.Concurrent.Lifted (threadDelay)
import Control.Exception (SomeException, bracket, catch)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Either.Extra (maybeToEither)
import Data.Foldable (traverse_)
import Data.Text qualified as Text
import Data.Text (Text)
import Ghc.Ui.Server.Monad (ServerM, logInfo)
import Network.Socket (Family (AF_UNIX), SockAddr (SockAddrUnix), SocketType (Stream), close, connect, socket)
import System.Directory (findExecutable)
import System.IO (Handle)
import System.OsPath (OsPath)
import System.OsPath.Extra (fromOsPath, toOsPath)
import System.Posix (sigKILL, signalProcess)
import System.Process.Typed (
  Process,
  createPipe,
  getPid,
  proc,
  setCreateGroup,
  setStderr,
  setStdout,
  startProcess,
  stopProcess,
  )

-- | Check whether something is listening on the given Unix socket, by attempting an actual @connect(2)@.
--
-- __Note__: this cannot use a lazy gRPC client (e.g. 'Network.GRPC.Client.withConnection') as a substitute, because
-- grapesy's connections are established asynchronously on first use, not by 'openConnection' itself — so a
-- "successful" 'withConnection' says nothing about whether a listener actually exists at the given path.
--
-- TODO Verify that we can't just rely on @serverStreaming@ succeeding instead.
--
-- TODO SomeException
isServerUp ::
  MonadIO m =>
  OsPath ->
  m Bool
isServerUp sock =
  liftIO $ catch tryConnect \ (_ :: SomeException) -> pure False
  where
    tryConnect =
      bracket (socket AF_UNIX Stream 0) close \ s ->
        True <$ connect s (SockAddrUnix (fromOsPath sock))

resolveServerExe :: Maybe OsPath -> IO (Either Text OsPath)
resolveServerExe = \case
  Just exe -> pure (Right exe)
  Nothing -> do
    result <- findExecutable "ghc-server"
    pure (toOsPath <$> maybeToEither notFound result)
  where
    notFound = "ghc-server executable not found. Pass --server-exe or ensure it's on PATH"

startServerProcess :: OsPath -> OsPath -> [Text] -> IO (Process () Handle Handle)
startServerProcess exe root extraArgs =
  startProcess $
  setStdout createPipe $
  setStderr createPipe $
  setCreateGroup True $
  proc (fromOsPath exe) (fromOsPath root : "--enable" : "instrument" : fmap Text.unpack extraArgs)

-- | 'stopProcess' sends TERM, and we send KILL if that doesn't manage to terminate the process within five seconds.
killGhcServer ::
  Process () Handle Handle ->
  ServerM ()
killGhcServer process = do
  race_ wait (stopProcess process)
  where
    wait = do
      threadDelay 200_000
      logInfo "Server is busy, waiting for 5 seconds..."
      threadDelay 5_000_000
      logInfo "Sending signal KILL to the server process"
      liftIO $ traverse_ (signalProcess sigKILL) =<< getPid process
