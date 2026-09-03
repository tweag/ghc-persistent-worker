module Ghc.Ui.Server.Stop where

import Control.Concurrent.Async.Lifted (async, cancel)
import Control.Monad (void)
import Control.Monad.Catch (finally)
import Ghc.Ui.Data.Main (MainEvent (..))
import Ghc.Ui.Data.ServerProcess (ServerConfig (..), ServerProcess (..), ServerStatus (..))
import Ghc.Ui.Server.Monad (ServerM, logError, logOp, trySendEvent, withProcess)
import Ghc.Ui.Server.Process (killGhcServer)

-- | The order of cancellations is critical here, otherwise the stdio readers will block 'stopProcess' and the listener
-- will be waiting for a request on an intact connection, apparently in a blocking operation.
--
-- The 'ServerConnected' case must return 'Nothing' to signal that the server cannot be restarted.
stopServer :: ServerM (Maybe ServerConfig)
stopServer =
  withProcess \case
    ServerProcess {config, status = ServerStarted {process, listener, stdoutReader, stderrReader}} -> do
      logOp "Killing ghc-server process"
      cancel stdoutReader
      cancel stderrReader
      killGhcServer process
      cancel listener
      trySendEvent ServerStopped {failedPath = Nothing, stderr = "Server killed successfully"}
      pure (Just ServerProcess {config, status = ServerInactive}, Just config)
    old@ServerProcess {status = ServerConnected} -> do
      logError "kill" "Cannot stop a ghc-server process that wasn't started by this session"
      pure (Just old, Nothing)
    process ->
      pure (Just process, Nothing)

requestShutdown :: ServerM ()
requestShutdown =
  void $ async do
    finally stopServer do
      trySendEvent ShutdownComplete
