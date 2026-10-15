-- | The instrumentation helpers that moved to "GhcWorker.LogEvents" are re-exported for the server's modules.
module GhcServer.Log (module GhcServer.Log, emitEvent, emitLog, instrumentLogger, renderGhcLogEvent) where

import Control.Monad (when)
import Data.Foldable (traverse_)
import Data.IORef (newIORef)
import GhcWorker.LogEvents (emitEvent, emitLog, instrumentLogger, renderGhcLogEvent)
import GHC.Utils.Outputable (showPprUnsafe)
import Prelude hiding (log)
import System.IO (hPutStrLn)
import System.IO qualified as IO (stderr)
import Test.Data.TestLog (emptyTestLog)
import Test.Log (testLogger)
import Types.Log (Logger (..))

-- | Create a logger.
--
-- When @verbose@ is 'True', debug and info messages are printed to stderr.
-- Diagnostics and errors are stored in 'Test.Data.TestLog.TestLog'.
newLogger :: Bool -> IO Logger
newLogger verbose = do
  logVar <- newIORef emptyTestLog
  let
    base = testLogger logVar

    debug message = do
      base.debug message
      when verbose do
        hPutStrLn IO.stderr message
  pure base {
    debug,
    debugD = debug . showPprUnsafe,
    info = debug,
    infoD = debug . showPprUnsafe,
    fatal = \ message -> base.fatal message *> debug (showPprUnsafe message),
    ghcAction = \ flags msg_class srcSpan msg -> do
      base.ghcAction flags msg_class srcSpan msg
      when verbose do
        traverse_ (debug . snd) =<< renderGhcLogEvent flags msg_class srcSpan msg
  }

-- | Create a task logger that captures GHC messages and pass it to the given action.
--
-- Uses non-verbose mode since task loggers are internal; the main build logger handles user-visible output.
withBuildLog :: (Logger -> IO a) -> IO a
withBuildLog action =
  action =<< newLogger False
