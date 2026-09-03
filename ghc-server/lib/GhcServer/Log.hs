module GhcServer.Log where

import Control.Concurrent.Chan (Chan, writeChan)
import Control.Monad (when)
import Data.Foldable (traverse_)
import Data.IORef (newIORef)
import Data.Time (nominalDiffTimeToSeconds)
import Data.Time.Clock.POSIX (getPOSIXTime)
import GHC (Severity (..), SrcSpan)
import GHC.Types.Error (MessageClass (..))
import GHC.Utils.Logger (LogFlags)
import GHC.Utils.Outputable (SDoc, showPprUnsafe)
import Internal.Log (decorateDiagnostic, renderLogMessage)
import Prelude hiding (log)
import System.IO (hPutStrLn)
import System.IO qualified as IO (stderr)
import Test.Data.TestLog (emptyTestLog)
import Test.Log (testLogger)
import Types.Api (Event (..))
import Types.Log (Logger (..))

-- | Render a GHC log-hook message the same way 'Internal.Log.logGhcAction' does, returning the level\/text pair
-- to forward to the instrument channel, or 'Nothing' for messages that should be dropped (e.g. ignored
-- diagnostics).
renderGhcLogEvent :: LogFlags -> MessageClass -> SrcSpan -> SDoc -> IO (Maybe (String, String))
renderGhcLogEvent flags msg_class srcSpan msg =
  case msg_class of
    MCOutput -> info
    MCDump -> info
    MCInteractive -> info
    MCInfo -> info
    MCFatal -> atLevel "fatal"
    MCDiagnostic SevIgnore _ _ -> pure Nothing
    MCDiagnostic _ _ _ -> do
      decorated <- decorateDiagnostic flags msg_class srcSpan msg
      pure (Just ("diagnostic", renderLogMessage flags decorated))
  where
    info = atLevel "info"

    atLevel level = pure (Just (level, rendered))

    rendered = renderLogMessage flags msg

-- | Create a logger.
--
-- When @verbose@ is 'True', debug and info messages are printed to stderr.
-- Diagnostics and errors are stored in 'Test.Data.TestLog.TestLog'.
newLogger :: Bool -> IO Logger
newLogger verbose = do
  logVar <- newIORef emptyTestLog
  let base = testLogger logVar
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
  where
    debug message =
      when verbose do
        hPutStrLn IO.stderr message

-- | Create a task logger that captures GHC messages and pass it to the given action.
--
-- Uses non-verbose mode since task loggers are internal; the main build logger handles user-visible output.
withBuildLog :: (Logger -> IO a) -> IO a
withBuildLog action =
  action =<< newLogger False

-- | Push a 'LogMessage' event to the instrument channel, if instrumentation is enabled. No-op otherwise.
emitLog :: Maybe (Chan Event) -> String -> String -> String -> IO ()
emitLog Nothing _ _ _ = pure ()
emitLog (Just chan) category level message = do
  time <- nominalDiffTimeToSeconds <$> getPOSIXTime
  writeChan chan LogMessage {category, level, message, time}

-- | Push an arbitrary instrumentation event to the given channel, if instrumentation is enabled. No-op otherwise.
-- Shared by all build steps that report events beyond plain log messages (e.g. 'Types.Api.PhaseEvent'),
-- so it lives below both 'GhcServer.Build.Propagate' (whose own 'GhcServer.Build.Propagate.emitEvent' takes a
-- 'GhcServer.Data.BuildEnv.BuildEnv' for convenience) and its callees, avoiding an import cycle.
emitEvent :: Maybe (Chan Event) -> Event -> IO ()
emitEvent Nothing _ = pure ()
emitEvent (Just chan) evt = writeChan chan evt

-- | Wrap a 'Logger' so that every message it processes (via 'debug'\/'debugD'\/'info'\/'infoD'\/'fatal', and
-- GHC's own log hook 'ghcAction') is also pushed to the instrument event channel as a 'Types.Api.LogMessage',
-- tagged with the given target text (e.g. @unitName:metadata@ or @unitName:moduleName@) and the current time.
--
-- A no-op passthrough (returns the input 'Logger' unchanged) when instrumentation is disabled ('Nothing' channel).
instrumentLogger :: Maybe (Chan Event) -> String -> Logger -> Logger
instrumentLogger Nothing _ logger = logger
instrumentLogger chan@(Just _) target logger =
  logger
    { debug = \ msg -> logger.debug msg *> emitLog chan target "debug" msg
    , debugD = \ doc -> logger.debugD doc *> emitLog chan target "debug" (showPprUnsafe doc)
    , info = \ msg -> logger.info msg *> emitLog chan target "info" msg
    , infoD = \ doc -> logger.infoD doc *> emitLog chan target "info" (showPprUnsafe doc)
    , fatal = \ doc -> logger.fatal doc *> emitLog chan target "fatal" (showPprUnsafe doc)
    , ghcAction = \ flags msg_class srcSpan msg -> do
        logger.ghcAction flags msg_class srcSpan msg
        renderGhcLogEvent flags msg_class srcSpan msg >>= \case
          Nothing -> pure ()
          Just (level, rendered) -> emitLog chan target level rendered
    }
