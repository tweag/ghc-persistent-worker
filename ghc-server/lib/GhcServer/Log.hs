-- | Build log capture for the standalone GHC server.
module GhcServer.Log where

import Control.Concurrent.Chan (Chan, writeChan)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Time.Clock.POSIX (getPOSIXTime)
import GHC (Severity (..), SrcSpan)
import GHC.Types.Error (MessageClass (..))
import GHC.Utils.Logger (LogAction, LogFlags)
import GHC.Utils.Outputable (SDoc, showPprUnsafe)
import Internal.Log (decorateDiagnostic, renderLogMessage)
import Prelude hiding (log)
import System.IO (hPutStrLn)
import System.IO qualified as IO (stderr)
import Types.Instrument (Event (..))
import Types.Log (Logger (..))

-- | Captured log output for diagnostics.
data BuildLog =
  BuildLog {
    diagnostics :: [String],
    errors :: [String]
  }

emptyBuildLog :: BuildLog
emptyBuildLog = BuildLog [] []

modifyLog :: IORef BuildLog -> (BuildLog -> BuildLog) -> IO ()
modifyLog ref f = atomicModifyIORef' ref \ l -> (f l, ())

-- | Flush the build log, returning captured diagnostics and errors.
flushBuildLog :: IORef BuildLog -> IO [String]
flushBuildLog ref = do
  l <- readIORef ref
  pure (l.diagnostics ++ l.errors)

buildGhcAction :: IORef BuildLog -> LogAction
buildGhcAction logRef flags msg_class srcSpan msg = case msg_class of
  MCOutput -> pure ()
  MCDump -> pure ()
  MCInteractive -> pure ()
  MCInfo -> pure ()
  MCFatal ->
    modifyLog logRef \ l -> l {errors = showPprUnsafe msg : l.errors}
  MCDiagnostic SevIgnore _ _ -> pure ()
  MCDiagnostic _sev _rea _code -> do
    rendered <- renderLogMessage flags <$> decorateDiagnostic flags msg_class srcSpan msg
    modifyLog logRef \ l -> l {diagnostics = rendered : l.diagnostics}

-- | Create a logger.
--
-- When @verbose@ is 'True', debug and info messages are printed to stderr
-- synchronously.  Diagnostics and errors are always captured for 'flush'.
newLogger :: Bool -> IO Logger
newLogger verbose = do
  logRef <- newIORef emptyBuildLog
  pure Logger {
    setTarget = \ _ -> pure (),
    debug,
    debugD = debug . showPprUnsafe,
    info = debug,
    infoD = debug . showPprUnsafe,
    fatal = \ message ->
      modifyLog logRef \ l -> l {errors = showPprUnsafe message : l.errors},
    ghcAction = buildGhcAction logRef,
    flush = flushBuildLog logRef
  }
  where
    debug message
      | verbose = hPutStrLn IO.stderr message
      | otherwise = pure ()

-- | Create a fresh per-task logger that captures output, running the given action.
--
-- Uses non-verbose mode since per-task loggers are internal; the main build
-- logger handles user-visible output.
withBuildLog :: (Logger -> IO a) -> IO a
withBuildLog action =
  action =<< newLogger False

-- | Current time as a millisecond epoch timestamp, for 'Types.Instrument.LogMessage'.
currentTimeMs :: IO Integer
currentTimeMs =
  round . (* 1000) <$> getPOSIXTime

-- | Push a 'LogMessage' event to the instrument channel, if instrumentation is enabled. No-op otherwise.
emitLog :: Maybe (Chan Event) -> String -> String -> String -> IO ()
emitLog Nothing _ _ _ = pure ()
emitLog (Just chan) target level message = do
  timestampMs <- currentTimeMs
  writeChan chan LogMessage {target, level, message, timestampMs}

-- | Render a GHC log-hook message the same way 'Internal.Log.logGhcAction' does, returning the level\/text pair
-- to forward to the instrument channel, or 'Nothing' for messages that should be dropped (e.g. ignored
-- diagnostics).
renderGhcLogEvent :: LogFlags -> MessageClass -> SrcSpan -> SDoc -> IO (Maybe (String, String))
renderGhcLogEvent _ MCOutput _ msg = pure (Just ("info", showPprUnsafe msg))
renderGhcLogEvent flags MCDump _ msg = pure (Just ("info", renderLogMessage flags msg))
renderGhcLogEvent _ MCInteractive _ msg = pure (Just ("info", showPprUnsafe msg))
renderGhcLogEvent _ MCInfo _ msg = pure (Just ("info", showPprUnsafe msg))
renderGhcLogEvent _ MCFatal _ msg = pure (Just ("fatal", showPprUnsafe msg))
renderGhcLogEvent _ (MCDiagnostic SevIgnore _ _) _ _ = pure Nothing
renderGhcLogEvent flags msg_class@(MCDiagnostic _ _ _) srcSpan msg = do
  rendered <- decorateDiagnostic flags msg_class srcSpan msg
  pure (Just ("diagnostic", renderLogMessage flags rendered))

-- | Wrap a 'Logger' so that every message it processes (via 'debug'\/'debugD'\/'info'\/'infoD'\/'fatal', and
-- GHC's own log hook 'ghcAction') is also pushed to the instrument event channel as a 'Types.Instrument.LogMessage',
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
