-- | Forwarding of log messages to the instrumentation event stream, shared by @ghc-server@ and the Buck worker.
module GhcWorker.LogEvents where

import Control.Concurrent.Chan (Chan, writeChan)
import Data.Foldable (traverse_)
import Data.Time (nominalDiffTimeToSeconds)
import Data.Time.Clock.POSIX (getPOSIXTime)
import GHC (Severity (..), SrcSpan)
import GHC.Types.Error (MessageClass (..))
import GHC.Utils.Logger (LogFlags)
import GHC.Utils.Outputable (SDoc, showPprUnsafe)
import Internal.Log (decorateDiagnostic, renderLogMessage)
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

-- | Publish a 'LogMessage' with the current time.
emitLogWith :: (Event -> IO ()) -> String -> String -> String -> IO ()
emitLogWith publish category level message = do
  time <- nominalDiffTimeToSeconds <$> getPOSIXTime
  publish LogMessage {category, level, message, time}

-- | Push a 'LogMessage' event to the instrument channel, if instrumentation is enabled. No-op otherwise.
emitLog :: Maybe (Chan Event) -> String -> String -> String -> IO ()
emitLog chan category level message =
  traverse_ (\ c -> emitLogWith (writeChan c) category level message) chan

-- | Push an arbitrary instrumentation event to the given channel, if instrumentation is enabled. No-op otherwise.
emitEvent :: Maybe (Chan Event) -> Event -> IO ()
emitEvent chan evt =
  traverse_ (\ c -> writeChan c evt) chan

-- | Wrap a 'Logger' so that every message it processes (via 'debug'\/'debugD'\/'info'\/'infoD'\/'fatal', and
-- GHC's own log hook 'ghcAction') is also published as a 'Types.Api.LogMessage', tagged with the category obtained
-- from the second argument (e.g. @unitName:metadata@ or @unitName:moduleName@) at the time of the message.
instrumentLoggerWith :: (Event -> IO ()) -> IO String -> Logger -> Logger
instrumentLoggerWith publish category logger =
  logger
    { debug = \ msg -> logger.debug msg *> emit "debug" msg
    , debugD = \ doc -> logger.debugD doc *> emit "debug" (showPprUnsafe doc)
    , info = \ msg -> logger.info msg *> emit "info" msg
    , infoD = \ doc -> logger.infoD doc *> emit "info" (showPprUnsafe doc)
    , fatal = \ doc -> logger.fatal doc *> emit "fatal" (showPprUnsafe doc)
    , ghcAction = \ flags msg_class srcSpan msg -> do
        logger.ghcAction flags msg_class srcSpan msg
        renderGhcLogEvent flags msg_class srcSpan msg >>= \case
          Nothing -> pure ()
          Just (level, rendered) -> emit level rendered
    }
  where
    emit level message = do
      name <- category
      emitLogWith publish name level message

-- | Like 'instrumentLoggerWith' with a fixed category, publishing to the channel.
--
-- A no-op passthrough (returns the input 'Logger' unchanged) when instrumentation is disabled ('Nothing' channel).
instrumentLogger :: Maybe (Chan Event) -> String -> Logger -> Logger
instrumentLogger Nothing _ logger = logger
instrumentLogger (Just chan) target logger =
  instrumentLoggerWith (writeChan chan) (pure target) logger
