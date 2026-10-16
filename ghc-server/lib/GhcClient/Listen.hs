{-# LANGUAGE ApplicativeDo #-}

-- | The client's @listen@ command: stream instrumentation events from a running server and print them to stdout.
module GhcClient.Listen where

import BuckWorkerProto ()
import BuckWorkerProto.Protocol (streamEvents)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (newMVar, withMVar)
import Control.Exception (SomeAsyncException, SomeException, catch, displayException, fromException, throwIO)
import Control.Monad (void)
import Data.Maybe (isJust)
import Data.Binary (decode)
import Data.ByteString (fromStrict)
import Data.List (intercalate)
import Data.Text qualified as Text
import GhcServer.Optparse (readPath)
import GhcServer.Path (socketPath)
import Internal.Watch (waitForPath, watchWorkerSockets, workerDirectory)
import Network.GRPC.Client (Server (..), withConnection)
import Network.GRPC.Common (def)
import Network.GRPC.Common.NextElem (whileNext_)
import Options.Applicative (
  Parser,
  help,
  long,
  metavar,
  option,
  optional,
  )
import System.Environment (lookupEnv)
import System.IO qualified as IO
import System.OsPath (OsPath)
import System.OsPath.Extra (fromOsPath, takeDirectory, takeFileName, toOsPath)
import Types.Api (
  Event (..),
  ExecutorId (..),
  ExecutorStatus (..),
  ProcessStats (..),
  RequestId (..),
  TrackedBytecode,
  UnitName (..),
  UnitSummary (..),
  renderTarget,
  )

data ListenConfig =
  ListenConfig {
    projectRoot :: Maybe OsPath,
    socket :: Maybe OsPath
  }
  deriving stock (Eq, Show)

listenParser :: Parser ListenConfig
listenParser = do
  projectRoot <- optional (option readPath (long "root" <> metavar "PROJECT_ROOT" <> help "Project directory whose ghc-server socket is used"))
  socket <- optional (option readPath (long "socket" <> metavar "SOCKET" <> help "Path to the server's Unix socket; by default, workers are discovered in the worker directory"))
  pure ListenConfig {..}

-- | Where events come from: one known socket, or every worker that appears in the worker directory.
data Source =
  SingleSocket OsPath
  |
  WorkerDirectory OsPath
  deriving stock (Eq, Show)

-- | Without an explicit socket or project root, workers are discovered like the UI does: via @INSTRUMENT_SOCKET@ or
-- the worker directory (@WORKER_PATH@, default @/tmp/ghc-persistent-worker@).
resolveSource :: ListenConfig -> IO Source
resolveSource config =
  case (config.socket, config.projectRoot) of
    (Just path, _) -> pure (SingleSocket path)
    (Nothing, Just root) -> pure (SingleSocket (socketPath root))
    (Nothing, Nothing) ->
      lookupEnv "INSTRUMENT_SOCKET" >>= \case
        Just path -> pure (SingleSocket (toOsPath path))
        Nothing -> WorkerDirectory <$> workerDirectory

data Printer =
  Printer { emit :: Maybe String -> Event -> IO () }

-- | Print events from concurrent listeners without interleaving them, prefixing each line with the label if any.
newPrinter :: IO Printer
newPrinter = do
  lock <- newMVar ()
  pure Printer {emit = \ label event -> withMVar lock \ () -> putStr (render label event)}
  where
    render label event =
      unlines (fmap (maybe id (\ l -> (('[' : l ++ "] ") ++)) label) (lines (prettyEvent event)))

attempts :: Int
attempts = 30

retryDelayMicros :: Int
retryDelayMicros = 100_000

-- | Run the action, retrying after a delay when it throws a synchronous exception, until the attempts run out, in
-- which case the last exception is rethrown.
retrying :: Int -> IO a -> IO a
retrying n action =
  action `catch` \ (e :: SomeException) ->
    if n <= 1 || isJust (fromException @SomeAsyncException e)
    then throwIO e
    else do
      threadDelay retryDelayMicros
      retrying (n - 1) action

-- | Stream the events of a single socket to the printer until the stream ends.
streamSocket :: Printer -> Maybe String -> OsPath -> IO ()
streamSocket printer label socket =
  withConnection def (ServerUnix (fromOsPath socket)) \ connection ->
    streamEvents connection \ recv ->
      whileNext_ recv \ payload -> printer.emit label (decode (fromStrict payload))

-- | Wait indefinitely for the socket to appear, then stream its events, retrying when connecting fails.
listenOn :: Printer -> Maybe String -> OsPath -> IO ()
listenOn printer label socket = do
  info ("Waiting for socket at " ++ fromOsPath socket)
  waitForPath socket
  info ("Listening for events at " ++ fromOsPath socket)
  retrying attempts (streamSocket printer label socket)
  info ("Event stream ended at " ++ fromOsPath socket)

info :: String -> IO ()
info = IO.hPutStrLn IO.stderr

-- | Failures of individual workers must not stop the listener from picking up others.
reportFailure :: OsPath -> IO () -> IO ()
reportFailure socket action =
  action `catch` \ (e :: SomeException) ->
    info ("Failed to listen at " ++ fromOsPath socket ++ ": " ++ displayException e)

-- | Print events from the selected source; for a worker directory, never returns.
listen :: ListenConfig -> IO ()
listen config = do
  printer <- newPrinter
  resolveSource config >>= \case
    SingleSocket socket ->
      listenOn printer Nothing socket
    WorkerDirectory dir -> do
      info ("Watching for workers in " ++ fromOsPath dir)
      watchWorkerSockets dir \ socket ->
        void $ forkIO $ reportFailure socket $
          listenOn printer (Just (fromOsPath (takeFileName (takeDirectory socket)))) socket

requestTag :: RequestId -> String
requestTag RequestId {number} = "#" ++ show number

indent :: String -> String
indent = intercalate "\n" . fmap ("    " ++) . lines

-- | Render an event as a single header line, followed by indented detail lines where there are any.
prettyEvent :: Event -> String
prettyEvent = \case
  CompileStart {target, debuggable, process, requestId} ->
    unwords (
      ["start", requestTag requestId, Text.unpack (renderTarget target)]
      ++ ["(debuggable)" | debuggable]
      ++ ["(subprocess)" | process]
    )
  CompileEnd {target, exitCode, stderr = errors, result, processStats, noMain, requestId} ->
    withDetails
      (unwords (
        [status, requestTag requestId, Text.unpack (renderTarget target)]
        ++ ["(no main)" | noMain]
      ))
      (
        foldMap (\ r -> ["result: " ++ r]) result
        ++ foldMap (pure . prettyProcessStats) processStats
        ++ [errors | not (null errors)]
      )
    where
      status = if exitCode == 0 then "done " else "fail "
  Stats {memory, cpuNs, gcCpuNs} ->
    "stats: " ++ show (sum memory) ++ " bytes, cpu " ++ millis cpuNs ++ ", gc " ++ millis gcCpuNs
  PhaseStart {target, phase, requestId} ->
    unwords ["phase", requestTag requestId, Text.unpack (renderTarget target), phase]
  PhaseEnd {target, durationMs, requestId} ->
    unwords ["phase", requestTag requestId, Text.unpack (renderTarget target), "finished in", show durationMs ++ "ms"]
  ProjectStructure {units} ->
    withDetails
      ("project: " ++ show (length units) ++ " units")
      [Text.unpack unit.name.text ++ " (" ++ show (length unit.modules) ++ " modules)" | unit <- units]
  BytecodeSnapshot {entries} ->
    "bytecode: " ++ prettyBytecode entries
  LogMessage {category, level, message, time} ->
    withDetails ("[" ++ level ++ "] " ++ category ++ " @" ++ show time) (lines message)
  RequestCompleted {statusMessage} ->
    "completed: " ++ Text.unpack statusMessage
  ExecutorUpdate {executor, pid, status = ExecutorStatus {memInUseBytes, liveBytes, activeTasks}} ->
    unwords [
      "executor", Text.unpack executor.text,
      maybe "" (\ p -> "[" ++ show p ++ "]") pid,
      "mem", show memInUseBytes, "live", show liveBytes, "active", show activeTasks
    ]
  ExecutorGone {executor} ->
    "executor " ++ Text.unpack executor.text ++ " gone"
  where
    withDetails header details = intercalate "\n" (header : fmap indent details)
    millis ns = show (ns `div` 1_000_000) ++ "ms"
    prettyProcessStats ProcessStats {maxMemInUseBytes, maxLiveBytes} =
      "mem " ++ show maxMemInUseBytes ++ ", live " ++ show maxLiveBytes

prettyBytecode :: [TrackedBytecode] -> String
prettyBytecode entries = show (length entries) ++ " entries"
