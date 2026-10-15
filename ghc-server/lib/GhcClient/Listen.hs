-- | The client's @listen@ command: stream instrumentation events from a running server and print them to stdout.
module GhcClient.Listen where

import BuckWorkerProto ()
import Data.Binary (decode)
import Data.ByteString (fromStrict)
import Data.List (intercalate)
import Data.Text qualified as Text
import GhcServer.Optparse (readPath)
import GhcServer.Path (socketPath)
import Network.GRPC.Client (Server (..), rpc, withConnection)
import Network.GRPC.Client.StreamType.IO (serverStreaming)
import Network.GRPC.Common (def)
import Network.GRPC.Common.NextElem (whileNext_)
import Network.GRPC.Common.Protobuf (Protobuf, defMessage)
import Options.Applicative (
  Parser,
  ReadM,
  eitherReader,
  help,
  long,
  metavar,
  option,
  optional,
  showDefault,
  value,
  )
import Proto.GhcServer (GhcServer)
import Proto.Instrument (Instrument)
import System.Directory.OsPath (getCurrentDirectory)
import System.IO (hPutStrLn, stderr)
import System.OsPath (OsPath)
import System.OsPath.Extra (fromOsPath)
import Types.Api (
  Event (..),
  ExecutorId (..),
  ExecutorStatus (..),
  ProcessStats (..),
  RequestId (..),
  TrackedBytecode,
  UnitSummary (..),
  renderTarget,
  )

-- | The gRPC service whose @Events@ endpoint is read.
data Protocol =
  ProtocolGhcServer
  |
  ProtocolInstrument
  deriving stock (Eq, Show)

data ListenConfig =
  ListenConfig {
    protocol :: Protocol,
    projectRoot :: Maybe OsPath,
    socket :: Maybe OsPath
  }
  deriving stock (Eq, Show)

protocolReader :: ReadM Protocol
protocolReader =
  eitherReader \case
    "ghc-server" -> Right ProtocolGhcServer
    "instrument" -> Right ProtocolInstrument
    other -> Left ("Invalid protocol: " ++ other ++ " (supported: ghc-server, instrument)")

listenParser :: Parser ListenConfig
listenParser = do
  protocol <-
    option protocolReader (
      long "protocol" <> metavar "PROTOCOL" <> value ProtocolGhcServer <> showDefault
      <> help "Service providing the events: ghc-server or instrument"
    )
  projectRoot <- optional (option readPath (long "root" <> metavar "PROJECT_ROOT" <> help "Path to the project directory"))
  socket <- optional (option readPath (long "socket" <> metavar "SOCKET" <> help "Path to the server's Unix socket, overriding the one derived from the project root"))
  pure ListenConfig {..}

resolveSocket :: ListenConfig -> IO OsPath
resolveSocket config =
  case config.socket of
    Just path -> pure path
    Nothing -> socketPath <$> maybe getCurrentDirectory pure config.projectRoot

-- | Connect to the server and print each received event until the stream ends.
listen :: ListenConfig -> IO ()
listen config = do
  socket <- resolveSocket config
  hPutStrLn stderr ("Listening for events at " ++ fromOsPath socket)
  withConnection def (ServerUnix (fromOsPath socket)) \ connection ->
    case config.protocol of
      ProtocolGhcServer ->
        serverStreaming connection (rpc @(Protobuf GhcServer "events")) defMessage \ recv ->
          whileNext_ recv \ message -> printEvent (decode (fromStrict message.payload))
      ProtocolInstrument ->
        serverStreaming connection (rpc @(Protobuf Instrument "events")) defMessage \ recv ->
          whileNext_ recv \ message -> printEvent (decode (fromStrict message.payload))
  hPutStrLn stderr "Event stream ended."

printEvent :: Event -> IO ()
printEvent = putStrLn . prettyEvent

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
  CompileEnd {target, exitCode, stderr, result, processStats, noMain, requestId} ->
    withDetails
      (unwords (
        [status, requestTag requestId, Text.unpack (renderTarget target)]
        ++ ["(no main)" | noMain]
      ))
      (
        foldMap (\ r -> ["result: " ++ r]) result
        ++ foldMap (pure . prettyProcessStats) processStats
        ++ [stderr | not (null stderr)]
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
