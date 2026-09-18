{-# LANGUAGE ApplicativeDo #-}

module GhcWorker.Run where

import BuckWorkerProto (Instrument, Worker)
import Common.Grpc (GrpcHandler (..), fromGrpcHandler)
import Control.Applicative (many, optional, (<|>))
import Control.Concurrent (MVar, newChan, newMVar)
import Control.Concurrent.Chan (Chan)
import Data.Bifunctor (first)
import Data.Functor (void)
import Data.Text (pack, unpack)
import GhcWorker.GhcHandler (ghcHandler)
import GhcWorker.Grpc (instrumentMethods)
import GhcWorker.Instrumentation (WorkerStatus (..), toGrpcHandler)
import GhcWorker.Orchestration (CreateMethods (..), runCentralGhcSpawned)
import Internal.State (newState)
import Network.GRPC.Server.Protobuf (ProtobufMethodsOf)
import Network.GRPC.Server.StreamType (Methods)
import Options.Applicative (
  Parser,
  ParserInfo,
  eitherReader,
  execParser,
  fullDesc,
  header,
  help,
  helper,
  info,
  long,
  metavar,
  option,
  progDesc,
  strOption,
  (<**>),
  )
import System.OsPath.Extra (toOsPath)
import Types.Api (Event)
import Types.FeatureFlags (parseFeatureFlag)
import Types.Grpc (CommandEnv, RequestArgs)
import Types.Log (TraceId (..))
import Types.Orchestration (ServerSocketPath (..), serverSocketFromPath)
import Types.Settings (Settings (..), defaultSettings, parseByteSize, setFeature)
import Types.State (WorkerState (..))

-- | Global options for the worker, passed when the process is started, in contrast to request options stored in
-- 'BuckArgs'.
data CliOptions =
  CliOptions {
    -- | If this is given, the app should start a GHC server synchronously, listening on the given path.
    serve :: ServerSocketPath,

    -- | Runtime feature flags.
    settings :: Settings
  }
  deriving stock (Eq, Show)

-- | Parser for runtime feature flags.
featureFlagsParser :: Parser Settings
featureFlagsParser =
  applyFlags <$> many (
    (option (flagOption True) (long "enable" <> metavar "FEATURE" <> help "Enable an optional feature"))
    <|>
    (option (flagOption False) (long "disable" <> metavar "FEATURE" <> help "Disable an optional feature"))
  )
  where
    applyFlags = flip foldr defaultSettings (uncurry setFeature)

    flagOption value = do
      flag <- eitherReader (first unpack . parseFeatureFlag . pack)
      pure (flag, value)

-- | Parser for '--max-bytecode', bounding the loader state size.
maxBytecodeParser :: Parser (Maybe Int)
maxBytecodeParser =
  optional (
    option (eitherReader parseByteSize) (
      long "max-bytecode"
      <> metavar "NUM[k|M|G]"
      <> help "Maximum number of BCOs in the loader state (enables eviction)"
      )
    )

cliOptionsParser :: Parser CliOptions
cliOptionsParser = do
  serve <- serverSocketFromPath . toOsPath <$> strOption (long "serve" <> metavar "SOCKET" <> help "Socket path for the GHC server")
  features0 <- featureFlagsParser
  maxBytecode <- maxBytecodeParser
  pure CliOptions {serve, settings = features0 {lazyByteCodeCacheLimit = maxBytecode}}

cliOptionsParserInfo :: ParserInfo CliOptions
cliOptionsParserInfo =
  info (cliOptionsParser <**> helper)
    (fullDesc <> progDesc "GHC persistent worker" <> header "ghc-worker")

-- | Allocate a communication channel for instrumentation events and construct a gRPC server handler that streams said
-- events to a client.
--
-- Returns the channel so that a GHC server can use it to send events.
createInstrumentMethods ::
  MVar WorkerState ->
  (CommandEnv -> RequestArgs -> IO ()) ->
  IO (Chan Event, Methods IO (ProtobufMethodsOf Instrument))
createInstrumentMethods stateVar recompile = do
  instrChan <- newChan
  pure (instrChan, instrumentMethods instrChan stateVar recompile)

-- | Construct a gRPC server handler for the main part of the persistent worker.
createGhcMethods ::
  MVar WorkerState ->
  MVar WorkerStatus ->
  Maybe TraceId ->
  Maybe (Chan Event) ->
  IO (CommandEnv -> RequestArgs -> IO (), Methods IO (ProtobufMethodsOf Worker))
createGhcMethods state status traceId instrChan =
  let handler = toGrpcHandler (ghcHandler state traceId) status state instrChan
      voidRun commandEnv requestArgs =
        void $ handler.run commandEnv requestArgs
  in pure (voidRun, fromGrpcHandler handler)

-- | Main function for running the default persistent worker using the provided server socket path and CLI options.
runWorker :: CliOptions -> IO ()
runWorker CliOptions {serve, settings} = do
  state <- newState settings
  status <- newMVar WorkerStatus {active = 0, nextRequestId = 0}
  let
    methods = CreateMethods {
      createInstrumentation = createInstrumentMethods state,
      createGhc = createGhcMethods state status traceId
    }
  runCentralGhcSpawned methods settings serve
  where
    traceId = if null serve.traceId then Nothing else Just (TraceId serve.traceId)

parseCliArgs :: IO CliOptions
parseCliArgs = execParser cliOptionsParserInfo
