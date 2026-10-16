{-# LANGUAGE ApplicativeDo #-}

module GhcWorker.Run where

import BuckWorkerProto (Worker)
import Control.Applicative (many, optional, (<|>))
import Control.Concurrent (MVar, newChan, newMVar)
import Control.Concurrent.Chan (Chan)
import Data.Bifunctor (first)
import Data.Functor (void)
import Data.Text (pack, unpack)
import GhcWorker.GhcHandler (ghcHandler)
import GhcWorker.Grpc (ghcMethods)
import GhcWorker.Instrumentation (WorkerStatus (..), toGrpcHandler)
import GhcWorker.Orchestration (runCentralGhcSpawned)
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
  value,
  (<**>),
  )
import System.OsPath.Extra (toOsPath)
import Types.Api (Event)
import Types.FeatureFlags (Feature (..), parseFeatureFlag)
import Types.Grpc (CommandEnv, RequestArgs)
import Types.Log (TraceId (..))
import Types.Orchestration (ServerSocketPath (..), serverSocketFromPath)
import Types.Settings (Settings (..), defaultExecutorRts, defaultSettings, featureOn, parseByteSize, setFeature)
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

    flagOption enabled = do
      flag <- eitherReader (first unpack . parseFeatureFlag . pack)
      pure (flag, enabled)

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

-- | Parser for '--executor-rts', the RTS options of persistent executor subprocesses.
executorRtsParser :: Parser [String]
executorRtsParser =
  words <$> strOption (
    long "executor-rts"
    <> metavar "OPTS"
    <> help "RTS options for persistent executor subprocesses, separated by spaces"
    <> value (unwords defaultExecutorRts)
    )

cliOptionsParser :: Parser CliOptions
cliOptionsParser = do
  serve <- serverSocketFromPath . toOsPath <$> strOption (long "serve" <> metavar "SOCKET" <> help "Socket path for the GHC server")
  features0 <- featureFlagsParser
  maxBytecode <- maxBytecodeParser
  executorRts <- executorRtsParser
  pure CliOptions {serve, settings = features0 {lazyByteCodeCacheLimit = maxBytecode, executorRts}}

cliOptionsParserInfo :: ParserInfo CliOptions
cliOptionsParserInfo =
  info (cliOptionsParser <**> helper)
    (fullDesc <> progDesc "GHC persistent worker" <> header "ghc-worker")

-- | Construct the 'Worker' service for the persistent worker.
-- Instrumentation is only active if the feature flag is enabled.
createMethods ::
  Settings ->
  MVar WorkerState ->
  MVar WorkerStatus ->
  Maybe TraceId ->
  IO (Methods IO (ProtobufMethodsOf Worker))
createMethods settings state status traceId = do
  instrChan <- if featureOn FeatureInstrument settings then Just <$> newChan else pure Nothing
  pure (ghcMethods state instrChan (toGrpcHandler (ghcHandler state traceId) status state instrChan))

-- | Main function for running the default persistent worker using the provided server socket path and CLI options.
runWorker :: CliOptions -> IO ()
runWorker CliOptions {serve, settings} = do
  state <- newState settings
  status <- newMVar WorkerStatus {active = 0, nextRequestId = 0}
  methods <- createMethods settings state status traceId
  runCentralGhcSpawned methods serve
  where
    traceId = if null serve.traceId then Nothing else Just (TraceId serve.traceId)

parseCliArgs :: IO CliOptions
parseCliArgs = execParser cliOptionsParserInfo
