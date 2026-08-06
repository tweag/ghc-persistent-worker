-- | Main module for the standalone GHC build server.
-- Listens on a Unix socket and accepts build schedule requests via the Worker gRPC protocol.
module GhcServer.Run where

import Common.Grpc (fromGrpcHandler, runGrpcServer)
import Control.Applicative (many, optional, (<|>))
import Control.Concurrent.Async (async)
import Control.Monad (void)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import GhcServer.Cabal.Setup (cabalSetup)
import GhcServer.Data.Config (ServerConfig (..))
import GhcServer.Grpc (instrumentMethods)
import GhcServer.Handler (ServerContext (..), serverContext)
import GhcServer.Path (instrumentSocketPath, socketDirName, socketPath)
import Options.Applicative (
  Parser,
  ParserInfo,
  argument,
  auto,
  command,
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
  short,
  str,
  strArgument,
  subparser,
  switch,
  value,
  (<**>),
  )
import Options.Applicative.Builder (allPositional)
import System.Directory.OsPath (createDirectoryIfMissing)
import System.Exit (die)
import System.IO (BufferMode (..), hPutStrLn, hSetBuffering, stderr, stdout)
import System.OsPath (encodeUtf, (</>))
import System.OsPath.Extra (fromOsPath)
import Types.FeatureFlags (FeatureFlag (..), FeatureFlags (..), defaultFeatureFlags, parseByteSize)

-- | Parser for runtime feature flags.
featureFlagsParser :: Parser FeatureFlags
featureFlagsParser =
  (\ flags maxBytecode -> flags {lazyByteCodeCacheLimit = maxBytecode}) <$> flagsParser <*> maxBytecodeParser
  where
    flagsParser =
      applyFlags <$> many (
        (option (flagOption True) (long "enable" <> metavar "FEATURE" <> help "Enable an optional feature"))
        <|>
        (option (flagOption False) (long "disable" <> metavar "FEATURE" <> help "Disable an optional feature"))
        )

    applyFlags =
      flip foldl' defaultFeatureFlags \ flags -> \case
        (fixedNodesCache, FeatureFixedNodesCache) -> flags {fixedNodesCache}
        (flagParser, FeatureFlagParser) -> flags {flagParser}
        (concurrentInitUnits, FeatureConcurrentInitUnits) -> flags {concurrentInitUnits}
        (incrementalBuildPlan, FeatureIncrementalBuildPlan) -> flags {incrementalBuildPlan}
        (lazyByteCode, FeatureLazyByteCode) -> flags {lazyByteCode}
        (instrument, FeatureInstrument) -> flags {instrument}

    flagOption v = do
      flag <- eitherReader \case
        "fixed-nodes-cache" -> Right FeatureFixedNodesCache
        "flag-parser" -> Right FeatureFlagParser
        "concurrent-init-units" -> Right FeatureConcurrentInitUnits
        "incremental-build-plan" -> Right FeatureIncrementalBuildPlan
        "lazy-byte-code" -> Right FeatureLazyByteCode
        "instrument" -> Right FeatureInstrument
        flag -> Left ("Invalid feature flag: " ++ flag)
      pure (v, flag)

-- | Parser for '--max-bytecode', bounding the lazily-loaded bytecode cache (see 'FeatureFlags.lazyByteCodeCacheLimit').
maxBytecodeParser :: Parser (Maybe Int)
maxBytecodeParser =
  optional (
    option (eitherReader parseByteSize) (
      long "max-bytecode"
      <> metavar "NUM[M|G]"
      <> help "Upper bound on the tracked size of lazily-loaded bytecode kept in the HPT (enables eviction)"
      )
    )

data ExecMode =
  ExecServer ServerConfig
  |
  ExecCabalSetup [String]
  deriving stock (Show)

-- | CLI argument parser for the server.
serverConfigParser :: Parser ServerConfig
serverConfigParser =
  ServerConfig
    <$> argument readOsPath (metavar "PROJECT_ROOT" <> help "Path to the project root directory")
    <*> option auto (long "jobs" <> short 'j' <> metavar "N" <> help "Maximum concurrent jobs" <> value 4)
    <*> switch (long "verbose" <> short 'v' <> help "Print the build log on success")
    <*> switch (long "cabal" <> help "Use .cabal file for project discovery")
    <*> featureFlagsParser
  where
    readOsPath = str >>= \ s ->
      case encodeUtf s of
        Right p -> pure p
        Left e -> fail ("Invalid path: " ++ show e)

cabalSetupParser :: Parser [String]
cabalSetupParser =
  subparser $
  command "act-as-setup" (info (many (strArgument mempty)) (allPositional <> progDesc "cabal act-as-setup proxy"))

execModeParser :: Parser ExecMode
execModeParser =
  (ExecCabalSetup <$> cabalSetupParser)
  <|>
  (ExecServer <$> serverConfigParser)

serverParserInfo :: ParserInfo ExecMode
serverParserInfo =
  info (execModeParser <**> helper)
    (fullDesc <> progDesc "Standalone GHC build server" <> header "ghc-server")

-- | Run the server: parse CLI args, start the gRPC server on a Unix socket. If the @instrument@ feature is
-- enabled, also starts the Instrument gRPC service on a second socket, mirroring 'ghc-worker'\'s behavior.
runServer :: IO ()
runServer = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  execParser serverParserInfo >>= \case
    ExecServer config -> either die pure =<< runExceptT (server config)
    ExecCabalSetup args -> cabalSetup args
  where
    server :: ServerConfig -> ExceptT String IO ()
    server config = do
      let socket = socketPath config.projectRoot
      lift do
        createDirectoryIfMissing True (config.projectRoot </> socketDirName)
        ServerContext {grpcHandler, stateVar, build, project, instrChan} <- serverContext config
        let methods = fromGrpcHandler grpcHandler
        case instrChan of
          Just chan -> do
            let instrSocket = instrumentSocketPath config.projectRoot
            hPutStrLn stderr ("Starting instrument service on " ++ fromOsPath instrSocket)
            void $ async $ runGrpcServer instrSocket (instrumentMethods chan stateVar build project)
          Nothing ->
            pure ()
        hPutStrLn stderr ("Starting ghc-server on " ++ fromOsPath socket)
        runGrpcServer socket methods
