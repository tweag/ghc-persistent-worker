{-# LANGUAGE ApplicativeDo #-}

-- | Main module for the standalone GHC build server.
module GhcServer.Run where

import Common.Grpc (runGrpcServer)
import Control.Applicative (many, optional, (<|>))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Data.Bifunctor (first)
import Data.Text (pack, unpack)
import GhcServer.Build.ExecutorChild (runExecutor)
import GhcServer.Build.ProcessChild (runProcessEval)
import GhcServer.Cabal.Setup (cabalSetup)
import GhcServer.Data.Config (ServerConfig (..))
import GhcServer.Data.ProcessEval (ProcessEvalOptions (..))
import GhcServer.Grpc (serverMethods)
import GhcServer.Handler (serverContext)
import GhcServer.Optparse (readPath)
import GhcServer.Path (socketDirName, socketPath)
import Options.Applicative (
  Parser,
  ParserInfo,
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
  strArgument,
  subparser,
  switch,
  value,
  (<**>),
  )
import Options.Applicative.Builder (allPositional)
import System.Directory.OsPath (canonicalizePath, createDirectoryIfMissing, getCurrentDirectory)
import System.Exit (die)
import System.IO (BufferMode (..), hPutStrLn, hSetBuffering, stderr, stdout)
import System.OsPath (OsPath, (</>))
import System.OsPath.Extra (fromOsPath)
import Types.FeatureFlags (parseFeatureFlag)
import Types.Settings (Settings (..), defaultSettings, parseByteSize, setFeature)

-- | Parser for runtime feature flags.
featureFlagsParser :: Parser Settings
featureFlagsParser =
  (\ flags maxBytecode -> flags {lazyByteCodeCacheLimit = maxBytecode}) <$> flagsParser <*> maxBytecodeParser
  where
    flagsParser =
      applyFlags <$> many (
        (option (flagOption True) (long "enable" <> metavar "FEATURE" <> help "Enable an optional feature"))
        <|>
        (option (flagOption False) (long "disable" <> metavar "FEATURE" <> help "Disable an optional feature"))
        )

    applyFlags = flip foldr defaultSettings (uncurry setFeature)

    flagOption v = do
      flag <- eitherReader (first unpack . parseFeatureFlag . pack)
      pure (flag, v)

-- | Parser for '--max-bytecode', bounding the lazily-loaded bytecode cache (see 'Settings.lazyByteCodeCacheLimit').
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
  ExecServer RawServerConfig
  |
  ExecCabalSetup [String]
  |
  ExecEvaluate ProcessEvalOptions
  |
  ExecExecutor OsPath
  deriving stock (Show)

-- | Server CLI config prior to resolving the project root, which defaults to the current directory when not given
-- explicitly (see 'resolveServerConfig').
data RawServerConfig =
  RawServerConfig {
    projectRoot :: Maybe OsPath,
    maxJobs :: Int,
    verbose :: Bool,
    jsonConfig :: Bool,
    settings :: Settings
  }
  deriving stock (Show)

-- | CLI argument parser for the server.
serverConfigParser :: Parser RawServerConfig
serverConfigParser = do
  projectRoot <- optional (option readPath (long "root" <> metavar "PROJECT_ROOT" <> help "Path to the project directory"))
  maxJobs <- option auto (long "jobs" <> short 'j' <> metavar "N" <> help "Maximum concurrent jobs" <> value 4)
  verbose <- switch (long "verbose" <> short 'v' <> help "Print the build log on success")
  jsonConfig <- switch (long "json-config" <> help "Force unit.json-based project discovery even if a .cabal file is present")
  settings <- featureFlagsParser
  pure RawServerConfig {..}

-- | Resolve a 'RawServerConfig' into a 'ServerConfig', defaulting the project root to the current directory when
-- not given explicitly.
resolveServerConfig :: RawServerConfig -> IO ServerConfig
resolveServerConfig raw = do
  -- Canonicalize unconditionally: the project root is used verbatim to compute source paths recorded in
  -- on-disk caches ('cached_unit.json', 'dep_units.json') and to key in-memory scheduler task IDs
  -- ('GhcServer.Build.Schedule.PendingSource'). Two server invocations pointed at the same physical directory
  -- via different (both valid) path spellings would otherwise produce syntactically different 'OsPath' values
  -- for the same file, causing cache entries written by one invocation to silently fail to resolve pending
  -- compile tasks submitted to another -- such an unresolved pending task is never promoted, dispatched, or
  -- reported as a failure, so the request completes as a silent, incorrect "success".
  projectRoot <- canonicalizePath =<< maybe getCurrentDirectory pure raw.projectRoot
  pure ServerConfig {
    projectRoot,
    maxJobs = raw.maxJobs,
    verbose = raw.verbose,
    jsonConfig = raw.jsonConfig,
    settings = raw.settings
  }

cabalSetupInfo :: ParserInfo ExecMode
cabalSetupInfo =
  ExecCabalSetup <$> info (many (strArgument mempty)) (allPositional <> progDesc "cabal act-as-setup proxy")

evaluateParser :: Parser ProcessEvalOptions
evaluateParser = do
  configFile <- optional (option readPath (long "config" <> help "JSON file containing the evaluation config"))
  pure ProcessEvalOptions {..}

evaluateInfo :: ParserInfo ExecMode
evaluateInfo =
  ExecEvaluate <$> info evaluateParser (allPositional <> progDesc "Evaluate an expression")

executorInfo :: ParserInfo ExecMode
executorInfo =
  ExecExecutor <$> info parser (progDesc "Run a persistent executor serving execute tasks until stdin closes")
  where
    parser = option readPath (long "socket" <> metavar "PATH" <> help "Unix socket to serve the executor on")

execModeParser :: Parser ExecMode
execModeParser =
  subparser (
    command "act-as-setup" cabalSetupInfo <> command "eval" evaluateInfo <> command "executor" executorInfo
    )
  <|>
  (ExecServer <$> serverConfigParser)

serverParserInfo :: ParserInfo ExecMode
serverParserInfo =
  info (execModeParser <**> helper) (fullDesc <> progDesc "GHC build server" <> header "ghc-server")

-- | Run the server: parse CLI args, start the single gRPC service ('GhcServer.Grpc.serverMethods') on a Unix
-- socket.
--
-- Checks for 'GhcServer.Build.Process.evalProcessFlag' before running the normal 'optparse-applicative' parser:
-- a self-relaunched execute-task child process (see 'GhcServer.Build.Process') is a distinct, much simpler
-- entry point that never starts the gRPC server, so it must be dispatched before argument parsing that assumes
-- a 'RawServerConfig' shape.
runServer :: IO ()
runServer = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  execParser serverParserInfo >>= \case
    ExecServer raw -> either die pure =<< runExceptT (server =<< lift (resolveServerConfig raw))
    ExecCabalSetup cabalArgs -> cabalSetup cabalArgs
    ExecEvaluate options -> runProcessEval options
    ExecExecutor socket -> runExecutor socket
  where
    server :: ServerConfig -> ExceptT String IO ()
    server config =
      lift do
        let socket = socketPath config.projectRoot
        createDirectoryIfMissing True (config.projectRoot </> socketDirName)
        ctx <- serverContext config
        hPutStrLn stderr ("Starting ghc-server on " ++ fromOsPath socket)
        runGrpcServer socket (serverMethods ctx)
