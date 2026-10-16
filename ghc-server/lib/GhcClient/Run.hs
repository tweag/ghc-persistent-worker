{-# LANGUAGE ApplicativeDo #-}

module GhcClient.Run where

import BuckWorkerProto ()
import Common.Grpc (sendRequest, waitPoll)
import Control.Applicative ((<|>))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Data.Text qualified as Text
import Data.Text.Encoding (encodeUtf8)
import GhcClient.Listen (ListenConfig, listen, listenParser)
import GhcServer.Data.Config (ClientConfig (..))
import GhcServer.Data.Request (ExecutorSelection (..))
import Types.Api (ExecutorId (..))
import GhcServer.Optparse (readPath)
import GhcServer.Path (socketPath)
import Internal.Log (dbg)
import Network.GRPC.Client (Server (..))
import Network.GRPC.Common.Protobuf (Proto, defMessage, (&), (.~))
import Options.Applicative (
  Parser,
  ParserInfo,
  eitherReader,
  execParser,
  flag,
  fullDesc,
  header,
  command,
  help,
  helper,
  hsubparser,
  info,
  long,
  many,
  metavar,
  option,
  optional,
  progDesc,
  short,
  str,
  strArgument,
  switch,
  (<**>),
  )
import Proto.Worker (ExecuteCommand)
import Proto.Worker_Fields qualified as Fields
import System.Directory.OsPath (getCurrentDirectory)
import System.Exit (die)
import System.IO (BufferMode (..), hPutStrLn, hSetBuffering, stderr, stdout)
import System.OsPath.Extra (fromOsPath)

-- | CLI argument parser for the client.
clientConfigParser :: Parser ClientConfig
clientConfigParser = do
  projectRoot <- optional (option readPath (long "root" <> metavar "PROJECT_ROOT" <> help "Path to the project directory"))
  wait <- switch (long "wait" <> short 'w' <> help "Wait for the build to complete before returning")
  recompile <- switch (long "recompile" <> help "Recompile modules even when cached artifacts exist")
  rebuild <- switch (long "rebuild" <> help "Recompute metadata and recompile even when cached")
  executor <- executorParser
  targets <- many (strArgument (metavar "TARGETS..." <> help "Schedule targets (e.g. unit1 unit2:metadata unit2:Module)"))
  pure ClientConfig {..}

-- | @--executor ID@ selects a persistent executor subprocess; @--process@ is shorthand for @--executor default@;
-- @--executors unit@ uses a separate executor for each unit.
executorParser :: Parser (Maybe ExecutorSelection)
executorParser =
  Just . ExecutorNamed . ExecutorId <$> option str (long "executor" <> metavar "ID" <> help "Run execute tasks in the persistent executor subprocess ID")
  <|>
  Just <$> option (eitherReader executorScheme) (long "executors" <> metavar "SCHEME" <> help "Run execute tasks in multiple persistent executor subprocesses; 'unit' uses one per unit")
  <|>
  flag Nothing (Just (ExecutorNamed "default")) (long "process" <> help "Run execute tasks in the persistent executor subprocess 'default'")
  where
    executorScheme = \case
      "unit" -> Right ExecutorPerUnit
      other -> Left ("Invalid executor scheme: " ++ other ++ " (supported: unit)")

data Command =
  Build ClientConfig
  |
  Listen ListenConfig

-- | The @listen@ subcommand streams events; without a subcommand, targets are scheduled for building.
commandParser :: Parser Command
commandParser =
  hsubparser (command "listen" (info (Listen <$> listenParser) (progDesc "Print events streamed by the server")))
  <|>
  Build <$> clientConfigParser

clientParserInfo :: ParserInfo Command
clientParserInfo =
  info (commandParser <**> helper) (fullDesc <> progDesc desc <> header "CLI for ghc-server")
  where
    desc = "Send build commands to ghc-server"

-- | Wait for the server to come online, then send a gRPC request to schedule jobs.
client :: ClientConfig -> ExceptT String IO ()
client config = do
  socket <- socketPath <$> liftIO (maybe getCurrentDirectory pure config.projectRoot)
  liftIO do
    hPutStrLn stderr ("Connecting to ghc-server at " ++ fromOsPath socket)
    waitPoll socket
  response <- liftIO $ sendRequest (ServerUnix (fromOsPath socket)) request
  dbg (Text.unpack response.stderr)
  if response.exitCode == 0
  then dbg "Build succeeded."
  else throwE "Build failed."
  where
    request :: Proto ExecuteCommand
    request =
      defMessage
        & Fields.argv .~ ("schedule" : flagArgs ++ [encodeUtf8 (Text.pack arg) | arg <- config.targets])

    flagArgs =
      ["--wait" | config.wait]
      ++ ["--recompile" | config.recompile]
      ++ ["--rebuild" | config.rebuild]
      ++ foldMap executorArgs config.executor

    executorArgs = \case
      ExecutorNamed executorId -> ["--executor", encodeUtf8 executorId.text]
      ExecutorPerUnit -> ["--executors", "unit"]

-- | Parse CLI args and run the client command.
runClient :: IO ()
runClient = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  config <- execParser clientParserInfo
  case config of
    Build clientConfig -> either die pure =<< runExceptT (client clientConfig)
    Listen listenConfig -> listen listenConfig
