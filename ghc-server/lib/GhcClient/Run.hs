{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}

module GhcClient.Run where

import BuckWorkerProto ()
import Control.Concurrent (threadDelay)
import Control.Exception (throwIO, try)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Data.Bifunctor (first)
import Data.Text qualified as Text
import Data.Text.Encoding (encodeUtf8)
import GhcServer.Data.Config (ClientConfig (..))
import GhcServer.Path (socketPath)
import Internal.Log (dbg)
import Network.GRPC.Client (Server (..), recvNextOutput, sendFinalInput, withConnection, withRPC)
import Network.GRPC.Common (Proxy (..), def)
import Network.GRPC.Common.Protobuf (Proto, Protobuf, defMessage, (&), (.~))
import Options.Applicative (
  Parser,
  ParserInfo,
  argument,
  eitherReader,
  execParser,
  fullDesc,
  header,
  help,
  helper,
  info,
  long,
  many,
  metavar,
  progDesc,
  short,
  strArgument,
  switch,
  (<**>),
  )
import Proto.GhcServer (ExecuteCommand, ExecuteResponse, GhcServer)
import Proto.GhcServer_Fields qualified as Fields
import System.Exit (die)
import System.IO (BufferMode (..), hPutStrLn, hSetBuffering, stderr, stdout)
import System.OsPath (OsPath, encodeUtf)
import System.OsPath.Extra (fromOsPath)

-- | CLI argument parser for the client.
clientConfigParser :: Parser ClientConfig
clientConfigParser = do
  projectRoot <- argument readOsPath (metavar "PROJECT_ROOT" <> help "Path to the project root directory")
  wait <- switch (long "wait" <> short 'w' <> help "Wait for the build to complete before returning")
  recompile <- switch (long "recompile" <> help "Recompile modules even when cached artifacts exist")
  rebuild <- switch (long "rebuild" <> help "Recompute metadata and recompile even when cached")
  targets <- many (strArgument (metavar "TARGETS..." <> help "Schedule targets (e.g. unit1 unit2:metadata unit2:Module)"))
  pure ClientConfig {..}
  where
    readOsPath =
      eitherReader (first show <$> encodeUtf)

clientParserInfo :: ParserInfo ClientConfig
clientParserInfo =
  info (clientConfigParser <**> helper) (fullDesc <> progDesc desc <> header "CLI for ghc-server")
  where
    desc = "Send build commands to ghc-server"

-- | Send an 'ExecuteCommand' to the 'GhcServer' service on a new connection and return the response.
sendExecute :: Server -> Proto ExecuteCommand -> IO (Proto ExecuteResponse)
sendExecute server request =
  withConnection def server \ connection ->
    withRPC connection def (Proxy @(Protobuf GhcServer "execute")) \ call -> do
      sendFinalInput call request
      recvNextOutput call

-- | Poll the given socket path until 'ghc-server' responds, by repeatedly attempting a real 'sendExecute' call.
-- Retries up to 30 times with 100ms delay (3 seconds total).
waitPoll :: OsPath -> IO ()
waitPoll socketP =
  check maxRetries
  where
    maxRetries :: Int
    maxRetries = 30

    check 0 = throwIO (userError "GHC server didn't respond within 3 seconds")
    check n =
      try connect >>= \case
        Right _ -> pure ()
        Left (_ :: IOError) -> do
          threadDelay 100_000
          check (n - 1)

    -- The part that throws is in 'withConnection', so this has to be executed every time.
    connect = sendExecute (ServerUnix (fromOsPath socketP)) defMessage

-- | Wait for the server to come online, then send a gRPC request to schedule jobs.
client :: ClientConfig -> ExceptT String IO ()
client config = do
  liftIO do
    hPutStrLn stderr ("Connecting to ghc-server at " ++ fromOsPath socket)
    waitPoll socket
  response <- liftIO $ sendExecute (ServerUnix (fromOsPath socket)) request
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

    socket = socketPath config.projectRoot

-- | Parse CLI args and run the client command.
runClient :: IO ()
runClient = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  config <- execParser clientParserInfo
  either die pure =<< runExceptT (client config)
