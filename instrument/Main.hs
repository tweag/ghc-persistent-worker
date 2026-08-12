module Main where

import Brick.BChan (BChan, newBChan, writeBChan)
import BuckWorkerProto (Instrument)
import Control.Applicative ((<|>))
import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (SomeException, catch, try, IOException)
import Control.Monad (filterM, forever, void, when)
import Data.Binary (decode)
import Data.ByteString (fromStrict)
import Data.Foldable (for_)
import Data.List (isInfixOf)
import Data.Maybe (fromMaybe, isNothing)
import Data.Text qualified as Text
import Data.Time (getCurrentTime)
import Graphics.Vty (Vty (shutdown))
import Network.GRPC.Client (Server (ServerUnix), rpc, withConnection)
import Network.GRPC.Client.StreamType.IO (serverStreaming)
import Network.GRPC.Common (def)
import Network.GRPC.Common.NextElem (whileNext_)
import Network.GRPC.Common.Protobuf (Protobuf, defMessage)
import Options.Applicative (
  Parser,
  ParserInfo,
  execParser,
  fullDesc,
  header,
  help,
  helper,
  info,
  long,
  metavar,
  optional,
  progDesc,
  strOption,
  (<**>),
  )
import ServeGhcServer (defaultSocketPath, ensureGhcServer, isServerUp)
import System.Directory (doesPathExist, getModificationTime, listDirectory, createDirectoryIfMissing, getCurrentDirectory)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import System.FSNotify (Event (..), EventIsDirectory (..), watchDir, withManager)
import System.IO (Handle)
import System.IO qualified as IO
import UI qualified
import UI.Session qualified as Session
import UI.SessionSelector qualified as SS
import UI.Types (WorkerId (WorkerId))

-- | CLI options for the @instrument@ client.
newtype Options = Options {serverExe :: Maybe FilePath}

optionsParser :: Parser Options
optionsParser =
  Options
    <$> optional (
      strOption (
        long "server-exe"
        <> metavar "PATH"
        <> help "Path to the ghc-server executable to use when starting one (defaults to a PATH lookup)"
        )
      )

optionsInfo :: ParserInfo Options
optionsInfo =
  info (optionsParser <**> helper) (fullDesc <> progDesc "Instrumentation TUI client" <> header "instrument")

newtype WorkerPath
  = WorkerPath {path :: FilePath}
  deriving stock (Eq, Show)

envWorkerPath :: IO WorkerPath
envWorkerPath = WorkerPath . (++ "/") . fromMaybe "/tmp/ghc-persistent-worker" <$> lookupEnv "WORKER_PATH"

listen :: BChan UI.Event -> FilePath -> IO ()
listen eventChan instrPath = do
  void $ forkIO $ go 5
 where
  -- TODO: This is a hack, ids should be sent over grpc
  (sessionId', workerId') = break (== '_') instrPath
  sessionId = Session.Id $ Text.pack sessionId'
  workerId = WorkerId $ Text.pack workerId'
  go :: Int -> IO ()
  go 0 = writeBChan eventChan $ UI.SessionSelectorEvent $ SS.RemoveWorker sessionId workerId
  go n =
    catch @SomeException
      ( withConnection def (ServerUnix instrPath) $ \conn -> do
          serverStreaming conn (rpc @(Protobuf Instrument "notifyMe")) defMessage $ \recv -> do
            time <- getModificationTime instrPath
            writeBChan eventChan $ UI.SessionSelectorEvent $ SS.AddWorker sessionId workerId time conn
            writeBChan eventChan (UI.SendOptions (Just workerId))
            whileNext_ recv
              $ writeBChan eventChan
              . UI.SessionSelectorEvent
              . SS.SessionEvent sessionId
              . Session.InstrEvent workerId
              . decode
              . fromStrict
              . (.encoded)
      )
      (const $ threadDelay 100_000 >> go (n - 1))

-- | Builds the closure passed to 'UI.initialState' that backs the capital-@S@ key binding: ensures a @ghc-server@
-- is running for the given path (empty means the current directory) with the given extra CLI options, then starts
-- listening on its Instrument socket.
startServer :: Maybe FilePath -> BChan UI.Event -> Text.Text -> [String] -> IO ()
startServer serverExe eventChan path extraOpts = do
  let explicitRoot = if Text.null path then Nothing else Just (Text.unpack path)
  (sock, mHandles) <- ensureGhcServer serverExe explicitRoot extraOpts
  for_ mHandles \ (out, err) -> do
    void $ forkIO $ streamLines eventChan (Text.pack "stdout") out
    void $ forkIO $ streamLines eventChan (Text.pack "stderr") err
  listen eventChan sock

-- | Read lines from a spawned ghc-server's stdout\/stderr handle until it closes (EOF or the process exits),
-- dispatching each as a 'UI.ProcessLog' event tagged with the given stream name. Runs in its own thread so the
-- two streams (stdout, stderr) can be read concurrently without blocking each other.
streamLines :: BChan UI.Event -> Text.Text -> Handle -> IO ()
streamLines eventChan streamName h =
  catch @SomeException loop (const (pure ()))
  where
    loop = do
      eof <- IO.hIsEOF h
      if eof
        then pure ()
        else do
          line <- IO.hGetLine h
          writeBChan eventChan (UI.ProcessLog streamName (Text.pack line))
          loop

main :: IO ()
main = do
  opts <- execParser optionsInfo
  workers <- envWorkerPath
  workerPathExists <- doesPathExist workers.path
  instrSocketEnv <- lookupEnv "INSTRUMENT_SOCKET"
  cwd <- getCurrentDirectory
  defaultUp <- isServerUp (defaultSocketPath cwd)
  let instrSocket = instrSocketEnv <|> (if defaultUp then Just (defaultSocketPath cwd) else Nothing)
  eventChan <- newBChan 10

  -- Update time every 100ms
  _ <- forkIO $ forever $ do
    time <- getCurrentTime
    writeBChan eventChan (UI.SetTime time)
    threadDelay 100_000

  case instrSocket of
    -- Connect directly to a single known socket (e.g. ghc-server's flat `<project>/socket/instrument` layout),
    -- bypassing the `WORKER_PATH` directory-watching discovery mechanism below.
    Just sock -> void $ forkIO $ listen eventChan sock
    Nothing -> do
      -- Find already running workers
      when workerPathExists do
        primaryDirs <- do
          dirs <- listDirectory workers.path
          filterM (\dir -> doesPathExist (workers.path ++ dir ++ "/instrument")) dirs
        mapM_ (listen eventChan . (++ "/instrument") . (workers.path ++)) primaryDirs

      void $ try @IOException do
        createDirectoryIfMissing True workers.path

  -- Detect new workers
  withManager $ \mgr -> do
    when (isNothing instrSocket) do
      void $ watchDir mgr workers.path (const True) $ \case
        Added dir _ IsDirectory | not ("/log" `isInfixOf` dir) -> do
          listen eventChan $ dir </> "instrument"
        _ -> pure ()

    (_, vty) <- UI.customMainWithDefaultVty (Just eventChan) UI.app (UI.initialState (startServer opts.serverExe eventChan))
    vty.shutdown
