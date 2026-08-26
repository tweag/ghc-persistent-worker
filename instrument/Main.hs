module Main where

import Brick.BChan (BChan, newBChan, writeBChan)
import BuckWorkerProto (Instrument)
import Control.Applicative ((<|>))
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Async (Async, async, cancel)
import Control.Exception (IOException, SomeException, catch, displayException, finally, try)
import Control.Monad (filterM, forever, unless, void, when)
import Data.Binary (decode)
import Data.ByteString (fromStrict)
import Data.Foldable (for_)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
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
  switch,
  (<**>),
  )
import ServeGhcServer (cleanGhcServer, defaultSocketPath, isServerUp, killGhcServer, resolveServerExe, spawnGhcServer)
import System.Directory (doesPathExist, getModificationTime, listDirectory, createDirectoryIfMissing, getCurrentDirectory)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.FSNotify (Event (..), EventIsDirectory (..), watchDir, withManager)
import System.IO (Handle)
import System.IO qualified as IO
import System.Process (ProcessHandle, waitForProcess)
import UI qualified
import UI.Session qualified as Session
import UI.SessionSelector qualified as SS
import UI.Types (WorkerId (WorkerId))

-- | CLI options for the @instrument@ client.
data Options = Options
  { serverExe :: Maybe FilePath
  , -- | Disables the default finalizing behavior of killing the tracked @ghc-server@ instance and cleaning its
    -- @cache@\/@output@ directories on UI exit (see 'finalizeServer'), and likewise leaves the K\/R\/C key bindings
    -- as the only way to affect the server's lifecycle. Useful when the server is meant to outlive this UI session
    -- (e.g. shared across multiple @instrument@\/@ghc-client@ invocations).
    remain :: Bool
  }

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
    <*> switch (
      long "remain"
      <> help "Do not kill the ghc-server instance or clean its cache/output directories on exit"
      )

optionsInfo :: ParserInfo Options
optionsInfo =
  info (optionsParser <**> helper) (fullDesc <> progDesc "Instrumentation TUI client" <> header "instrument")

-- | Tracks the most recently ensured\/spawned @ghc-server@ instance for this session, so the @K@\/@R@\/@C@ key
-- bindings and the exit-time finalizer (see 'finalizeServer') can act on it. Only populated when this client
-- itself spawns\/connects to a server (via the capital-@S@ popup) -- a server discovered through
-- 'INSTRUMENT_SOCKET' or the default-socket auto-connect path at startup is never tracked here, since this client
-- doesn't own its lifecycle in that case.
data ServerInfo = ServerInfo
  { projectRoot :: FilePath
  , -- | The subprocess this client spawned, if any (i.e. 'Nothing' when an already-running server was reused).
    process :: Maybe ProcessHandle
  , -- | The background thread polling 'isServerUp' while the subprocess starts up (see 'startServer'). Cancelled
    -- once the server comes up, the subprocess terminates, or this instance is superseded by a new kill\/restart\/
    -- start request.
    pollAsync :: Maybe (Async ())
  , -- | The raw @(path, extraOpts)@ arguments last passed to 'startServer', kept so 'restartServerAction' can
    -- re-ensure a server identically after killing the old one.
    serveArgs :: (Text.Text, [String])
  }

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

-- | Kill the tracked server's poll thread and\/or subprocess, if any, leaving 'ServerInfo.projectRoot'\/'serveArgs'
-- intact (so a subsequent restart can still reuse them). A no-op when nothing is tracked. Used both by the @K@ key
-- binding and as the mandatory first step of any kill\/restart\/(re)start request (see 'startServer'), so that a
-- stale process\/poll thread from a previous attempt is never left running alongside a new one.
killTracked :: IORef (Maybe ServerInfo) -> IO ()
killTracked serverInfoRef = do
  minfo <- readIORef serverInfoRef
  for_ (minfo >>= (.pollAsync)) cancel
  for_ (minfo >>= (.process)) killGhcServer
  modifyIORef' serverInfoRef (fmap \si -> si{process = Nothing, pollAsync = Nothing})

-- | Poll 'isServerUp' indefinitely (no automatic timeout -- a slow build\/compile inside @ghc-server@'s startup
-- must never be killed by this client) until the socket responds, then start listening on it via 'listen'. Runs
-- as its own 'Async' (tracked in 'ServerInfo.pollAsync') so it can be cancelled by 'killTracked' once the server
-- terminates or a new kill\/restart\/start request supersedes it.
pollUntilUp :: BChan UI.Event -> FilePath -> IO ()
pollUntilUp eventChan sock = do
  up <- isServerUp sock
  if up
    then listen eventChan sock
    else threadDelay 100_000 >> pollUntilUp eventChan sock

-- | Runs in the background thread forked by 'startServer': performs the actual check-existing\/spawn\/wait sequence
-- described there. Split out only so 'startServer' itself can return immediately after forking this.
runStartup :: IORef (Maybe ServerInfo) -> Maybe FilePath -> BChan UI.Event -> Text.Text -> [String] -> IO ()
runStartup serverInfoRef serverExe eventChan path extraOpts = do
  killTracked serverInfoRef
  let explicitRoot = if Text.null path then Nothing else Just (Text.unpack path)
  projectRoot <- maybe getCurrentDirectory pure explicitRoot
  let sock = defaultSocketPath projectRoot
  up <- isServerUp sock
  if up
    then do
      writeIORef serverInfoRef $ Just ServerInfo{projectRoot, process = Nothing, pollAsync = Nothing, serveArgs = (path, extraOpts)}
      listen eventChan sock
    else do
      spawned <- try @SomeException (resolveServerExe serverExe >>= \exe -> spawnGhcServer exe projectRoot extraOpts)
      case spawned of
        Left err -> writeBChan eventChan $ UI.ServerFailed path (Text.pack (displayException err))
        Right (out, err, ph) -> spawnedServer serverInfoRef eventChan path extraOpts projectRoot sock out err ph

-- | Once a subprocess has actually been spawned: starts the indefinite 'isServerUp' poll (recording its 'Async' in
-- 'ServerInfo'), starts streaming its stdout\/stderr, then blocks until the process terminates. On termination,
-- cleans up the tracked state and, if the exit code indicates failure, reports the accumulated stderr via
-- 'UI.ServerFailed'.
spawnedServer ::
  IORef (Maybe ServerInfo) ->
  BChan UI.Event ->
  Text.Text ->
  [String] ->
  FilePath ->
  FilePath ->
  Handle ->
  Handle ->
  ProcessHandle ->
  IO ()
spawnedServer serverInfoRef eventChan path extraOpts projectRoot sock out err ph = do
  poll <- async (pollUntilUp eventChan sock)
  writeIORef serverInfoRef $
    Just ServerInfo{projectRoot, process = Just ph, pollAsync = Just poll, serveArgs = (path, extraOpts)}
  stderrLines <- newIORef []
  void $ forkIO $ streamLines eventChan (Text.pack "stdout") out Nothing
  void $ forkIO $ streamLines eventChan (Text.pack "stderr") err (Just stderrLines)
  exitCode <- waitForProcess ph
  cancel poll
  modifyIORef' serverInfoRef (fmap \si -> si{process = Nothing, pollAsync = Nothing})
  case exitCode of
    ExitSuccess -> pure ()
    ExitFailure _ -> do
      captured <- reverse <$> readIORef stderrLines
      writeBChan eventChan $ UI.ServerFailed path (Text.unlines captured)

-- | Builds the closure passed to 'UI.initialState' that backs the capital-@S@ key binding (and, indirectly, the
-- @R@ restart binding): dispatches 'UI.ServerStarting' immediately so the UI can replace its placeholder message,
-- then forks a background thread ('runStartup') and returns without waiting for it. That thread first kills any
-- previously tracked process\/poll thread (see 'killTracked'), then either connects to an already-running server
-- or spawns a new one, storing its handles in 'ServerInfo', starting an indefinite 'isServerUp' poll thread
-- (also tracked), and finally blocking on the subprocess's termination (see 'spawnedServer') -- there is
-- deliberately no timeout anywhere in this path.
startServer :: IORef (Maybe ServerInfo) -> Maybe FilePath -> BChan UI.Event -> Text.Text -> [String] -> IO ()
startServer serverInfoRef serverExe eventChan path extraOpts = do
  writeBChan eventChan (UI.ServerStarting path)
  void $ forkIO $ runStartup serverInfoRef serverExe eventChan path extraOpts

-- | Backs the capital-@K@ key binding: kills the tracked server's process\/poll thread, if any (see
-- 'killTracked'). Logs (rather than silently no-oping) when nothing is tracked, e.g. because the client only
-- ever connected to an already-running server.
killServerAction :: IORef (Maybe ServerInfo) -> BChan UI.Event -> IO ()
killServerAction serverInfoRef eventChan = do
  minfo <- readIORef serverInfoRef
  case minfo >>= (.process) of
    Just _ -> killTracked serverInfoRef
    Nothing ->
      writeBChan eventChan $
        UI.ProcessLog (Text.pack "error") (Text.pack "No tracked ghc-server process to kill (it wasn't started by this session)")

-- | Backs the capital-@R@ key binding: re-runs 'startServer' with the same project path\/extra options previously
-- used to start the tracked server ('startServer' itself is responsible for killing the old one first, via
-- 'killTracked').
restartServerAction :: IORef (Maybe ServerInfo) -> Maybe FilePath -> BChan UI.Event -> IO ()
restartServerAction serverInfoRef serverExe eventChan = do
  minfo <- readIORef serverInfoRef
  case minfo of
    Just si -> uncurry (startServer serverInfoRef serverExe eventChan) si.serveArgs
    Nothing ->
      writeBChan eventChan $
        UI.ProcessLog (Text.pack "error") (Text.pack "No prior ghc-server info to restart from")

-- | Backs the capital-@C@ key binding: asks the tracked server to remove its @cache@\/@output@ directories via
-- the @Clean@ RPC (see 'ServeGhcServer.cleanGhcServer').
cleanServerAction :: IORef (Maybe ServerInfo) -> BChan UI.Event -> IO ()
cleanServerAction serverInfoRef eventChan = do
  minfo <- readIORef serverInfoRef
  case minfo of
    Just si -> do
      result <- cleanGhcServer si.projectRoot
      writeBChan eventChan $ UI.ProcessLog (Text.pack "clean") (Text.pack (either ("Clean failed: " ++) ("Clean: " ++) result))
    Nothing ->
      writeBChan eventChan $
        UI.ProcessLog (Text.pack "error") (Text.pack "No tracked ghc-server project root to clean")

-- | The default exit-time finalizer, run via 'finally' around the whole UI session so it fires even when the app
-- exits via an exception, not just a clean @q@\/Esc quit. Disabled entirely by @--remain@. Cleans the tracked
-- server's @cache@\/@output@ directories (via the @Clean@ RPC, which requires the server to still be reachable --
-- hence running this /before/ killing it) and then kills the tracked process (if any) -- a no-op if this session
-- never started a server itself (see 'ServerInfo').
finalizeServer :: Options -> IORef (Maybe ServerInfo) -> BChan UI.Event -> IO ()
finalizeServer opts serverInfoRef eventChan =
  unless opts.remain $ do
    cleanServerAction serverInfoRef eventChan
    killServerAction serverInfoRef eventChan

-- | Read lines from a spawned ghc-server's stdout\/stderr handle until it closes (EOF or the process exits),
-- dispatching each as a 'UI.ProcessLog' event tagged with the given stream name. When given an accumulator
-- (used for stderr, see 'spawnedServer'), each line is also prepended to it, so the full text is available for
-- 'UI.ServerFailed' if the process later exits with a failure code. Runs in its own thread so the two streams
-- (stdout, stderr) can be read concurrently without blocking each other.
streamLines :: BChan UI.Event -> Text.Text -> Handle -> Maybe (IORef [Text.Text]) -> IO ()
streamLines eventChan streamName h macc =
  catch @SomeException loop (const (pure ()))
  where
    loop = do
      eof <- IO.hIsEOF h
      if eof
        then pure ()
        else do
          line <- Text.pack <$> IO.hGetLine h
          for_ macc (`modifyIORef'` (line :))
          writeBChan eventChan (UI.ProcessLog streamName line)
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
  serverInfoRef <- newIORef Nothing
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

    (_, vty) <-
      UI.customMainWithDefaultVty (Just eventChan) UI.app
        ( UI.initialState
            (startServer serverInfoRef opts.serverExe eventChan)
            (killServerAction serverInfoRef eventChan)
            (restartServerAction serverInfoRef opts.serverExe eventChan)
            (cleanServerAction serverInfoRef eventChan)
        )
        `finally` finalizeServer opts serverInfoRef eventChan
    vty.shutdown
