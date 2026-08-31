module Main where

import Brick.BChan (BChan, newBChan, writeBChan, writeBChanNonBlocking)
import BuckWorkerProto (Instrument)
import Control.Applicative ((<|>))
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Async (Async, async, cancel)
import Control.Exception (IOException, SomeException, catch, displayException, finally, try)
import Control.Monad (filterM, forever, unless, void, when)
import Data.Binary (decode)
import Data.ByteString (fromStrict)
import Data.Foldable (for_)
import Data.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef, writeIORef)
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

-- | Dispatch an event to the UI's event channel without ever blocking. The channel is bounded ('newBChan 10'),
-- and once the Brick main loop has halted (e.g. the user quit while background threads -- the periodic
-- 'SetTime' ticker, 'listen', 'streamLines', the exit-time finalizer's own log messages -- are still producing
-- events), nothing drains it any more. A plain blocking 'writeBChan' call made at that point (in particular from
-- 'finalizeServer', which runs on the main thread after the Brick loop returns) would never return, since the
-- channel fills up within about a second of ticks alone -- this was the root cause of the UI hanging indefinitely
-- on quit. Dropping an event under backpressure (the 'False' case, silently ignored) is an acceptable trade-off:
-- these are all best-effort UI notifications, not events anything relies on for correctness.
-- | Best-effort dispatch: drops the event under backpressure instead of blocking. Reserved for events that are
-- either recoverable\/idempotent (the periodic 'UI.SetTime' ticker, 'UI.ProcessLog' diagnostics) or dispatched from
-- code paths that may run on the main thread /after/ the Brick loop has already halted (the exit-time finalizer,
-- see 'finalizeServer') -- a blocking write there would hang the whole process, since nothing drains the channel
-- any more once Brick has returned. See 'dispatch' for the counterpart used by session\/build-lifecycle events,
-- which must never be silently dropped.
notify :: BChan UI.Event -> UI.Event -> IO ()
notify eventChan = void . writeBChanNonBlocking eventChan

-- | Blocking dispatch for session\/build-lifecycle events (worker connect\/disconnect, and every event forwarded
-- from a 'notifyMe' gRPC stream, including 'CompileStart'\/'CompileEnd'). These carry state transitions the UI
-- has no way to recover if dropped (e.g. a dropped 'CompileEnd' leaves a task showing as running forever, or a
-- dropped 'ProjectStructure'\/metadata event never appears at all) -- unlike 'notify', silently discarding them
-- under backpressure is not an acceptable trade-off. This is safe to block on: 'dispatch' is only ever called
-- from 'listen'\'s forked stream-reading thread, never from the main thread, so blocking here cannot reproduce the
-- quit-time hang that motivated 'notify' -- when the RTS's main thread exits, any forked thread blocked on this
-- call is simply killed along with it.
dispatch :: BChan UI.Event -> UI.Event -> IO ()
dispatch = writeBChan

listen :: BChan UI.Event -> FilePath -> IO ()
listen eventChan instrPath = do
  void $ forkIO $ go 5
 where
  -- TODO: This is a hack, ids should be sent over grpc
  (sessionId', workerId') = break (== '_') instrPath
  sessionId = Session.Id $ Text.pack sessionId'
  workerId = WorkerId $ Text.pack workerId'
  go :: Int -> IO ()
  go 0 = dispatch eventChan $ UI.SessionSelectorEvent $ SS.RemoveWorker sessionId workerId
  go n =
    catch @SomeException
      ( withConnection def (ServerUnix instrPath) $ \conn -> do
          serverStreaming conn (rpc @(Protobuf Instrument "notifyMe")) defMessage $ \recv -> do
            time <- getModificationTime instrPath
            dispatch eventChan $ UI.SessionSelectorEvent $ SS.AddWorker sessionId workerId time conn
            dispatch eventChan (UI.SendOptions (Just workerId))
            whileNext_ recv
              $ dispatch eventChan
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
        Left err -> notify eventChan $ UI.ServerFailed path (Text.pack (displayException err))
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
      notify eventChan $ UI.ServerFailed path (Text.unlines captured)

-- | Builds the closure passed to 'UI.initialState' that backs the capital-@S@ key binding (and, indirectly, the
-- @R@ restart binding): dispatches 'UI.ServerStarting' immediately so the UI can replace its placeholder message,
-- then forks a background thread ('runStartup') and returns without waiting for it. That thread first kills any
-- previously tracked process\/poll thread (see 'killTracked'), then either connects to an already-running server
-- or spawns a new one, storing its handles in 'ServerInfo', starting an indefinite 'isServerUp' poll thread
-- (also tracked), and finally blocking on the subprocess's termination (see 'spawnedServer') -- there is
-- deliberately no timeout anywhere in this path.
startServer :: IORef (Maybe ServerInfo) -> Maybe FilePath -> BChan UI.Event -> Text.Text -> [String] -> IO ()
startServer serverInfoRef serverExe eventChan path extraOpts = do
  notify eventChan (UI.ServerStarting path)
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
      notify eventChan $
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
      notify eventChan $
        UI.ProcessLog (Text.pack "error") (Text.pack "No prior ghc-server info to restart from")

-- | Backs the capital-@C@ key binding: asks the tracked server to remove its @cache@\/@output@ directories for
-- the given scope via the @Clean@ RPC (see 'ServeGhcServer.cleanGhcServer'). @target@ is the clean scope
-- computed by 'UI.TaskTree.selectedCleanTarget': the sentinel @"*"@ for the whole project, a bare unit name, or
-- @unitName:moduleName@.
cleanServerAction :: IORef (Maybe ServerInfo) -> BChan UI.Event -> Text.Text -> IO ()
cleanServerAction serverInfoRef eventChan target = do
  minfo <- readIORef serverInfoRef
  case minfo of
    Just si -> do
      result <- cleanGhcServer si.projectRoot target
      notify eventChan $ UI.ProcessLog (Text.pack "clean") (Text.pack (either ("Clean failed: " ++) ("Clean: " ++) result))
      notify eventChan $ UI.CleanCompleted target (either (const False) (const True) result)
    Nothing ->
      notify eventChan $
        UI.ProcessLog (Text.pack "error") (Text.pack "No tracked ghc-server project root to clean")

-- | The clean\/kill shutdown steps shared by 'finalizeServer' (the exception-safety-net path, which may run
-- after Brick's event loop has already halted) and 'requestShutdown' (the live, quit-key-triggered path, which
-- runs while Brick is still pumping events). Parameterized over the event-sending function so each caller can
-- pick the semantics appropriate to when it runs: non-blocking 'notify' (safe post-halt, may silently drop) for
-- 'finalizeServer', blocking 'dispatch' (guaranteed delivery, but only safe while something is still draining
-- the channel) for 'requestShutdown'. A no-op when @--remain@ was passed.
runSteps :: (UI.Event -> IO ()) -> Options -> IORef (Maybe ServerInfo) -> BChan UI.Event -> IO ()
runSteps send opts serverInfoRef eventChan =
  unless opts.remain $ do
    send (UI.OpLogMessage (Text.pack "Cleaning ghc-server cache/output directories"))
    cleanServerAction serverInfoRef eventChan (Text.pack "*")
    send (UI.OpLogMessage (Text.pack "Killing ghc-server"))
    killServerAction serverInfoRef eventChan

-- | The default exit-time finalizer, run via 'finally' around the whole UI session so it fires even when the app
-- exits via an exception, not just a clean @q@\/Esc quit. Cleans the tracked server's @cache@\/@output@
-- directories and kills the tracked process, if any (see 'runSteps') -- unless 'requestShutdown' (the quit-key
-- path) already did so, as tracked by 'finalizedRef', in which case this is a no-op: it exists purely as a
-- safety net for abnormal exits (exceptions, signals) that bypass the quit key entirely. Uses non-blocking
-- 'notify' throughout, since this can run after Brick's event loop has already halted, in which case nothing
-- would ever drain a blocking 'dispatch'.
finalizeServer :: Options -> IORef Bool -> IORef (Maybe ServerInfo) -> BChan UI.Event -> IO ()
finalizeServer opts finalizedRef serverInfoRef eventChan = do
  already <- readIORef finalizedRef
  unless already $ runSteps (notify eventChan) opts serverInfoRef eventChan

-- | Backs the quit key's '_shutdown' closure (see 'UI.requestQuit'): runs while Brick's event loop is still
-- alive (the quit handler cleared the session list and dispatched a "Shutting down" message but did /not/ call
-- 'halt'), so the clean\/kill steps (see 'runSteps') can use blocking 'dispatch' and have their progress genuinely
-- rendered live -- if shutdown hangs, the last dispatched message stays visible on screen instead of the app
-- just freezing silently. Forks its own thread so the UI's event loop keeps responding\/redrawing while this
-- runs. Sets 'finalizedRef' first (guarding against a second quit-key press re-running this concurrently) so
-- 'finalizeServer's fallback path skips redundant cleanup afterwards. Dispatches 'UI.ShutdownComplete' once done,
-- which is what actually triggers 'halt'.
requestShutdown :: Options -> IORef Bool -> IORef (Maybe ServerInfo) -> BChan UI.Event -> IO ()
requestShutdown opts finalizedRef serverInfoRef eventChan = do
  alreadyShuttingDown <- atomicModifyIORef' finalizedRef (\old -> (True, old))
  unless alreadyShuttingDown $ void $ forkIO $ do
    runSteps (dispatch eventChan) opts serverInfoRef eventChan
    dispatch eventChan (UI.OpLogMessage (Text.pack "Shutdown complete"))
    dispatch eventChan UI.ShutdownComplete

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
          notify eventChan (UI.ProcessLog streamName line)
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
  -- Sized generously beyond the old default of 10: 'dispatch' (used for session\/build events, see above) blocks
  -- until space is available rather than dropping, so a larger buffer reduces how often a burst of build
  -- traffic (metadata\/compile start\/end, log messages) has to wait on the best-effort ticker\/diagnostic traffic
  -- ('notify') being drained first.
  eventChan <- newBChan 1000
  serverInfoRef <- newIORef Nothing
  -- Guards against 'finalizeServer' (the exception safety net) redundantly re-running the clean\/kill steps
  -- after 'requestShutdown' (the quit-key path) already did, and against a second quit-key press re-triggering
  -- 'requestShutdown' while the first invocation is still running.
  finalizedRef <- newIORef False
  _ <- forkIO $ forever $ do
    time <- getCurrentTime
    notify eventChan (UI.SetTime time)
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
            (requestShutdown opts finalizedRef serverInfoRef eventChan)
        )
        `finally` finalizeServer opts finalizedRef serverInfoRef eventChan
    vty.shutdown
