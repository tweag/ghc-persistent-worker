-- | Support for starting a @ghc-server@ instance on demand from the instrument UI (the capital-@S@ key binding, and
-- the @K@\/@R@ lifecycle keys). This module only exposes the low-level primitives (socket probing, process
-- spawning\/killing, cache cleaning); the orchestration -- checking for an existing server, spawning one, polling
-- for it to come up, and waiting for it to terminate -- lives in @Main@, since it requires background threads and
-- app-state bookkeeping that this module has no business owning.
module ServeGhcServer (resolveServerExe, isServerUp, defaultSocketPath, spawnGhcServer, killGhcServer, cleanGhcServer) where

import BuckWorkerProto ()
import Control.Exception (SomeException, bracket, catch, displayException, try)
import Data.Foldable (for_)
import Data.Functor (void)
import Data.Text qualified as Text
import Network.GRPC.Client (Server (ServerUnix), recvNextOutput, sendFinalInput, withConnection, withRPC)
import Network.GRPC.Common (Proxy (..), def)
import Network.GRPC.Common.Protobuf (Protobuf, defMessage)
import Network.Socket (Family (AF_UNIX), SockAddr (SockAddrUnix), SocketType (Stream), close, connect, socket)
import Proto.GhcServer (GhcServer)
import System.Directory (findExecutable)
import System.Exit (die)
import System.IO (Handle)
import System.Posix.Signals (sigKILL, signalProcessGroup)
import System.Process (
  CreateProcess (..),
  ProcessHandle,
  StdStream (..),
  createProcess,
  getPid,
  interruptProcessGroupOf,
  proc,
  waitForProcess,
  )
import System.Timeout (timeout)

-- | Check whether something is listening on the given Unix socket, by attempting an actual @connect(2)@.
--
-- __Note__: this cannot use a lazy gRPC client (e.g. 'Network.GRPC.Client.withConnection') as a substitute, because
-- grapesy's connections are established asynchronously on first use, not by 'openConnection' itself — so a
-- "successful" 'withConnection' says nothing about whether a listener actually exists at the given path.
isServerUp :: FilePath -> IO Bool
isServerUp sock =
  catch @SomeException
    (bracket (socket AF_UNIX Stream 0) close (\s -> connect s (SockAddrUnix sock)) >> pure True)
    (const (pure False))

-- | The default Instrument socket path for a given project root, i.e. @PROJECT_ROOT/socket/instrument@.
defaultSocketPath :: FilePath -> FilePath
defaultSocketPath projectRoot = projectRoot ++ "/socket/instrument"

-- | Spawn @ghc-server@ as a detached daemon process rooted at the given project directory, with the @instrument@
-- and @lazy-byte-code@ features enabled -- the former opens an Instrument gRPC socket, the latter is required for
-- the worker to retain any bytecode at all (see 'Types.FeatureFlags.lazyByteCode'), without which the
-- @instrument@ UI's bytecode-cache browser panel and its eviction key binding have nothing to ever display or
-- unload regardless of what gets executed -- plus any extra CLI options given by the caller (e.g. from the UI's
-- start-server popup, which can still override/extend this via @extraArgs@, e.g. @--max-bytecode@ to also bound
-- the cache and enable LRU eviction). Returns the process's stdout\/stderr read handles (piped rather than
-- inherited), so the caller can stream the server's real output into its own logging instead of leaving it
-- invisible (a detached, @new_session@'d process has no attached terminal of its own).
spawnGhcServer :: FilePath -> FilePath -> [String] -> IO (Handle, Handle, ProcessHandle)
spawnGhcServer exe projectRoot extraArgs = do
  (_, mout, merr, ph) <-
    createProcess
      (proc exe (projectRoot : "--enable" : "instrument" : "--enable" : "lazy-byte-code" : "--max-bytecode" : "50" : extraArgs))
        { std_in = NoStream
        , std_out = CreatePipe
        , std_err = CreatePipe
        , new_session = True
        }
  case (mout, merr) of
    (Just out, Just err) -> pure (out, err, ph)
    _ -> die "ghc-server: expected stdout/stderr pipes from createProcess, got Nothing"

-- | Resolve the @ghc-server@ executable to use: the explicit path given via @--server-exe@ if present, otherwise a
-- @PATH@ lookup.
resolveServerExe :: Maybe FilePath -> IO FilePath
resolveServerExe = \case
  Just exe -> pure exe
  Nothing -> do
    mExe <- findExecutable "ghc-server"
    maybe (die "ghc-server executable not found in PATH; pass --server-exe or ensure it's on PATH") pure mExe

-- | How long to wait for a graceful shutdown (via 'interruptProcessGroupOf', i.e. @SIGINT@) before escalating to
-- @SIGKILL@ in 'killGhcServer'. @ghc-server@ can legitimately spend a long time inside an uninterruptible GHC
-- foreign call while compiling, during which @SIGINT@ is queued but not delivered -- without an escalation, a
-- client quitting mid-build would block indefinitely on 'waitForProcess'.
gracefulShutdownTimeoutMicros :: Int
gracefulShutdownTimeoutMicros = 5_000_000

-- | Kill a @ghc-server@ process previously spawned by 'spawnGhcServer'. It was started with @new_session = True@,
-- making it the leader of its own process group, so interrupting the whole group (rather than just
-- 'System.Process.terminateProcess' on the single tracked PID) also reaches any subprocesses it may have spawned
-- itself (e.g. via the in-process Cabal external-dependency build, which could in principle shell out).
--
-- Tries a graceful @SIGINT@ first, waiting up to 'gracefulShutdownTimeoutMicros' for the process to exit on its
-- own. If it hasn't exited within that window (e.g. it's stuck inside a long-running or uninterruptible GHC
-- compile), escalates to @SIGKILL@ on the whole process group, which the kernel delivers unconditionally --
-- guaranteeing this function always returns instead of blocking forever. Always blocks until the process has
-- actually exited.
killGhcServer :: ProcessHandle -> IO ()
killGhcServer ph = do
  interruptProcessGroupOf ph
  exited <- timeout gracefulShutdownTimeoutMicros (waitForProcess ph)
  case exited of
    Just _ -> pure ()
    Nothing -> do
      mpid <- getPid ph
      for_ mpid (signalProcessGroup sigKILL)
      void $ waitForProcess ph

-- | Ask a project's running @ghc-server@ to remove its own @output@\/@cache@ directories, via the @Clean@ RPC on
-- the @ghc-server.proto@-defined @GhcServer@ service (see 'kb-grpc'). The server -- not this client -- knows and
-- owns the actual paths, so this no longer duplicates that assumption locally (unlike the previous version of this
-- function, which deleted @<projectRoot>/cache@\/@<projectRoot>/output@ directly). Requires the server to still be
-- reachable: call this /before/, not after, killing a server that's about to be torn down. Returns @Left@ with a
-- human-readable reason on any failure (connection failure, or the RPC itself reporting @success = False@);
-- @Right@ with the server's reported message otherwise.
--
-- Bounded by 'gracefulShutdownTimeoutMicros': if the server is busy (e.g. mid-build) and doesn't respond within
-- that window, returns @Left@ instead of blocking indefinitely -- callers that need to tear the server down
-- afterwards (e.g. the exit-time finalizer) must not be stalled by an unresponsive @Clean@ RPC.
cleanGhcServer :: FilePath -> IO (Either String String)
cleanGhcServer projectRoot = do
  eres <- try @SomeException (timeout gracefulShutdownTimeoutMicros rpcCall)
  pure $ case eres of
    Left e -> Left (displayException e)
    Right Nothing -> Left "Clean request timed out (server unresponsive)"
    Right (Just resp)
      | resp.success -> Right (Text.unpack resp.message)
      | otherwise -> Left (Text.unpack resp.message)
  where
    sock = projectRoot ++ "/socket/server.sock"

    rpcCall =
      withConnection def (ServerUnix sock) \ conn ->
        withRPC conn def (Proxy @(Protobuf GhcServer "clean")) \ call -> do
          sendFinalInput call defMessage
          recvNextOutput call
