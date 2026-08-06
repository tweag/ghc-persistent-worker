-- | Support for the @--serve@ CLI option: ensures a @ghc-server@ instance is running for the current directory,
-- spawning one as a detached daemon subprocess if necessary. The executable can be looked up on @PATH@ or given
-- explicitly (via @--server-exe@), see 'ensureGhcServer'.
module ServeGhcServer (ensureGhcServer) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, bracket, catch)
import Network.Socket (Family (AF_UNIX), SockAddr (SockAddrUnix), SocketType (Stream), close, connect, socket)
import System.Directory (findExecutable, getCurrentDirectory)
import System.Exit (die)
import System.Process (CreateProcess (..), StdStream (..), createProcess, proc)

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

-- | Poll the socket until the server responds, retrying up to @n@ times with a 100ms delay (matches the retry budget
-- used elsewhere in this codebase, e.g. 'Common.Grpc.waitPoll').
waitForServer :: FilePath -> Int -> IO ()
waitForServer sock n
  | n <= 0 = die "ghc-server didn't become responsive within the expected time"
  | otherwise = do
      up <- isServerUp sock
      if up
        then pure ()
        else threadDelay 100_000 >> waitForServer sock (n - 1)

-- | Spawn @ghc-server@ as a detached daemon process rooted at the given project directory, with the @instrument@
-- feature enabled so it opens an Instrument gRPC socket.
spawnGhcServer :: FilePath -> FilePath -> IO ()
spawnGhcServer exe projectRoot = do
  _ <-
    createProcess
      (proc exe [projectRoot, "--enable", "instrument", "--cabal"])
        { std_in = NoStream
        , std_out = CreatePipe
        , std_err = CreatePipe
        , new_session = True
        }
  pure ()

-- | Resolve the @ghc-server@ executable to use: the explicit path given via @--server-exe@ if present, otherwise a
-- @PATH@ lookup.
resolveServerExe :: Maybe FilePath -> IO FilePath
resolveServerExe = \case
  Just exe -> pure exe
  Nothing -> do
    mExe <- findExecutable "ghc-server"
    maybe (die "ghc-server executable not found in PATH; pass --server-exe or ensure it's on PATH") pure mExe

-- | Ensure a @ghc-server@ instance is running for the current directory, starting one as a daemon subprocess if none
-- is listening yet. Returns the path to its Instrument gRPC socket.
--
-- If @serverExe@ is @Nothing@, the executable is looked up on @PATH@ (see 'resolveServerExe').
ensureGhcServer :: Maybe FilePath -> IO FilePath
ensureGhcServer serverExe = do
  projectRoot <- getCurrentDirectory
  let sock = projectRoot ++ "/socket/instrument"
  up <- isServerUp sock
  if up
    then pure sock
    else do
      exe <- resolveServerExe serverExe
      spawnGhcServer exe projectRoot
      waitForServer sock 30
      pure sock
