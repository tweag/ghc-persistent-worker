{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeApplications #-}
-- | Feasibility experiment for the multi-process execution redesign.
--
-- Forks a handful of child processes with 'forkProcess', exchanges data with
-- each of them over pipes, and has them write into a POSIX @mmap@ region that
-- is shared (via @MAP_SHARED@) between parent and children. This exercises
-- the two IPC primitives ("send data back and forth" and "share a block of
-- memory") that a real process-per-handler architecture would need for
-- something like 'WorkerState', without touching any of the actual GHC
-- session/state machinery.
--
-- This test is Linux-specific (hardcoded @mmap@/@MAP_ANONYMOUS@ flag values)
-- and must run single-capability; see the module-level caveat below and the
-- @kb-standalone-server@/@kb-state@ notes for context.
module Test.ForkTest where

import Control.Exception (SomeException, bracket, onException, try)
import Control.Monad (forM, forM_)
import Control.Monad.IO.Class (liftIO)
import Data.Int (Int64)
import Foreign.C.Error (throwErrnoIfMinus1, throwErrnoIfMinus1_)
import Foreign.C.Types (CInt (..), CSize (..))
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.Storable (peekElemOff, pokeElemOff)
import GHC.Conc (getNumCapabilities, setNumCapabilities)
import Hedgehog (TestT, annotate, property, test, withTests, (===))
import System.Posix.IO (closeFd, createPipe, fdToHandle)
import System.Posix.Process (ProcessStatus (..), forkProcess, getProcessStatus)
import System.Posix.Signals (sigKILL, signalProcess)
import System.Posix.Types (Fd, ProcessID)
import System.IO (BufferMode (LineBuffering), hClose, hFlush, hGetLine, hPutStrLn, hSetBuffering)
import System.Exit (ExitCode (ExitSuccess))
import Test.Tasty (TestName, TestTree)
import Test.Tasty.Hedgehog (testProperty)

-- ---------------------------------------------------------------------------
-- Raw @mmap@ FFI
--
-- 'unix' has no wrapper for @mmap@/@shm_open@, so this binds the two syscalls
-- directly. An anonymous 'MAP_SHARED' mapping is sufficient here: it is
-- created before 'forkProcess', and the mapping (not just its contents) is
-- inherited by the child, so no 'shm_open'/named object is needed to get a
-- handle shared across the fork boundary.
-- ---------------------------------------------------------------------------

foreign import ccall unsafe "mmap"
  c_mmap :: Ptr () -> CSize -> CInt -> CInt -> CInt -> CSize -> IO (Ptr Int64)

foreign import ccall unsafe "munmap"
  c_munmap :: Ptr Int64 -> CSize -> IO CInt

-- | @PROT_READ | PROT_WRITE@ (Linux/x86_64 values).
protReadWrite :: CInt
protReadWrite = 0x1 + 0x2

-- | @MAP_SHARED | MAP_ANONYMOUS@ (Linux values; @MAP_ANONYMOUS@ is not POSIX).
mapSharedAnon :: CInt
mapSharedAnon = 0x01 + 0x20

-- | Number of 'Int64' slots in the shared region, one per worker.
workerCount :: Int
workerCount = 4

slotSize :: CSize
slotSize = fromIntegral (workerCount * 8)

mmapShared :: IO (Ptr Int64)
mmapShared = do
  ptr <- c_mmap nullPtr slotSize protReadWrite mapSharedAnon (-1) 0
  if ptr == castPtr nullPtr
    then throwErrnoIfMinus1 "mmap" (pure (-1 :: CInt)) >> pure ptr
    else pure ptr

munmapShared :: Ptr Int64 -> IO ()
munmapShared ptr =
  throwErrnoIfMinus1_ "munmap" (c_munmap ptr slotSize)

-- ---------------------------------------------------------------------------
-- Worker logic
-- ---------------------------------------------------------------------------

-- | Pipes connecting the parent to a single forked worker: a request pipe
-- (parent writes, child reads) and a response pipe (child writes, parent
-- reads).
data WorkerPipes = WorkerPipes {
  reqRead :: Fd,
  reqWrite :: Fd,
  respRead :: Fd,
  respWrite :: Fd
  }

newWorkerPipes :: IO WorkerPipes
newWorkerPipes = do
  (reqRead, reqWrite) <- createPipe
  (respRead, respWrite) <- createPipe
  pure WorkerPipes {reqRead, reqWrite, respRead, respWrite}

-- | Child action: read an 'Int' request from the parent, square it, publish
-- the result into its shared-memory slot, and report back over the response
-- pipe. Runs entirely in the forked child; nothing here touches parent-only
-- resources besides the fds explicitly passed via 'WorkerPipes' and the
-- shared mapping.
childMain :: Ptr Int64 -> Int -> WorkerPipes -> IO ()
childMain shm index pipes = do
  closeFd pipes.reqWrite
  closeFd pipes.respRead
  reqHdl <- fdToHandle pipes.reqRead
  hSetBuffering reqHdl LineBuffering
  n <- read @Int <$> hGetLine reqHdl
  let result = n * n
  pokeElemOff shm index (fromIntegral result)
  respHdl <- fdToHandle pipes.respWrite
  hSetBuffering respHdl LineBuffering
  hPutStrLn respHdl (show result)
  hFlush respHdl
  hClose respHdl

-- | Spawn one worker, send it @n@, and return its pid plus a handle to read
-- its response from. Closes the parent's copies of the child-only fds.
spawnWorker :: Ptr Int64 -> Int -> Int -> IO (ProcessID, WorkerPipes)
spawnWorker shm index n = do
  pipes <- newWorkerPipes
  pid <- forkProcess (childMain shm index pipes)
  closeFd pipes.reqRead
  closeFd pipes.respWrite
  reqHdl <- fdToHandle pipes.reqWrite
  hSetBuffering reqHdl LineBuffering
  hPutStrLn reqHdl (show n)
  hFlush reqHdl
  hClose reqHdl
  pure (pid, pipes)

readResponse :: WorkerPipes -> IO Int
readResponse pipes = do
  respHdl <- fdToHandle pipes.respRead
  hSetBuffering respHdl LineBuffering
  read <$> hGetLine respHdl

-- | Send @SIGKILL@ to every child and reap it, ignoring failures (a child
-- may already be dead/reaped by the time this runs). Used as the cleanup
-- path when something between spawning and reaping the children throws, so
-- a failing test never leaks zombie/orphan processes.
killAll :: [ProcessID] -> IO ()
killAll pids =
  forM_ pids $ \pid -> do
    _ <- try @SomeException (signalProcess sigKILL pid)
    _ <- try @SomeException (getProcessStatus True False pid)
    pure ()

-- ---------------------------------------------------------------------------
-- Test
-- ---------------------------------------------------------------------------

-- | Fork 'workerCount' children, have each compute @n * n@ for a distinct
-- @n@ both over a pipe (the "request/response" channel) and by writing into
-- a shared @mmap@ region (the "shared state" channel), then verify both
-- channels agree once every child has exited cleanly.
--
-- __Caveat__ (see report): 'forkProcess' is documented as unreliable under
-- @+RTS -N@ with more than one capability. The test suite's default RTS
-- options include @-N@; this test only demonstrates the primitive in
-- isolation and does not by itself prove safety for a multi-capability
-- worker process.
test_forkShared :: TestName -> TestTree
test_forkShared name =
  testProperty name (withTests 1 (property (test forkShared)))

-- | The forking/IPC experiment proper, run in plain 'IO' since it only
-- exercises 'bracket'-based resource cleanup around OS processes and has no
-- need for property-test generation.
forkSharedIO :: IO ([Int], [Maybe ProcessStatus], [Int64])
forkSharedIO =
  bracket withSingleCapability setNumCapabilities $ \_ ->
    bracket mmapShared munmapShared $ \shm -> do
      spawned <- forM [0 .. workerCount - 1] (\i -> spawnWorker shm i i)
      let pids = map fst spawned
      (do
        responses <- forM spawned (\(_, pipes) -> readResponse pipes)
        statuses <- forM pids (\pid -> getProcessStatus True False pid)
        shared <- forM [0 .. workerCount - 1] (peekElemOff shm)
        pure (responses, statuses, shared)) `onException` killAll pids

forkShared :: TestT IO ()
forkShared = do
  (responses, statuses, shared) <- liftIO forkSharedIO
  let expected = [i * i | i <- [0 .. workerCount - 1]]
  annotate (show (responses, fmap (fmap show) statuses, shared))
  responses === expected
  fmap fromIntegral shared === expected
  forM_ statuses (=== Just (Exited ExitSuccess))


-- | Drop the RTS to a single capability for the duration of the fork
-- experiment and return the previous capability count so it can be restored.
-- Per 'forkProcess''s own documentation, forking is "not currently very well
-- supported when using multiple capabilities (+RTS -N)"; this test suite
-- runs with @-N@ by default, so the capability count is narrowed here rather
-- than relying on the process-wide RTS flags.
withSingleCapability :: IO Int
withSingleCapability = do
  n <- getNumCapabilities
  setNumCapabilities 1
  pure n

