module Types.State.Executor where

import Control.Concurrent.QSemN (QSemN, newQSemN, signalQSemN, waitQSemN)
import Control.Exception (bracket_)
import System.IO (Handle)
import System.OsPath (OsPath)
import System.Process (ProcessHandle)
import Types.Api (MemoryUsage)

-- | The number of calls that can hold an executor's 'ExecutorHandle.calls' at the same time.
--
-- TODO unclear, reword later
maxConcurrentCalls :: Int
maxConcurrentCalls = 64

newCallLock :: IO QSemN
newCallLock = newQSemN maxConcurrentCalls

-- | Run a call to the executor, concurrently with other calls, but not while 'withoutCalls' runs.
--
-- TODO unclear, reword later
withCall :: ExecutorHandle -> IO a -> IO a
withCall handle =
  bracket_ (waitQSemN handle.calls 1) (signalQSemN handle.calls 1)

-- | Run an action while no call is in flight and none is started, e.g. to replace the process.
withoutCalls :: ExecutorHandle -> IO a -> IO a
withoutCalls handle =
  bracket_ (waitQSemN handle.calls maxConcurrentCalls) (signalQSemN handle.calls maxConcurrentCalls)

-- | Resources and state for an executor process.
--
-- TODO split in resources and state?
data ExecutorHandle =
  ExecutorHandle {
    process :: ProcessHandle,
    socket :: OsPath,
    stdin :: Handle,
    -- | Last reported memory usage.
    --
    -- TODO should this not be the maxiumum?
    memory :: Maybe MemoryUsage,
    -- | Lets calls run concurrently while keeping a respawn from stopping the process during a call, see
    -- 'withCall' and 'withoutCalls'.
    -- Shared by all handles registered for the same 'Types.Api.ExecutorId', including respawned ones.
    --
    -- TODO unclear, reword later
    calls :: QSemN,
    -- | Counts the processes spawned for the same 'Types.Api.ExecutorId' before this one, so that a respawn can tell
    -- whether the process that failed has already been replaced.
    --
    -- TODO unclear, reword later
    incarnation :: Int
  }
