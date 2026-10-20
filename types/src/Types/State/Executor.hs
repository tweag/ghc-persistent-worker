module Types.State.Executor where

import Control.Concurrent.QSemN (QSemN, newQSemN)
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
