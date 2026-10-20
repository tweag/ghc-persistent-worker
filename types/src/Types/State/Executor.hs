-- | The persistent-executor subprocess handle stored in 'Types.State.WorkerState.executors', keyed by
-- 'Types.Api.ExecutorId'. See @kb-process-isolation@ for the rationale behind running execute tasks out of
-- process, and 'GhcServer.Build.Executor' for the parent-side spawn\/dispatch\/terminate logic.
module Types.State.Executor where

import Control.Concurrent.QSemN (QSemN, newQSemN)
import System.IO (Handle)
import System.OsPath (OsPath)
import System.Process (ProcessHandle)
import Types.Api (ProcessStats)

-- | The number of calls that can hold an executor's 'ExecutorHandle.calls' at the same time.
maxConcurrentCalls :: Int
maxConcurrentCalls = 64

newCallLock :: IO QSemN
newCallLock = newQSemN maxConcurrentCalls

-- | A running persistent executor child process: its OS process handle (for termination), the Unix socket
-- path on which it serves the @Executor@ gRPC service, and the write end of its stdin pipe. The child exits when
-- its stdin reaches EOF, so the handle must be kept alive (and is closed on termination).
data ExecutorHandle =
  ExecutorHandle {
    process :: ProcessHandle,
    socket :: OsPath,
    stdin :: Handle,
    -- | RTS stats the executor reported with its most recent result.
    stats :: Maybe ProcessStats,
    -- | Lets calls run concurrently while keeping a respawn from stopping the process during a call, see
    -- 'withCall' and 'withoutCalls'.
    -- Shared by all handles registered for the same 'Types.Api.ExecutorId', including respawned ones.
    calls :: QSemN,
    -- | Counts the processes spawned for the same 'Types.Api.ExecutorId' before this one, so that a respawn can tell
    -- whether the process that failed has already been replaced.
    incarnation :: Int
  }
