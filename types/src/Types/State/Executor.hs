-- | The persistent-executor subprocess handle stored in 'Types.State.WorkerState.executors', keyed by
-- 'Types.Api.ExecutorId'. See @kb-process-isolation@ for the rationale behind running execute tasks out of
-- process, and 'GhcServer.Build.Executor' for the parent-side spawn\/dispatch\/terminate logic.
module Types.State.Executor where

import Control.Concurrent.QSemN (QSemN, newQSemN, signalQSemN, waitQSemN)
import Control.Exception (bracket_)
import System.IO (Handle)
import System.OsPath (OsPath)
import System.Process (ProcessHandle)
import Types.Api (ProcessStats)

-- | The number of calls that can hold an executor's 'ExecutorHandle.calls' at the same time.
maxConcurrentCalls :: Int
maxConcurrentCalls = 64

newCallLock :: IO QSemN
newCallLock = newQSemN maxConcurrentCalls

-- | Run a call to the executor, concurrently with other calls, but not while 'withoutCalls' runs.
withCall :: ExecutorHandle -> IO a -> IO a
withCall handle =
  bracket_ (waitQSemN handle.calls 1) (signalQSemN handle.calls 1)

-- | Run an action while no call is in flight and none is started, e.g. to replace the process.
withoutCalls :: ExecutorHandle -> IO a -> IO a
withoutCalls handle =
  bracket_ (waitQSemN handle.calls maxConcurrentCalls) (signalQSemN handle.calls maxConcurrentCalls)

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
