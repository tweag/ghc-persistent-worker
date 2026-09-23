-- | The persistent-executor subprocess handle stored in 'Types.State.WorkerState.executors', keyed by
-- 'Types.Api.ExecutorId'. See @kb-process-isolation@ for the rationale behind running execute tasks out of
-- process, and 'GhcServer.Build.Executor' for the parent-side spawn\/dispatch\/terminate logic.
module Types.State.Executor where

import System.OsPath (OsPath)
import System.Process (ProcessHandle)

-- | A running persistent executor child process: its OS process handle (for termination) and the Unix socket
-- path on which it serves the @Executor@ gRPC service.
data ExecutorHandle =
  ExecutorHandle {
    process :: ProcessHandle,
    socket :: OsPath
  }
