-- | Parent-side primitives for persistent, gRPC-addressable executor subprocesses (see
-- 'Types.State.WorkerState.executors'): spawning the relaunch of an executable in @executor@ mode, waiting for its
-- socket, sending calls and stopping it.
--
-- Used by the @ghc-server@ ('GhcServer.Build.Executor') and by the worker's eval mode ('GhcWorker.Eval.Executor').
module Internal.Executor.Client where

-- | Wrap RTS options in @+RTS ... -RTS@, or nothing if there are none.
rtsArgs :: [String] -> [String]
rtsArgs = \case
  [] -> []
  opts -> ["+RTS"] ++ opts ++ ["-RTS"]

-- | Argument that makes an executor attribute its stdout and stderr to the calls, see
-- 'Types.Settings.Settings.executorCaptureStdio'.
captureStdioArg :: String
captureStdioArg = "--capture-stdio"
