-- | Parent-side lifecycle management for persistent, gRPC-addressable executor subprocesses (see
-- 'Types.State.WorkerState.executors' and 'Types.Api.ExecutorId').
--
-- __Known limitation__: this module does not yet implement the actual persistent gRPC subprocess (spawning a
-- long-lived @ghc-server executor@ child listening on its own Unix socket, dialing it as a gRPC client per
-- execute task). That requires a new @executor.proto@ service, generated stubs, and a new 'GhcServer.Run'
-- server mode, none of which exist yet. As a placeholder that keeps the public API
-- ('ensureExecutor'\/'executeModuleTaskExecutor'\/'terminateExecutor') stable and the rest of the codebase
-- (scheduler task values, the @TerminateExecutor@ API request, 'GhcServer.Grpc') wired up correctly,
-- 'executeModuleTaskExecutor' currently delegates to the existing one-shot 'GhcServer.Build.Process.executeModuleTaskProcess'
-- (a fresh subprocess per call, not reused across calls), and 'ensureExecutor' never actually populates
-- 'Types.State.WorkerState.executors' with a running child. Consequently 'terminateExecutor' is always a
-- successful no-op (there is never a tracked process to kill). See the task's final report for what remains to
-- turn this into the real persistent design.
module GhcServer.Build.Executor where

import Control.Concurrent.MVar (modifyMVar_, readMVar)
import qualified Data.Map.Strict as Map
import qualified GHC
import GhcServer.Build.Process (executeModuleTaskProcess)
import GhcServer.Data.BuildEnv (BuildEnv (..))
import GhcServer.Data.Unit (Unit)
import System.Process (terminateProcess)
import Test.Scheduler (TaskResult)
import Types.Api (ExecutorId, ProcessStats)
import Types.State (WorkerState (..))
import Types.State.Executor (ExecutorHandle (..))

-- | Ensure a persistent executor subprocess is running for the given id, spawning one if necessary, and return
-- its socket path.
--
-- __Not yet implemented as a real persistent subprocess__ -- see the module haddock. Currently a no-op that
-- never registers anything in 'WorkerState.executors'.
ensureExecutor :: BuildEnv -> ExecutorId -> IO (Either String ())
ensureExecutor _env _executorId =
  pure (Right ())

-- | Run a module's execute task via the persistent executor identified by 'ExecutorId'.
--
-- __Not yet implemented as a real persistent subprocess__ -- see the module haddock. Currently delegates to
-- the existing one-shot 'executeModuleTaskProcess', which spawns and tears down a fresh child for every call
-- instead of reusing a long-lived one.
executeModuleTaskExecutor :: BuildEnv -> ExecutorId -> Unit -> GHC.ModuleName -> IO (Maybe (TaskResult String), Maybe ProcessStats)
executeModuleTaskExecutor env _executorId unit modName =
  executeModuleTaskProcess env unit modName

-- | Terminate the persistent executor subprocess identified by 'ExecutorId', if one is running, and remove it
-- from 'WorkerState.executors'. Returns whether an executor with that id was actually running.
--
-- Always returns 'False' for now -- see the module haddock; no executor is ever registered by 'ensureExecutor'.
terminateExecutor :: BuildEnv -> ExecutorId -> IO Bool
terminateExecutor env executorId = do
  state <- readMVar env.stateVar
  case Map.lookup executorId state.executors of
    Nothing -> pure False
    Just handle -> do
      terminateProcess handle.process
      modifyMVar_ env.stateVar \ s -> pure s {executors = Map.delete executorId s.executors}
      pure True
