-- | The child side of a persistent executor subprocess of the @ghc-server@ (see 'GhcServer.Build.Executor' for the
-- parent side).
--
-- Unlike the one-shot child in 'GhcServer.Build.ProcessChild', a persistent executor keeps a single
-- 'Types.State.WorkerState' for its whole lifetime, so that the GHC session state (loaded interfaces, bytecode,
-- interpreter) built up by one execute task is reused by the next one dispatched to the same
-- 'Types.Api.ExecutorId'. The gRPC service is implemented in 'GhcWorker.Executor.Child'.
module GhcServer.Build.ExecutorChild where

import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import qualified Data.Aeson as Aeson
import Internal.Executor.Output (Attach)
import GhcServer.Build.ProcessChild (configInvalidResult, evalUncaptured)
import GhcWorker.Executor.Child (planResult, serveExecutor)
import Internal.Executor.Load (PlanSessionVar, newPlanSessionVar)
import Internal.State (newState)
import System.OsPath.Extra (OsPath)
import Types.Executor.Protocol (CallResult, ExecutorCall (..))
import Types.Settings (defaultSettings)
import Types.State (WorkerState)

-- | Evaluate one 'ExecutorCall' against the executor's persistent state.
-- The output is captured by 'serveExecutor'.
-- Module calls share the executor's 'Types.State.WorkerState' and are serialized with the given lock, whereas plans run
-- concurrently.
dispatchCall :: MVar () -> MVar WorkerState -> PlanSessionVar -> Attach -> ExecutorCall -> IO CallResult
dispatchCall moduleLock stateVar planVar attach = \case
  CallModule value ->
    withMVar moduleLock \ () -> either configInvalidResult (evalUncaptured stateVar) (decodeConfig value)
  CallPlan plan -> planResult planVar attach plan
  where
    decodeConfig value =
      case Aeson.fromJSON value of
        Aeson.Error err -> Left err
        Aeson.Success config -> Right config

-- | Entry point of the @executor@ CLI mode: serve the @Executor@ service on the given socket until stdin closes,
-- capturing stdio if the flag is set.
runExecutor :: Bool -> OsPath -> IO ()
runExecutor capture socket = do
  stateVar <- newState defaultSettings
  planVar <- newPlanSessionVar
  moduleLock <- newMVar ()
  serveExecutor capture socket (dispatchCall moduleLock stateVar planVar)
