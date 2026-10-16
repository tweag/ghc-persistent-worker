-- | The child side of a persistent executor subprocess of the @ghc-server@ (see 'GhcServer.Build.Executor' for the
-- parent side).
--
-- Unlike the one-shot child in 'GhcServer.Build.ProcessChild', a persistent executor keeps a single
-- 'Types.State.WorkerState' for its whole lifetime, so that the GHC session state (loaded interfaces, bytecode,
-- interpreter) built up by one execute task is reused by the next one dispatched to the same
-- 'Types.Api.ExecutorId'. The gRPC service is implemented in 'GhcWorker.Executor.Child'.
module GhcServer.Build.ExecutorChild where

import Control.Concurrent.MVar (MVar)
import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import GhcServer.Build.ProcessChild (configInvalidResult, evalResult)
import GhcWorker.Executor.Child (planResult, serveExecutor)
import Internal.Executor.Load (PlanSessionVar, newPlanSessionVar)
import Internal.State (newState)
import System.OsPath.Extra (OsPath)
import Types.Executor.Protocol (CallResult, ExecutorCall (..))
import Types.Settings (defaultSettings)
import Types.State (WorkerState)

-- | Evaluate one encoded 'ExecutorCall' against the executor's persistent state.
dispatchCall :: MVar WorkerState -> PlanSessionVar -> ByteString -> IO CallResult
dispatchCall stateVar planVar payload =
  either configInvalidResult dispatch (Aeson.eitherDecodeStrict' payload)
  where
    dispatch = \case
      CallModule value -> either configInvalidResult (evalResult stateVar) (decodeConfig value)
      CallPlan plan -> planResult planVar plan

    decodeConfig value =
      case Aeson.fromJSON value of
        Aeson.Error err -> Left err
        Aeson.Success config -> Right config

-- | Entry point of the @executor@ CLI mode: serve the @Executor@ service on the given socket until stdin closes.
runExecutor :: OsPath -> IO ()
runExecutor socket = do
  stateVar <- newState defaultSettings
  planVar <- newPlanSessionVar
  serveExecutor socket (dispatchCall stateVar planVar)
