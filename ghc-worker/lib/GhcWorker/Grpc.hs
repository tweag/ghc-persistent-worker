module GhcWorker.Grpc where

import BuckWorkerProto ()
import Common.Grpc (GrpcHandler, execute, workerMethods)
import Control.Concurrent.Chan (Chan, dupChan, readChan)
import Control.Concurrent.MVar (MVar, modifyMVar_, readMVar)
import Control.Monad (forever)
import Data.Aeson (eitherDecodeStrict, encode)
import Data.Binary qualified as Binary
import Data.ByteString (toStrict)
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as Map
import Data.Text (pack)
import GHC.Stats (GCDetails (..), RTSStats (..), getRTSStats)
import Network.GRPC.Common (NextElem (..))
import Network.GRPC.Common.Protobuf (Proto, defMessage, (&), (.~))
import Network.GRPC.Server.Protobuf (ProtobufMethodsOf)
import Network.GRPC.Server.StreamType (Methods (..))
import Proto.Worker (Encoded, Worker)
import Proto.Worker_Fields qualified as Fields
import Types.Api (
  ApiRequest (..),
  ApiResponse (..),
  Event (..),
  SomeApiRequest (..),
  TaskKind (..),
  TaskTrigger (..),
  )
import Types.FeatureFlags (Feature (..))
import Types.Settings (toggleFlag)
import qualified Types.State as State
import Types.State (WorkerState (..))

-- | Fetch statistics about the current state of the RTS for instrumentation.
mkStats :: WorkerState -> IO Event
mkStats _ = do
  s <- getRTSStats
  pure $
    Stats
      { memory = Map.fromList
          [ ("Total", fromIntegral s.gc.gcdetails_mem_in_use_bytes)
          ]
      , gcCpuNs = fromIntegral s.gc_cpu_ns
      , cpuNs = fromIntegral s.cpu_ns
      }

-- | Implementation of the @Events@ method: sends instrumentation statistics and events pulled from the provided
-- channel to the client.
-- If instrumentation is disabled, the stream ends immediately.
notifyMe ::
  MVar WorkerState ->
  Maybe (Chan Event) ->
  (NextElem (Proto Encoded) -> IO ()) ->
  IO ()
notifyMe _ Nothing callback = callback NoNextElem
notifyMe stateVar (Just chan) callback = do
  state <- readMVar stateVar
  myChan <- dupChan chan
  stats <- mkStats state
  callback $ NextElem $
    defMessage
      & Fields.payload .~ toStrict (Binary.encode stats)
  forever $ do
    msg <- readChan myChan
    callback $ NextElem $
      defMessage
        & Fields.payload .~ toStrict (Binary.encode msg)

-- TODO rebuild flag is now duplicated again?
triggerTask :: TaskTrigger -> IO (ApiResponse a)
triggerTask = \case
  TaskTrigger {task = Metadata} ->
    pure (ApiFailure "Cannot trigger metadata")
  TaskTrigger {task = Build _rebuild} ->
    pure (ApiFailure "Cannot trigger build")
  TaskTrigger {task = Execute {}} ->
    pure (ApiFailure "Cannot trigger execute")

-- | Flip a single 'Feature' in 'WorkerState', shared by both 'GhcWorker.Grpc.apiRequest' and
-- 'GhcServer.Grpc.runCommand'.
toggleFeature :: MVar WorkerState -> Feature -> IO ()
toggleFeature stateVar feature =
  modifyMVar_ stateVar \ state -> pure state {State.settings = toggleFlag feature state.settings}

-- | Dispatch a single decoded 'ApiRequest' to the appropriate handler.
-- The worker doesn't support bytecode eviction yet.
apiRequest :: MVar WorkerState -> ApiRequest a -> IO (ApiResponse a)
apiRequest stateVar = \case
  TriggerTask trigger ->
    triggerTask trigger
  EvictBytecode _ ->
    pure (ApiFailure "Evicting bytecode not supported")
  Clean _ ->
    pure (ApiFailure "Cleaning not supported")
  ToggleFeature {feature} ->
    ApiSuccess () <$ toggleFeature stateVar feature
  TerminateExecutor {} ->
    pure (ApiFailure "Executor subprocesses not supported by this worker")

-- | Implementation of the @Api@ method: decodes the JSON payload, runs it via the supplied dispatcher, and
-- JSON-encodes the resulting response.
-- Exported (rather than kept local) so 'GhcServer.Grpc' can reuse it with its own dispatcher.
handleCommand ::
  (forall a . ApiRequest a -> IO (ApiResponse a)) ->
  Proto Encoded ->
  IO (Proto Encoded)
handleCommand run req = do
  resp <- case eitherDecodeStrict req.payload of
    Left err -> do
      pure (encode (ApiFailure @() ("handleCommand: failed to decode payload: " <> pack err)))
    Right (SomeApiRequest cmd) -> encode <$> run cmd
  pure (defMessage & Fields.payload .~ LBS.toStrict resp)

-- | The 'Worker' service of the GHC worker: builds are handled by the 'GrpcHandler', while instrumentation data is
-- streamed from the provided channel, if there is one, and the remaining API requests are dispatched through the
-- unified @Api@ method.
ghcMethods ::
  MVar WorkerState ->
  Maybe (Chan Event) ->
  GrpcHandler ->
  Methods IO (ProtobufMethodsOf Worker)
ghcMethods stateVar chan handler =
  workerMethods
    (handleCommand (apiRequest stateVar))
    (notifyMe stateVar chan)
    (execute handler)
