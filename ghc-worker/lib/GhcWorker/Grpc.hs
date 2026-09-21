module GhcWorker.Grpc where

import BuckWorkerProto ()
import Control.Concurrent.Chan (Chan, dupChan, readChan, writeChan)
import Control.Concurrent.MVar (MVar, modifyMVar_, readMVar)
import Control.Monad (forever)
import Data.Aeson (eitherDecodeStrict, encode)
import Data.Binary qualified as Binary
import Data.ByteString (toStrict)
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (pack)
import GHC.Stats (GCDetails (..), RTSStats (..), getRTSStats)
import Network.GRPC.Common (NextElem (..))
import Network.GRPC.Common.Protobuf (Proto, defMessage, (&), (.~))
import Network.GRPC.Server.Protobuf (ProtobufMethodsOf)
import Network.GRPC.Server.StreamType (Methods (..), mkNonStreaming, mkServerStreaming, simpleMethods)
import Proto.Instrument (Encoded, Instrument)
import Proto.Instrument_Fields qualified as Fields
import Types.Api (
  ApiRequest (..),
  ApiResponse (..),
  Event (..),
  SomeApiRequest (..),
  Target,
  TaskKind (..),
  TaskTrigger (..),
  TrackedBytecode (..),
  homeModuleFromGhc,
  homeModuleMatchTarget,
  )
import Types.FeatureFlags (Feature (..))
import Types.Grpc (CommandEnv (..), RequestArgs (..))
import Types.Settings (toggleFlag)
import qualified Types.State as State
import Types.State (WorkerState (..))
import Types.State.Make (BcoHistoryEntry (..), MakeState (..))

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

-- | Implementation of a streaming grapesy handler that sends instrumentation statistics pulled from the provided
-- channel to the client.
notifyMe ::
  MVar WorkerState ->
  Chan Event ->
  (NextElem (Proto Encoded) -> IO ()) ->
  IO ()
notifyMe stateVar chan callback = do
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
triggerTask ::
  MVar WorkerState ->
  (CommandEnv -> RequestArgs -> IO ()) ->
  TaskTrigger ->
  IO (ApiResponse a)
triggerTask _stateVar _recompile = \case
  TaskTrigger {task = Metadata} ->
    pure (ApiFailure "Cannot trigger metadata")
  TaskTrigger {task = Build _rebuild} ->
    pure (ApiFailure "Cannot trigger build")
  TaskTrigger {task = Execute {}} ->
    pure (ApiFailure "Cannot trigger execute")

-- | Compute cache-tracking info for every module ever tracked in 'MakeState.bcoHistory' (current residents and
-- past evictees alike), decorated with whether it's currently resident in 'MakeState.bcoCache' and whether it has
-- a pending eviction request. Shared by 'getBytecodeState' (RPC response) and 'pushBytecodeState' (pushed event).
--
-- TODO remove those qualifiers
bytecodeEntries :: WorkerState -> [TrackedBytecode]
bytecodeEntries state =
  [
    TrackedBytecode {
      key = homeModuleFromGhc m,
      resident = Map.member m state.make.bcoCache,
      pendingEviction = Set.member m state.make.pendingEvictions,
      ..
    }
    | (m, BcoHistoryEntry {..}) <- Map.toList state.make.bcoHistory
  ]

-- | Snapshot the historic lazily-loaded bytecode cache for the instrumentation UI: every module that has ever been
-- tracked in 'MakeState.bcoHistory' (current residents and past evictees alike), decorated with whether it's
-- currently resident in 'MakeState.bcoCache' and whether it has a pending eviction request.
getBytecodeState :: MVar WorkerState -> IO [TrackedBytecode]
getBytecodeState stateVar = bytecodeEntries <$> readMVar stateVar

-- | Push a snapshot of the bytecode cache (see 'bytecodeEntries') to the instrumentation channel, if enabled.
-- Called whenever the cache may have changed: after a compile\/metadata\/execute task finishes and its session has
-- been stored (see 'Internal.State.withState').
pushBytecodeState :: MVar WorkerState -> Chan Event -> IO ()
pushBytecodeState stateVar chan = do
  state <- readMVar stateVar
  writeChan chan (BytecodeSnapshot (bytecodeEntries state))

-- | Apply a bytecode-eviction request to 'WorkerState', recording the matched modules into
-- 'MakeState.pendingEvictions' without pushing any instrumentation event. Split out from 'evictBytecode' so
-- 'GhcServer.Grpc' can perform the eviction itself even when instrumentation is disabled (no live 'Chan Event'
-- to push a 'BytecodeSnapshot' to).
applyEviction :: MVar WorkerState -> Target -> IO ()
applyEviction stateVar target =
  modifyMVar_ stateVar \ state -> do
    let targets = Set.filter matches (Map.keysSet state.make.bcoCache)
    pure state {make = state.make {pendingEvictions = state.make.pendingEvictions <> targets}}
  where
    matches m = homeModuleMatchTarget (homeModuleFromGhc m) target

-- TODO immediately evict instead of scheduling it
evictBytecode :: MVar WorkerState -> Chan Event -> Target -> IO ()
evictBytecode stateVar chan req = do
  applyEviction stateVar req
  pushBytecodeState stateVar chan

-- | Flip a single 'Feature' in 'WorkerState', shared by both 'GhcWorker.Grpc.apiRequest' and
-- 'GhcServer.Grpc.runCommand'.
toggleFeature :: MVar WorkerState -> Feature -> IO ()
toggleFeature stateVar feature =
  modifyMVar_ stateVar \ state -> pure state {State.settings = toggleFlag feature state.settings}

-- | Dispatch a single decoded 'Command' to the appropriate handler, producing the 'Response'
-- to be JSON-encoded back into the @Send@ RPC's 'Instr.CommandResponse'.
apiRequest ::
  Chan Event ->
  MVar WorkerState ->
  (CommandEnv -> RequestArgs -> IO ()) ->
  ApiRequest a ->
  IO (ApiResponse a)
apiRequest chan stateVar recompile = \case
  TriggerTask trigger ->
    triggerTask stateVar recompile trigger
  EvictBytecode req ->
    ApiSuccess () <$ evictBytecode stateVar chan req
  Clean _ ->
    pure (ApiFailure "Cleaning not supported")
  ToggleFeature {feature} ->
    ApiSuccess () <$ toggleFeature stateVar feature

-- | Implementation of the unified @Send@ RPC: decodes the JSON 'Instr.Command' payload, runs it via the supplied
-- dispatcher, and JSON-encodes the resulting 'Response' back into an 'Instr.CommandResponse'. Exported
-- (rather than kept local) so 'GhcServer.Grpc' can reuse it with its own 'runCommand'-shaped dispatcher.
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

-- | A grapesy server that streams instrumentation data from the provided channel and dispatches every other
-- 'Instrument' operation through the unified @Send@ RPC.
instrumentMethods ::
  Chan Event ->
  MVar WorkerState ->
  (CommandEnv -> RequestArgs -> IO ()) ->
  Methods IO (ProtobufMethodsOf Instrument)
instrumentMethods chan stateVar recompile =
  simpleMethods
    (mkNonStreaming (handleCommand (apiRequest chan stateVar recompile)))
    (mkServerStreaming (const (notifyMe stateVar chan)))
