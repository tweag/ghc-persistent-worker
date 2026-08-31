module GhcWorker.Grpc where

import Common.Grpc ()
import Control.Concurrent.Chan (Chan, dupChan, readChan, writeChan)
import Control.Concurrent.MVar (MVar, modifyMVar_, readMVar)
import Control.Monad (forever)
import Data.Aeson (eitherDecodeStrict, encode)
import Data.Binary qualified as Binary
import Data.ByteString (toStrict)
import Data.ByteString.Lazy qualified as LBS
import Data.Foldable (for_)
import Data.Functor (($>))
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import GHC (moduleNameString)
import GHC qualified as GHC (moduleName)
import GHC.Stats (GCDetails (..), RTSStats (..), getRTSStats)
import GHC.Unit.Types (moduleUnitId, unitIdString)
import Network.GRPC.Common (NextElem (..))
import Network.GRPC.Common.Protobuf (Proto, defMessage, (&), (.~))
import Network.GRPC.Server.Protobuf (ProtobufMethodsOf)
import Network.GRPC.Server.StreamType (Methods (..), mkNonStreaming, mkServerStreaming, simpleMethods)
import qualified Proto.Instrument as Instr
import Proto.Instrument (Instrument)
import Proto.Instrument_Fields qualified as Instr
import System.IO qualified as IO (hPutStrLn, stderr)
import Types.Grpc (CommandEnv (..), RequestArgs (..))
import Types.Instrument qualified as Instrument
import Types.Instrument (Event (..), EvictRequest (..), TaskKind (..), TaskTrigger (..))
import Types.State (Options (..), WorkerState (..))
import Types.State.Make (BcoHistoryEntry (..), MakeState (..))
import Types.Target (TargetSpec (..))

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
  (NextElem (Proto Instr.Event) -> IO ()) ->
  IO ()
notifyMe stateVar chan callback = do
  state <- readMVar stateVar
  myChan <- dupChan chan
  stats <- mkStats state
  callback $ NextElem $
    defMessage
      & Instr.encoded .~ toStrict (Binary.encode stats)
  forever $ do
    msg <- readChan myChan
    callback $ NextElem $
      defMessage
        & Instr.encoded .~ toStrict (Binary.encode msg)

-- | Set the options for the server.
setOptions ::
  MVar WorkerState ->
  Options ->
  IO ()
setOptions stateVar opts =
  modifyMVar_ stateVar $ \state -> pure state {options = opts}

-- | Trigger a build task (recompile, or execute) for the given target. Merges the former separate
-- @triggerRebuild@\/@triggerExecute@ handlers into a single operation parameterized on 'TaskKind'.
triggerTask ::
  MVar WorkerState ->
  (CommandEnv -> RequestArgs -> IO ()) ->
  TaskTrigger ->
  IO ()
triggerTask stateVar recompile TaskTrigger{target, task = Rebuild} = do
  state <- readMVar stateVar
  let margs = Map.lookup (TargetUnknown target) state.targetArgs
  for_ margs (uncurry recompile)
-- | Stub for the persistent-worker protocol: 'GhcWorker' has no unit\/module-oriented project model to execute a
-- unit's modules against (see 'GhcServer.Grpc.triggerTask' for the real implementation, used by @ghc-server@).
-- Logs to stderr on invocation since this handler has no 'Types.Log.Logger' in scope and otherwise silently
-- no-ops, which would be indistinguishable from the request never reaching the server at all.
triggerTask _ _ TaskTrigger{target, task = Execute} =
  IO.hPutStrLn IO.stderr ("triggerTask: stub invoked for Execute (ghc-worker has no execute support), target=" ++ target)

-- | Compute cache-tracking info for every module ever tracked in 'MakeState.bcoHistory' (current residents and
-- past evictees alike), decorated with whether it's currently resident in 'MakeState.bcoCache' and whether it has
-- a pending eviction request. Shared by 'getBytecodeState' (RPC response) and 'pushBytecodeState' (pushed event).
bytecodeEntries :: WorkerState -> [Instrument.BcoEntryInfo]
bytecodeEntries state =
  [ Instrument.BcoEntryInfo
      { Instrument.unitId = unitIdString (moduleUnitId m)
      , Instrument.moduleName = moduleNameString (GHC.moduleName m)
      , Instrument.size = entry.size
      , Instrument.lastAccess = entry.lastAccess
      , Instrument.resident = Map.member m state.make.bcoCache
      , Instrument.pendingEviction = Set.member m state.make.pendingEvictions
      }
  | (m, entry) <- Map.toList state.make.bcoHistory
  ]

-- | Snapshot the historic lazily-loaded bytecode cache for the instrumentation UI: every module that has ever been
-- tracked in 'MakeState.bcoHistory' (current residents and past evictees alike), decorated with whether it's
-- currently resident in 'MakeState.bcoCache' and whether it has a pending eviction request.
getBytecodeState :: MVar WorkerState -> IO [Instrument.BcoEntryInfo]
getBytecodeState stateVar = bytecodeEntries <$> readMVar stateVar

-- | Push a snapshot of the bytecode cache (see 'bytecodeEntries') to the instrumentation channel, if enabled.
-- Called whenever the cache may have changed: after a compile\/metadata\/execute task finishes and its session has
-- been stored (see 'Internal.State.withState').
pushBytecodeState :: MVar WorkerState -> Chan Event -> IO ()
pushBytecodeState stateVar chan = do
  state <- readMVar stateVar
  writeChan chan (BytecodeSnapshot (bytecodeEntries state))

-- | Request eviction of a module (or, if 'moduleName' is empty, an entire unit; or, if 'unitId' is the sentinel
-- @"*"@, every unit in the project) from the lazily-loaded bytecode cache. Deferred until the next compile
-- job's session is stored, since eviction requires a live 'HscEnv'/'Interp' (see 'Types.State.Make.pendingEvictions').
evictBytecode :: MVar WorkerState -> EvictRequest -> IO ()
evictBytecode stateVar req =
  modifyMVar_ stateVar \state -> do
    let
      matches m =
        (req.unitId == "*" || unitIdString (moduleUnitId m) == req.unitId)
        && (null req.moduleName || moduleNameString (GHC.moduleName m) == req.moduleName)
      targets = Set.filter matches (Map.keysSet state.make.bcoCache)
    pure state {make = state.make {pendingEvictions = state.make.pendingEvictions <> targets}}

-- | Dispatch a single decoded 'Instrument.Command' to the appropriate handler, producing the 'Instrument.Response'
-- to be JSON-encoded back into the @Send@ RPC's 'Instr.CommandResponse'.
runCommand ::
  MVar WorkerState ->
  (CommandEnv -> RequestArgs -> IO ()) ->
  Instrument.Command ->
  IO Instrument.Response
runCommand stateVar recompile = \case
  Instrument.SetOptions opts -> setOptions stateVar opts $> Instrument.Ack
  Instrument.TriggerTask trigger -> triggerTask stateVar recompile trigger $> Instrument.Ack
  Instrument.EvictBytecode req -> evictBytecode stateVar req $> Instrument.Ack

-- | Implementation of the unified @Send@ RPC: decodes the JSON 'Instr.Command' payload, runs it via the supplied
-- dispatcher, and JSON-encodes the resulting 'Instrument.Response' back into an 'Instr.CommandResponse'. Exported
-- (rather than kept local) so 'GhcServer.Grpc' can reuse it with its own 'runCommand'-shaped dispatcher.
handleCommand ::
  (Instrument.Command -> IO Instrument.Response) ->
  Proto Instr.Command ->
  IO (Proto Instr.CommandResponse)
handleCommand run req = do
  resp <- case eitherDecodeStrict req.payload of
    Left err -> do
      IO.hPutStrLn IO.stderr ("handleCommand: failed to decode payload: " ++ err)
      pure Instrument.Ack
    Right cmd -> run cmd
  pure (defMessage & Instr.payload .~ LBS.toStrict (encode resp))

-- | A grapesy server that streams instrumentation data from the provided channel and dispatches every other
-- 'Instrument' operation through the unified @Send@ RPC.
instrumentMethods ::
  Chan Event ->
  MVar WorkerState ->
  (CommandEnv -> RequestArgs -> IO ()) ->
  Methods IO (ProtobufMethodsOf Instrument)
instrumentMethods chan stateVar recompile =
  simpleMethods
    (mkServerStreaming (const (notifyMe stateVar chan)))
    (mkNonStreaming (handleCommand (runCommand stateVar recompile)))

