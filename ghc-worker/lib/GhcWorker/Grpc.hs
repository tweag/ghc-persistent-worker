module GhcWorker.Grpc where

import Common.Grpc ()
import Control.Concurrent.Chan (Chan, dupChan, readChan)
import Control.Concurrent.MVar (MVar, modifyMVar_, readMVar)
import Control.Monad (forever)
import Data.Binary (encode)
import Data.ByteString (toStrict)
import Data.Foldable (for_)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import GHC (moduleName, moduleNameString)
import GHC.Stats (GCDetails (..), RTSStats (..), getRTSStats)
import GHC.Unit.Types (moduleUnitId, unitIdString)
import Network.GRPC.Common (NextElem (..))
import Network.GRPC.Common.Protobuf (Proto, defMessage, (&), (.~))
import Network.GRPC.Server.Protobuf (ProtobufMethodsOf)
import Network.GRPC.Server.StreamType (Methods (..), mkNonStreaming, mkServerStreaming, simpleMethods)
import qualified Proto.Instrument as Instr
import Proto.Instrument (Instrument)
import Proto.Instrument_Fields qualified as Instr
import Types.Grpc (CommandEnv (..), RequestArgs (..))
import Types.Instrument (Event (..))
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
      & Instr.encoded .~ toStrict (encode stats)
  forever $ do
    msg <- readChan myChan
    callback $ NextElem $
      defMessage
        & Instr.encoded .~ toStrict (encode msg)

-- | Set the options for the server.
setOptions ::
  MVar WorkerState ->
  Proto Instr.Options ->
  IO (Proto Instr.Empty)
setOptions stateVar opts = do
  modifyMVar_ stateVar $ \state ->
    pure state {
      options = Options {
        extraGhcOptions = Text.unpack opts.extraGhcOptions
      }
    }
  pure defMessage

-- | Trigger a rebuild for the given target.
triggerRebuild ::
  MVar WorkerState ->
  (CommandEnv -> RequestArgs -> IO ()) ->
  Proto Instr.RebuildRequest ->
  IO (Proto Instr.Empty)
triggerRebuild stateVar recompile target = do
  state <- readMVar stateVar
  let margs = Map.lookup (TargetUnknown (Text.unpack target.target)) state.targetArgs
  for_ margs (uncurry recompile)
  pure defMessage

-- | Snapshot the historic lazily-loaded bytecode cache for the instrumentation UI: every module that has ever been
-- tracked in 'MakeState.bcoHistory' (current residents and past evictees alike), decorated with whether it's
-- currently resident in 'MakeState.bcoCache' and whether it has a pending eviction request.
getBytecodeState ::
  MVar WorkerState ->
  Proto Instr.Empty ->
  IO (Proto Instr.BytecodeState)
getBytecodeState stateVar _ = do
  state <- readMVar stateVar
  let entries =
        [ defMessage
            & Instr.unitId .~ Text.pack (unitIdString (moduleUnitId m))
            & Instr.moduleName .~ Text.pack (moduleNameString (moduleName m))
            & Instr.size .~ fromIntegral (entry.size :: Int)
            & Instr.lastAccess .~ fromIntegral (entry.lastAccess :: Int)
            & Instr.resident .~ Map.member m state.make.bcoCache
            & Instr.pendingEviction .~ Set.member m state.make.pendingEvictions
        | (m, entry) <- Map.toList state.make.bcoHistory
        ]
  pure (defMessage & Instr.entries .~ entries)

-- | Request eviction of a module (or, if 'moduleName' is empty, an entire unit) from the lazily-loaded bytecode
-- cache. Deferred until the next compile job's session is stored, since eviction requires a live 'HscEnv'/'Interp'
-- (see 'Types.State.Make.pendingEvictions').
evictBytecode ::
  MVar WorkerState ->
  Proto Instr.EvictBytecodeRequest ->
  IO (Proto Instr.Empty)
evictBytecode stateVar req = do
  modifyMVar_ stateVar \ state -> do
    let
      matches m =
        unitIdString (moduleUnitId m) == Text.unpack req.unitId
        && (Text.null req.moduleName || moduleNameString (moduleName m) == Text.unpack req.moduleName)
      targets = Set.filter matches (Map.keysSet state.make.bcoCache)
    pure state {make = state.make {pendingEvictions = state.make.pendingEvictions <> targets}}
  pure defMessage

-- | A grapesy server that streams instrumentation data from the provided channel.
instrumentMethods ::
  Chan Event ->
  MVar WorkerState ->
  (CommandEnv -> RequestArgs -> IO ()) ->
  Methods IO (ProtobufMethodsOf Instrument)
instrumentMethods chan stateVar recompile =
  simpleMethods
    (mkNonStreaming (evictBytecode stateVar))
    (mkNonStreaming (getBytecodeState stateVar))
    (mkServerStreaming (const (notifyMe stateVar chan)))
    (mkNonStreaming (setOptions stateVar))
    (mkNonStreaming (triggerRebuild stateVar recompile))
