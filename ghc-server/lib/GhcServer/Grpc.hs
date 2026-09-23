module GhcServer.Grpc where

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (Chan, dupChan, readChan)
import Control.Concurrent.MVar (MVar, readMVar)
import Control.Concurrent.STM (atomically, modifyTVar', readTVar)
import Control.Monad (forever, void, when)
import Data.Aeson (eitherDecodeStrict)
import Data.Aeson qualified as Aeson (encode)
import Data.Binary qualified as Binary (encode)
import Data.ByteString (toStrict)
import Data.ByteString.Lazy qualified as LBS
import Data.Coerce (coerce)
import Data.Foldable (for_)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Data.Text (pack)
import GHC (moduleNameString)
import GhcServer.Build (Build (..), awaitBuild, scheduleBatch)
import GhcServer.Build.Executor (terminateExecutor)
import GhcServer.Data.BuildEnv (BuildEnv (..))
import GhcServer.Data.Request (ScheduleRequest (..), UnitRequest (..))
import GhcServer.Data.Unit (ClientModule (..), Project (..), Unit (..))
import GhcServer.Handler (ServerContext (..), executeCommand, runClean)
import GhcServer.Log (emitEvent)
import GhcWorker.Grpc qualified as Worker
import Network.GRPC.Common (NextElem (..))
import Network.GRPC.Common.Protobuf (Proto, defMessage, (&), (.~))
import Network.GRPC.Server.Protobuf (ProtobufMethodsOf)
import Network.GRPC.Server.StreamType (Methods, mkNonStreaming, mkServerStreaming, simpleMethods)
import Proto.GhcServer (Encoded, GhcServer)
import Proto.GhcServer_Fields qualified as Fields
import qualified Types.Api as TaskKind
import Types.Api (
  ApiRequest (..),
  ApiResponse (..),
  Event (..),
  HomeModule (..),
  ModuleName (..),
  SomeApiRequest (..),
  Target (..),
  TaskKind (..),
  TaskTrigger (..),
  UnitName (..),
  UnitSummary (..),
  )
import Types.Settings (Settings (..))
import Types.State (WorkerState (settings))

-- | Build a snapshot of the project's units and modules for the instrument UI's task tree, from the units
-- discovered at server startup (source file basenames as module names, no compilation required), alongside the
-- currently active feature flags.
projectStructureEvent :: Project -> Settings -> Event
projectStructureEvent project settings =
  ProjectStructure {
    units = [UnitSummary {name, modules = [ModuleName (Text.pack (moduleNameString m)) | (m, _) <- unit.modules]} | (name, unit) <- Map.toList project.units],
    settings
  }

-- | Implementation of the 'Events' RPC: streams instrumentation data pulled from the server's shared event
-- channel, prepending a 'ProjectStructure' snapshot and an initial RTS-stats snapshot as soon as a client
-- connects (mirroring the former @Instrument@ service's @NotifyMe@, which 'GhcServer.Grpc.notifyMe' used to wrap
-- around 'GhcWorker.Grpc.notifyMe'). When instrumentation is disabled (no '--enable instrument', so
-- 'GhcServer.Data.BuildEnv.BuildEnv.instrChan' is 'Nothing'), the stream ends immediately rather than blocking
-- forever or refusing the connection outright -- unlike the former dual-socket design, this RPC is always
-- present on the single 'GhcServer' service, so a client can no longer detect "instrumentation disabled" by a
-- failed connection attempt.
events ::
  Project ->
  MVar WorkerState ->
  Maybe (Chan Event) ->
  (NextElem (Proto Encoded) -> IO ()) ->
  IO ()
events _ _ Nothing callback = callback NoNextElem
events project stateVar (Just chan) callback = do
  state <- readMVar stateVar
  callback $ NextElem $
    defMessage & Fields.payload .~ toStrict (Binary.encode (projectStructureEvent project state.settings))
  stats <- Worker.mkStats state
  callback $ NextElem $
    defMessage & Fields.payload .~ toStrict (Binary.encode stats)
  myChan <- dupChan chan
  forever do
    msg <- readChan myChan
    callback $ NextElem $
      defMessage & Fields.payload .~ toStrict (Binary.encode msg)

targetToUnitRequest :: Project -> Target -> TaskKind -> [(UnitName, UnitRequest)]
targetToUnitRequest project = \cases
  TargetProject kind ->
    [(unit, unitRequest kind) | unit <- Map.keys project.units]
  TargetUnit {name} kind ->
    [(name, unitRequest kind)]
  TargetModule {key = HomeModule {unit, name}} kind ->
    [(unit, moduleRequest name kind)]
  where
    unitRequest = \case
      Metadata -> UnitMetadata
      TaskKind.Build {} -> UnitAll
      Execute {} -> UnitExecute

    moduleRequest name = \case
      Metadata -> UnitMetadata
      TaskKind.Build {} -> UnitModules [ClientModule (coerce name)]
      Execute {} -> UnitExecute

triggerTask ::
  Maybe (Chan Event) ->
  Build ->
  Project ->
  TaskTrigger ->
  IO ()
triggerTask mchan build project TaskTrigger {target, task} = do
  atomically $ modifyTVar' build.inFlight (+ 1)
  scheduleBatch build request
  void $ forkIO do
    _ <- awaitBuild build
    remaining <- atomically do
      modifyTVar' build.inFlight (subtract 1)
      readTVar build.inFlight
    when (remaining == 0) $
      emitEvent mchan (RequestCompleted "All tasks concluded")
  where
    steps = targetToUnitRequest project target task

    request = case task of
      TaskKind.Metadata -> ScheduleRequest {steps, recompile = False, rebuild = False, executor = Nothing}
      TaskKind.Build rebuild -> ScheduleRequest {steps, recompile = rebuild, rebuild, executor = Nothing}
      Execute {executor} -> ScheduleRequest {steps = map toExecuteStep steps, recompile = False, rebuild = False, executor}

    toExecuteStep (name, unitReq) = (name, executeVariant unitReq)

    executeVariant = \case
      UnitModules mods -> UnitExecuteModules mods
      _ -> UnitExecute

-- | Dispatch a single decoded 'Command' to the appropriate handler: this module's own 'triggerTask'
-- (needs the scheduler and parsed project), 'GhcWorker.Grpc.applyEviction' (only depends on 'WorkerState';
-- pushes a 'BytecodeSnapshot' event only when instrumentation is enabled, since eviction itself must still work
-- without a live channel), and 'GhcServer.Handler.runClean' (needs the scheduler and 'BuildEnv' to invalidate
-- in-memory\/on-disk build state -- this used to be its own gRPC method before the 'GhcServer'\/'Instrument'
-- proto merge).
runCommand ::
  Maybe (Chan Event) ->
  Build ->
  BuildEnv ->
  Project ->
  ApiRequest a ->
  IO (ApiResponse a)
runCommand mchan build env project = \case
  TriggerTask trigger -> ApiSuccess () <$ triggerTask mchan build project trigger
  EvictBytecode req -> do
    Worker.applyEviction env.stateVar req
    for_ mchan (Worker.pushBytecodeState env.stateVar)
    pure (ApiSuccess ())
  Clean target -> runClean build env target
  ToggleFeature {feature} -> ApiSuccess () <$ Worker.toggleFeature env.stateVar feature
  TerminateExecutor {executor} -> ApiSuccess () <$ terminateExecutor env executor

-- | Implementation of the unified 'Api' RPC: JSON-decodes the 'Command' from the request 'GS.Json'\'s
-- @payload@ field, runs it via the supplied dispatcher, and JSON-encodes the resulting 'Response'
-- back into a response 'GS.Json'. Replaces the former @Instrument@ service's @Send@ RPC\/'GhcWorker.Grpc.handleCommand'
-- (which was pinned to the now-unused @ApiRequest@\/@CommandResponse@ proto messages); this version is
-- pinned to @ghc-server.proto@'s @Encoded@ message, used for both request and response.
handleApi ::
  (forall a . ApiRequest a -> IO (ApiResponse a)) ->
  Proto Encoded ->
  IO (Proto Encoded)
handleApi run req = do
  resp <- case eitherDecodeStrict req.payload of
    Left err ->
      pure (Aeson.encode (ApiFailure @() ("handleApi: failed to decode payload: " <> pack err)))
    Right (SomeApiRequest cmd) -> Aeson.encode <$> run cmd
  pure (defMessage & Fields.payload .~ LBS.toStrict resp)

-- | Assemble the gRPC 'Methods' for the 'GhcServer' service, backed by the persistent state\/scheduler built by
-- 'GhcServer.Handler.serverContext'.
--
-- 'ServiceMethods'' generated type family lists RPC methods in alphabetical order, not declaration order --
-- @ghc-server.proto@'s merged service is @'["api", "events", "execute"]@, hence that argument order below.
serverMethods :: ServerContext -> Methods IO (ProtobufMethodsOf GhcServer)
serverMethods ctx =
  simpleMethods
    (mkNonStreaming (handleApi (runCommand ctx.buildEnv.instrChan ctx.build ctx.buildEnv ctx.buildEnv.project)))
    (mkServerStreaming (const (events ctx.buildEnv.project ctx.buildEnv.stateVar ctx.buildEnv.instrChan)))
    (mkNonStreaming (executeCommand ctx.grpcHandler))
