-- | gRPC 'Instrument' service for the standalone GHC server.
--
-- Reuses most of the worker's 'GhcWorker.Grpc' handlers (which only depend on 'Types.State.WorkerState'), but
-- replaces 'triggerRebuild': the worker's version looks up cached Buck target args, which 'ghc-server' never
-- populates. Instead, the rebuild request's target text is parsed and scheduled the same way 'ghc-client' does.
module GhcServer.Grpc where

import Common.Grpc ()
import Control.Concurrent.Chan (Chan)
import Control.Concurrent.MVar (MVar)
import qualified Data.Text as Text
import GhcServer.Build (Build, scheduleBatch)
import GhcServer.Data.Request (ScheduleRequest (..))
import GhcServer.Data.Unit (Project)
import GhcServer.Handler (parseTarget)
import GhcWorker.Grpc (evictBytecode, getBytecodeState, notifyMe, setOptions)
import Network.GRPC.Common.Protobuf (Proto, defMessage)
import Network.GRPC.Server.Protobuf (ProtobufMethodsOf)
import Network.GRPC.Server.StreamType (Methods, mkNonStreaming, mkServerStreaming, simpleMethods)
import qualified Proto.Instrument as Instr
import Proto.Instrument (Instrument)
import Types.Instrument (Event)
import Types.State (WorkerState)

-- | Trigger a build for the given target, parsed and scheduled the same way 'ghc-client' would from its argv
-- (@unitName@, @unitName:metadata@, @unitName:modules@, @unitName:ModuleName@). Fire-and-forget: does not wait for
-- completion, matching the semantics of the worker's own 'triggerRebuild'.
triggerRebuild ::
  Build ->
  Project ->
  Proto Instr.RebuildRequest ->
  IO (Proto Instr.Empty)
triggerRebuild build project req = do
  case parseTarget project (Text.unpack req.target) of
    Left _ -> pure ()
    Right (name, unitReq) ->
      scheduleBatch build ScheduleRequest {steps = [(name, unitReq)], recompile = True, rebuild = False}
  pure defMessage

-- | A grapesy server that streams instrumentation data and serves the bytecode-cache browser RPCs, backed by
-- 'ghc-server'\'s persistent 'WorkerState' and scheduler.
instrumentMethods ::
  Chan Event ->
  MVar WorkerState ->
  Build ->
  Project ->
  Methods IO (ProtobufMethodsOf Instrument)
instrumentMethods chan stateVar build project =
  simpleMethods
    (mkNonStreaming (evictBytecode stateVar))
    (mkNonStreaming (getBytecodeState stateVar))
    (mkServerStreaming (const (notifyMe stateVar chan)))
    (mkNonStreaming (setOptions stateVar))
    (mkNonStreaming (triggerRebuild build project))
