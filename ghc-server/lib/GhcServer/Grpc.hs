-- | gRPC 'Instrument' service for the standalone GHC server.
--
-- Reuses most of the worker's 'GhcWorker.Grpc' handlers (which only depend on 'Types.State.WorkerState'), but
-- replaces 'triggerRebuild': the worker's version looks up cached Buck target args, which 'ghc-server' never
-- populates. Instead, the rebuild request's target text is parsed and scheduled the same way 'ghc-client' does.
module GhcServer.Grpc where

import Common.Grpc ()
import Control.Concurrent.Chan (Chan)
import Control.Concurrent.MVar (MVar)
import Data.Binary (encode)
import Data.ByteString (toStrict)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import GhcServer.Build (Build, scheduleBatch)
import GhcServer.Data.Request (ScheduleRequest (..))
import GhcServer.Data.Unit (Project (..), Unit (..), UnitName (..))
import GhcServer.Handler (parseTarget)
import GhcServer.Path (fp)
import GhcWorker.Grpc (evictBytecode, getBytecodeState, setOptions)
import GhcWorker.Grpc qualified as Worker
import Network.GRPC.Common (NextElem (NextElem))
import Network.GRPC.Common.Protobuf (Proto, defMessage, (&), (.~))
import Network.GRPC.Server.Protobuf (ProtobufMethodsOf)
import Network.GRPC.Server.StreamType (Methods, mkNonStreaming, mkServerStreaming, simpleMethods)
import qualified Proto.Instrument as Instr
import Proto.Instrument (Instrument)
import Proto.Instrument_Fields qualified as Instr
import System.FilePath (takeBaseName)
import Types.Instrument (Event (..), UnitSummary (..))
import Types.State (WorkerState)

-- | Build a snapshot of the project's units and modules for the instrument UI's task tree, from the units
-- discovered at server startup (source file basenames as module names, no compilation required).
projectStructureEvent :: Project -> Event
projectStructureEvent project =
  ProjectStructure
    { units =
        [ UnitSummary
            { unitName = name.string
            , modules = [takeBaseName (fp src) | src <- unit.sources]
            }
        | (name, unit) <- Map.toList project.units
        ]
    }

-- | Wraps 'GhcWorker.Grpc.notifyMe' to additionally send a 'Instr.ProjectStructure' snapshot as soon as a client
-- connects, ahead of the initial stats snapshot. Unlike 'ghc-worker', 'ghc-server' has no persistent-worker-style
-- compile events to forward (see the caveat in 'kb-grpc'/'kb-instrument-ui'), so this is the only project-derived
-- data pushed to the UI on connect.
notifyMe ::
  Project ->
  MVar WorkerState ->
  Chan Event ->
  (NextElem (Proto Instr.Event) -> IO ()) ->
  IO ()
notifyMe project stateVar chan callback = do
  callback $ NextElem $
    defMessage
      & Instr.encoded .~ toStrict (encode (projectStructureEvent project))
  Worker.notifyMe stateVar chan callback

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
    (mkServerStreaming (const (notifyMe project stateVar chan)))
    (mkNonStreaming (setOptions stateVar))
    (mkNonStreaming (triggerRebuild build project))
