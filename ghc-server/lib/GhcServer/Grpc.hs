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
import GhcServer.Data.BuildEnv (BuildEnv (..))
import GhcServer.Data.Request (ScheduleRequest (..), UnitRequest (..))
import GhcServer.Data.Unit (Project (..), Unit (..), UnitName (..))
import GhcServer.Handler (parseTarget)
import GhcServer.Path (fp)
import GhcWorker.Grpc qualified as Worker
import GhcWorker.Grpc (evictBytecode, getBytecodeState, setOptions)
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
--
-- The sentinel unit name @"*"@ (used by the instrument UI's project-root task-tree node) requests the same
-- scope -- @"*:metadata"@ or bare @"*"@\/@"*:modules"@ -- for every unit in the project at once, expanding to one
-- schedule step per unit rather than being resolved by 'parseTarget' (which knows nothing about multi-unit
-- targets).
--
-- Always forces @rebuild = True@: unlike a scheduled build implicitly triggered by another target's dependency
-- resolution, this is always an explicit user request (from the instrument UI's 'b'/'r' keys, or 'ghc-client'
-- without flags), so metadata must actually be recomputed rather than silently skipped because the unit's cache
-- from a previous build looks up to date (see 'GhcServer.Build.Propagate.dispatchTask').
triggerRebuild ::
  Build ->
  Project ->
  Proto Instr.RebuildRequest ->
  IO (Proto Instr.Empty)
triggerRebuild build project req = do
  let targetText = Text.unpack req.target
  case break (== ':') targetText of
    ("*", suffix) -> do
      let unitReq = case suffix of
            ':' : "metadata" -> UnitMetadata
            _ -> UnitAll
      scheduleBatch build ScheduleRequest
        { steps = [(name, unitReq) | name <- Map.keys project.units]
        , recompile = True
        , rebuild = True
        }
    _ ->
      case parseTarget project targetText of
        Left _ -> error "TODO bad target string"
        Right (name, unitReq) ->
          scheduleBatch build ScheduleRequest {steps = [(name, unitReq)], recompile = True, rebuild = True}
  pure defMessage

-- | Trigger execution of @main@ for the given target, using the same target grammar as 'triggerRebuild'\/
-- 'parseTarget', with an added @:execute@ selector\/\@"*"@ sentinel:
--
-- * @"*"@ (project-root node selected) -- execute every module of every unit in the project.
-- * @unitName@ or @unitName:execute@ (unit header row selected) -- execute every module of that unit.
-- * @unitName:moduleName@ (module row selected) -- execute only that module.
--
-- Dispatched as an ordinary scheduler batch ('GhcServer.Build.Schedule.ExecuteModule' tasks, depending on their
-- module's compile task), rather than a raw 'forkIO'\/'Control.Concurrent.Async.forConcurrently_' fan-out.
-- Fire-and-forget, like 'triggerRebuild': does not await completion.
triggerExecute ::
  Build ->
  Project ->
  Proto Instr.RebuildRequest ->
  IO (Proto Instr.Empty)
triggerExecute build project req = do
  let targetText = Text.unpack req.target
  case break (== ':') targetText of
    ("*", _) ->
      scheduleBatch build ScheduleRequest
        { steps = [(name, UnitExecute) | name <- Map.keys project.units]
        , recompile = True
        , rebuild = True
        }
    _ ->
      case parseTarget project targetText of
        Left _ -> pure ()
        Right (name, unitReq) ->
          let
            execReq = case unitReq of
              UnitModules mods -> UnitExecuteModules mods
              _ -> UnitExecute
          in
            scheduleBatch build ScheduleRequest {steps = [(name, execReq)], recompile = True, rebuild = True}
  pure defMessage

-- | A grapesy server that streams instrumentation data and serves the bytecode-cache browser RPCs, backed by
-- 'ghc-server'\'s persistent 'WorkerState' and scheduler.
instrumentMethods ::
  Chan Event ->
  MVar WorkerState ->
  Build ->
  Project ->
  BuildEnv ->
  Methods IO (ProtobufMethodsOf Instrument)
instrumentMethods chan stateVar build project _buildEnv =
  simpleMethods
    (mkNonStreaming (evictBytecode stateVar))
    (mkNonStreaming (getBytecodeState stateVar))
    (mkServerStreaming (const (notifyMe project stateVar chan)))
    (mkNonStreaming (setOptions stateVar))
    (mkNonStreaming (triggerExecute build project))
    (mkNonStreaming (triggerRebuild build project))
