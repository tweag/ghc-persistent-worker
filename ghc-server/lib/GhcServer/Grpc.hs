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
import GhcServer.Data.Request (ScheduleRequest (..), UnitRequest (..))
import GhcServer.Data.Unit (Project (..), Unit (..), UnitName (..))
import GhcServer.Handler (parseTarget)
import GhcServer.Log (emitLog)
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
-- connects, ahead of the initial stats snapshot. This is the only project-derived data pushed to the UI on
-- connect.
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
-- (@unitName@, @unitName:metadata@, @unitName:modules@, @unitName:ModuleName@, or the @"*"@ wildcard for every
-- unit in the project). Fire-and-forget: does not wait for completion, matching the semantics of the worker's own
-- 'triggerRebuild'.
--
-- Always forces @rebuild = True@: unlike a scheduled build implicitly triggered by another target's dependency
-- resolution, this is always an explicit user request (from the instrument UI's 'b'/'r' keys, or 'ghc-client'
-- without flags), so metadata must actually be recomputed rather than silently skipped because the unit's cache
-- from a previous build looks up to date (see 'GhcServer.Build.Propagate.dispatchTask').
--
-- A malformed target string (one that fails 'parseTarget', e.g. an unknown unit name) is rejected by logging an
-- @"error"@-level 'Types.Instrument.LogMessage' on the instrument channel (visible in the @instrument@ app's
-- @L@ log viewer) and returning early -- it must never crash the gRPC handler thread.
triggerRebuild ::
  Chan Event ->
  Build ->
  Project ->
  Proto Instr.RebuildRequest ->
  IO (Proto Instr.Empty)
triggerRebuild chan build project req = do
  let targetText = Text.unpack req.target
  case parseTarget project targetText of
    Left err ->
      emitLog (Just chan) targetText "error" ("Rejected rebuild request: " ++ err)
    Right steps ->
      scheduleBatch build ScheduleRequest {steps, recompile = True, rebuild = True}
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
-- Fire-and-forget, like 'triggerRebuild': does not await completion. A malformed target string is rejected the
-- same way 'triggerRebuild' rejects one -- logged on the instrument channel, never crashing the handler thread.
triggerExecute ::
  Chan Event ->
  Build ->
  Project ->
  Proto Instr.RebuildRequest ->
  IO (Proto Instr.Empty)
triggerExecute chan build project req = do
  let targetText = Text.unpack req.target
  case parseTarget project targetText of
    Left err ->
      emitLog (Just chan) targetText "error" ("Rejected execute request: " ++ err)
    Right steps ->
      scheduleBatch build ScheduleRequest {steps = map toExecuteStep steps, recompile = True, rebuild = True}
  pure defMessage
  where
    toExecuteStep (name, unitReq) = (name, executeVariant unitReq)

    executeVariant = \case
      UnitModules mods -> UnitExecuteModules mods
      _ -> UnitExecute

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
    (mkNonStreaming (triggerExecute chan build project))
    (mkNonStreaming (triggerRebuild chan build project))
