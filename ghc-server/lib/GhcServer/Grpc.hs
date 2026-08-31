-- | gRPC 'Instrument' service for the standalone GHC server.
--
-- Reuses most of the worker's 'GhcWorker.Grpc' handlers (which only depend on 'Types.State.WorkerState'), but
-- replaces 'triggerRebuild': the worker's version looks up cached Buck target args, which 'ghc-server' never
-- populates. Instead, the rebuild request's target text is parsed and scheduled the same way 'ghc-client' does.
module GhcServer.Grpc where

import Common.Grpc ()
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (Chan, writeChan)
import Control.Concurrent.MVar (MVar)
import Control.Concurrent.STM (atomically, modifyTVar', readTVar)
import Control.Monad (void, when)
import Data.Binary (encode)
import Data.ByteString (toStrict)
import Data.Functor (($>))
import qualified Data.Map.Strict as Map
import GhcServer.Build (Build (..), awaitBuild, scheduleBatch)
import GhcServer.Data.Request (ScheduleRequest (..), UnitRequest (..))
import GhcServer.Data.Unit (Project (..), Unit (..), UnitName (..))
import GhcServer.Handler (parseTarget)
import GhcServer.Log (emitLog)
import GhcServer.Path (fp)
import GhcWorker.Grpc qualified as Worker
import Network.GRPC.Common (NextElem (NextElem))
import Network.GRPC.Common.Protobuf (Proto, defMessage, (&), (.~))
import Network.GRPC.Server.Protobuf (ProtobufMethodsOf)
import Network.GRPC.Server.StreamType (Methods, mkNonStreaming, mkServerStreaming, simpleMethods)
import qualified Proto.Instrument as Instr
import Proto.Instrument (Instrument)
import Proto.Instrument_Fields qualified as Instr
import System.FilePath (takeBaseName)
import Types.Instrument (Event (..), TaskKind (..), TaskTrigger (..), UnitSummary (..))
import Types.Instrument qualified as Instrument
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

-- | Trigger a build task (recompile, or execute @main@) for the given target, parsed and scheduled the same way
-- 'ghc-client' would from its argv (@unitName@, @unitName:metadata@, @unitName:modules@, @unitName:ModuleName@, or
-- the @"*"@ wildcard for every unit in the project). Fire-and-forget: does not wait for completion, matching the
-- semantics of the worker's own 'GhcWorker.Grpc.triggerTask'. Merges the former separate
-- 'triggerRebuild'\/'triggerExecute' handlers into a single operation parameterized on 'TaskKind'.
--
-- @recompile@ is derived from the same @rebuild@ flag the caller passes (the instrument UI's 'b'\/'m'\/'r'\/'x'
-- keys, or @ghc-client@'s @--rebuild@): an ordinary press (@rebuild = False@) relies purely on the Phase 0\/2
-- staleness analysis, so a repeated identical request against unchanged sources is a genuine no-op (no
-- metadata rerun, no recompile, no re-execute of @main@'s dependencies). Only a forced @rebuild = True@ request
-- also forces @recompile = True@, pushing the named targets' entire module set into the stale closure
-- unconditionally (see 'GhcServer.Build.Classify.classifyBuildRequest'\'s @forceAll@) -- this used to be
-- hardcoded to @True@ unconditionally for both kinds, which meant every UI-triggered request (including
-- ordinary, unchanged repeats) always recompiled its named targets regardless of the @rebuild@ flag; fixed to
-- track @rebuild@ instead. For 'Rebuild', @rebuild@ additionally discards the stored source-digest record
-- first, forcing every source in scope to be treated as changed -- this is the \'force full rebuild\' request,
-- as opposed to an ordinary incremental recompile. 'Execute' rewrites every step to its execute-variant, using
-- the same target grammar with an added @:execute@ selector\/@"*"@ sentinel:
--
-- * @"*"@ (project-root node selected) -- execute every module of every unit in the project.
-- * @unitName@ or @unitName:execute@ (unit header row selected) -- execute every module of that unit.
-- * @unitName:moduleName@ (module row selected) -- execute only that module.
--
-- Dispatched as an ordinary scheduler batch ('GhcServer.Build.Schedule.ExecuteModule' tasks, depending on their
-- module's compile task), rather than a raw 'forkIO'\/'Control.Concurrent.Async.forConcurrently_' fan-out. A
-- malformed target string is rejected the same way for both kinds -- logged on the instrument channel, never
-- crashing the handler thread.
--
-- Being fire-and-forget only means the gRPC response doesn't wait for the batch: the batch's completion is
-- still awaited on a background thread so that 'GhcServer.Build.awaitBuild' runs its post-batch bookkeeping
-- (committing Phase 0's source-digest records via 'GhcServer.Build.Diff.commitDigests', which is otherwise
-- only triggered by the @ghc-client@\/Buck request path's optional @--wait@ flag). Without this, every source
-- file touched by a UI-triggered build would be re-detected as \"changed\" on every subsequent build, forcing
-- a full unnecessary recompile of the same modules on every request.
--
-- 'Instrument.RequestCompleted' is only ever emitted once 'Build.inFlight' drops back to zero, i.e. once every
-- concurrently in-flight 'triggerTask' request (not just this one) has drained -- a single UI action fanned out
-- into several concurrent requests (e.g. a project-wide build across multiple units, dispatched as one
-- 'TriggerBuild' per module by the UI's \'b\' key) therefore surfaces exactly one completion indicator instead
-- of one per fanned-out request.
triggerTask ::
  Chan Event ->
  Build ->
  Project ->
  TaskTrigger ->
  IO ()
triggerTask chan build project TaskTrigger{target, task, rebuild} =
  case parseTarget project target of
    Left err ->
      emitLog (Just chan) target "error" ("Rejected " ++ label ++ " request: " ++ err)
    Right steps -> do
      atomically $ modifyTVar' build.inFlight (+ 1)
      scheduleBatch build (request steps)
      void $ forkIO do
        _ <- awaitBuild build
        remaining <- atomically do
          modifyTVar' build.inFlight (subtract 1)
          readTVar build.inFlight
        when (remaining == 0) $
          writeChan chan (RequestCompleted "All tasks concluded")
  where
    label = case task of
      Rebuild -> "rebuild"
      Execute -> "execute"

    request steps = case task of
      Rebuild -> ScheduleRequest {steps, recompile = rebuild, rebuild}
      Execute -> ScheduleRequest {steps = map toExecuteStep steps, recompile = rebuild, rebuild}

    toExecuteStep (name, unitReq) = (name, executeVariant unitReq)

    executeVariant = \case
      UnitModules mods -> UnitExecuteModules mods
      _ -> UnitExecute

-- | Dispatch a single decoded 'Instrument.Command' to the appropriate handler, reusing 'GhcWorker.Grpc''s
-- 'setOptions'\/'evictBytecode'\/'getBytecodeState' (which only depend on 'WorkerState') and this module's own
-- 'triggerTask' (which needs the scheduler and parsed project, unlike the worker's target-args lookup).
runCommand ::
  Chan Event ->
  Build ->
  Project ->
  MVar WorkerState ->
  Instrument.Command ->
  IO Instrument.Response
runCommand chan build project stateVar = \case
  Instrument.SetOptions opts -> Worker.setOptions stateVar opts $> Instrument.Ack
  Instrument.TriggerTask trigger -> triggerTask chan build project trigger $> Instrument.Ack
  Instrument.EvictBytecode req -> Worker.evictBytecode stateVar req $> Instrument.Ack

-- | A grapesy server that streams instrumentation data and dispatches every other 'Instrument' operation through
-- the unified @Send@ RPC, backed by 'ghc-server'\'s persistent 'WorkerState' and scheduler.
instrumentMethods ::
  Chan Event ->
  MVar WorkerState ->
  Build ->
  Project ->
  Methods IO (ProtobufMethodsOf Instrument)
instrumentMethods chan stateVar build project =
  simpleMethods
    (mkServerStreaming (const (notifyMe project stateVar chan)))
    (mkNonStreaming (Worker.handleCommand (runCommand chan build project stateVar)))
