{-# LANGUAGE DeriveAnyClass #-}

module Types.Instrument where

import Data.Aeson (FromJSON, ToJSON)
import Data.Binary (Binary)
import Data.Map (Map)
import GHC.Generics (Generic)
import Types.State (Options)

-- | A unit and its module names, as discovered by @ghc-server@'s project discovery (unit directories /
-- @unit.json@ files, or @.cabal@ parsing) at startup. Module names come from source file basenames, not from a
-- computed module graph -- this is available immediately on connection, before any metadata or compilation runs.
data UnitSummary
  = UnitSummary { unitName :: String, modules :: [String] }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)

-- | Cache-tracking info for a single lazily-loaded bytecode-cache entry, mirroring the @BcoCacheEntry@ proto
-- message. Unlike that message, values of this type are constructed directly from 'Types.State.Make.MakeState'
-- (see @GhcWorker.Grpc.bytecodeEntries@) and are used both to construct 'BytecodeSnapshot' events.
data BcoEntryInfo
  = BcoEntryInfo
      { unitId :: String
      , moduleName :: String
      , size :: Int
      , lastAccess :: Int
      , resident :: Bool
      , pendingEviction :: Bool
      }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary, FromJSON, ToJSON)

-- | Kind of task to trigger for a target: an ordinary recompile, or compiling and then executing @main@.
-- Merges the former separate @TriggerRebuild@\/@TriggerExecute@ RPCs into a single parameterized operation.
data TaskKind = Rebuild | Execute
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary, FromJSON, ToJSON)

-- | Request to trigger a build task for a target, replacing the identically-shaped @RebuildRequest@ payload
-- that the former @TriggerRebuild@\/@TriggerExecute@ RPCs both used.
data TaskTrigger
  = TaskTrigger
      { target :: String
      , task :: TaskKind
      , rebuild :: Bool
      }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary, FromJSON, ToJSON)

-- | Request to evict a module (or, if 'moduleName' is empty, an entire unit) from the bytecode cache. Replaces
-- the former @EvictBytecodeRequest@ proto message.
data EvictRequest
  = EvictRequest { unitId :: String, moduleName :: String }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary, FromJSON, ToJSON)

-- | The single sum type all non-streaming 'Instrument' RPC payloads are mapped to, JSON-encoded into the
-- unified @Send@ RPC's @Command@ message. Each constructor wraps the pure data type corresponding to the
-- request message the given operation used before the RPCs were merged.
data Command
  = SetOptions Options
  | TriggerTask TaskTrigger
  | EvictBytecode EvictRequest
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | The response counterpart to 'Command', JSON-encoded into the @Send@ RPC's @CommandResponse@ message.
-- 'Ack' answers every command.
data Response
  = Ack
  | BytecodeState [BcoEntryInfo]
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data Event
  = CompileStart { target :: String, canDebug :: Bool, requestId :: Int }
  | CompileEnd { target :: String, exitCode :: Int, stderr :: String, result :: Maybe String, requestId :: Int }
  | Stats { memory :: Map String Int, cpuNs :: Int, gcCpuNs :: Int }
  -- | The project's units and modules, sent once when a client connects to the Instrument service. Populates the
  -- @instrument@ app's task tree view immediately, ahead of any build activity.
  | ProjectStructure { units :: [UnitSummary] }
  -- | A snapshot of the lazily-loaded bytecode cache, pushed to the @instrument@ UI whenever it may have changed
  -- (after a compile\/metadata\/execute task finishes), rather than fetched on demand by the UI. See
  -- @GhcWorker.Grpc.pushBytecodeState@.
  | BytecodeSnapshot { entries :: [BcoEntryInfo] }
  -- | A single log message captured from the server's 'Types.Log.Logger' (debug\/info\/fatal messages and GHC's
  -- own diagnostic output), tagged with the unit\/module target that was active when it was emitted and a
  -- millisecond epoch timestamp. Only emitted when the @instrument@ feature is enabled (see
  -- @GhcServer.Log.instrumentLogger@).
  | LogMessage { target :: String, level :: String, message :: String, timestampMs :: Integer }
  -- | A single pipeline phase (@Hsc@\/@HscPostTc@\/@HscBackend@) firing for a target's compilation, emitted via
  -- GHC's @runPhaseHook@ (see @Internal.Compile.Make.withPhaseEvents@). Named @PhaseEvent@ rather than @Phase@ to
  -- avoid clashing with @GhcServer.Scheduler@'s unrelated @Phase@ type when both are imported unqualified.
  | PhaseStart { target :: String, phase :: String, requestId :: Int }
  | PhaseEnd { target :: String, durationMs :: Word, requestId :: Int }
  -- | Sent once the scheduler's queue has fully drained -- i.e. every in-flight UI-triggered build request (see
  -- @GhcServer.Grpc.triggerTask@) has completed, not just the one that happened to trigger this event. A single
  -- request fanned out into several concurrent scheduler batches (e.g. a project-wide build across multiple
  -- units) therefore produces exactly one of these, rather than one per batch.
  | RequestCompleted { statusMessage :: String }
  | Halt
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)
