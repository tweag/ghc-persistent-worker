{-# LANGUAGE DeriveAnyClass #-}
module Types.Instrument where

import Data.Binary (Binary)
import Data.Map (Map)
import GHC.Generics (Generic)

-- | A unit and its module names, as discovered by @ghc-server@'s project discovery (unit directories /
-- @unit.json@ files, or @.cabal@ parsing) at startup. Module names come from source file basenames, not from a
-- computed module graph -- this is available immediately on connection, before any metadata or compilation runs.
data UnitSummary
  = UnitSummary { unitName :: String, modules :: [String] }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)

-- | Cache-tracking info for a single lazily-loaded bytecode-cache entry, mirroring the @BcoCacheEntry@ proto
-- message. Unlike that message, values of this type are constructed directly from 'Types.State.Make.MakeState'
-- (see @GhcWorker.Grpc.bytecodeEntries@) and are used both to build the @GetBytecodeState@ RPC's response and to
-- construct 'BytecodeSnapshot' events.
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
  deriving anyclass (Binary)

data Event
  = CompileStart { target :: String, canDebug :: Bool }
  | CompileEnd { target :: String, exitCode :: Int, stderr :: String, result :: Maybe String }
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
  | Halt
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)
