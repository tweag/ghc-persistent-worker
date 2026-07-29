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

data Event
  = CompileStart { target :: String, canDebug :: Bool }
  | CompileEnd { target :: String, exitCode :: Int, stderr :: String }
  | Stats { memory :: Map String Int, cpuNs :: Int, gcCpuNs :: Int }
  -- | The project's units and modules, sent once when a client connects to the Instrument service. Populates the
  -- @instrument@ app's task tree view immediately, ahead of any build activity.
  | ProjectStructure { units :: [UnitSummary] }
  | Halt
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)
