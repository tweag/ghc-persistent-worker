-- | Types for the bytecode plans that the parent sends to interface-free persistent executors.
-- See 'GhcServer.Build.ExecutorPlan'.
module GhcServer.Data.ExecutorPlan where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Fingerprint (Fingerprint)
import GHC.Generics (Generic)
import GHC.Unit.Types (UnitId)
import Types.State.Executor (HomeModuleId, SegmentSlice (..))

-- | A home module in the transitive closure of an execution, with a reference to its serialized bytecode.
data PlannedModule =
  PlannedModule {
    home :: HomeModuleId,
    -- | Content hash of the payload, used to decide whether an executor has to reload the module.
    hash :: Fingerprint,
    -- | Direct home-module dependencies, taken from the module's interface.
    deps :: [HomeModuleId],
    -- | The module's bytecode in the format of a @.gbc@ file (@GHC.ByteCode.Serialize.writeBinByteCode@).
    payload :: Payload
  }
  deriving stock (Eq)

-- | Where the child finds a module's serialized bytecode.
data Payload =
  -- | A @.gbc@ file.
  PayloadFile FilePath
  |
  -- | A range in a shared payload segment (see 'GhcServer.Build.PayloadArena').
  PayloadShared SegmentSlice
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | How the parent transfers payloads to executors.
data PayloadTransport =
  -- | Point the child to existing @.gbc@ files, writing those of modules without one to the given directory.
  PayloadFiles FilePath
  |
  -- | Append all payloads to the shared segment at the given path.
  PayloadSegment FilePath
  deriving stock (Eq, Show)

-- | The modules an executor has to load (and reload) for an execution, as determined by
-- 'GhcServer.Build.ExecutorPlan.planDelta'.
data PlanDelta =
  PlanDelta {
    -- | Modules to link, dependencies first: those the executor doesn't have, those whose hash changed, and those
    -- depending on one of these that were loaded before.
    send :: [PlannedModule],
    -- | Loaded modules outside the execution's closure that depend on a replaced module.
    -- They are dropped from the loaded set, so they will be resent when they are needed next time.
    invalidated :: [HomeModuleId]
  }
  deriving stock (Eq)

-- | Everything an executor needs to link the bytecode for one execution.
data ExecutorPlan =
  ExecutorPlan {
    -- | Package units whose libraries must be loaded, i.e. the direct package dependencies of the whole closure.
    -- The loader resolves their transitive dependencies itself.
    units :: [UnitId],
    -- | Modules to load, dependencies first.
    modules :: [PlannedModule],
    -- | Sequence number of this plan for the executor, from which the child derives linkable times.
    generation :: Int,
    -- | Native libraries required by the home units of the closure (e.g. @extra-libraries@).
    extraLibs :: [ExtraLib]
  }
  deriving stock (Eq)

-- | Where a payload lives, for logs.
describePayload :: Payload -> String
describePayload = \case
  PayloadFile path -> "file " ++ path
  PayloadShared slice -> "shared " ++ slice.segment ++ "@" ++ show slice.offset ++ "+" ++ show slice.size

-- | One-line summary of a plan, for logs.
describePlan :: ExecutorPlan -> String
describePlan plan =
  "plan generation " ++ show plan.generation ++ ": " ++ show (length plan.modules) ++ " modules"
  ++ foldMap (("\n  " ++) . describePayload . (.payload)) plan.modules

-- | A native library that the executor loads before linking bytecode that may reference it.
data ExtraLib =
  ExtraLib {
    -- | Directories to search.
    paths :: [FilePath],
    -- | Library name, as in @-l<name>@.
    name :: String
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)


-- | How the child runs the entry binding.
data EntryKind =
  -- | @IO ()@; the result is what it printed.
  EntryIO
  |
  -- | @IO String@; the result is the returned string.
  EntryString
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | The wire form of an 'ExecutorPlan' plus the entry point, sent to a persistent executor.
-- The child needs neither hashes nor dependencies, since the parent orders the payloads and tracks what is loaded.
data PlanRequest =
  PlanRequest {
    -- | GHC options for the child's session. Only the first request's options take effect; a request with different
    -- options is rejected, and the parent has to respawn the executor.
    ghcOptions :: [String],
    units :: [String],
    -- | Payloads to link, dependencies first.
    payloads :: [Payload],
    -- | Linkable time for this plan's modules; must increase with every plan sent to the same process.
    generation :: Int,
    -- | Native libraries to load before linking; the child skips those it loaded before.
    extraLibs :: [ExtraLib],
    entryUnit :: String,
    entryModule :: String,
    entry :: String,
    entryKind :: EntryKind
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)
