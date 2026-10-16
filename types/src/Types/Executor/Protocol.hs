-- | The wire protocol between a parent and a persistent executor subprocess (see
-- 'Types.State.Executor.ExecutorHandle'), which is also the result format of the @ghc-server@'s one-shot
-- execute-task children.
module Types.Executor.Protocol where

import Data.Aeson (FromJSON, ToJSON, Value)
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import Types.Api (ProcessStats)
import Types.ExecutorPlan (PlanRequest)

-- | The payload of the executor's @Execute@ RPC.
data ExecutorCall =
  -- | Link the parent's bytecode plan in an interface-free session and run the entry (GBC only).
  CallPlan PlanRequest
  |
  -- | Restore a unit from the cache and run a module like a one-shot child.
  -- The payload is opaque to the generic executor code and decoded by the host that handles module calls.
  CallModule Value
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | The result of running the entry, once the child was set up to do so.
data EvalOutcome =
  -- | The entry ran to completion, optionally carrying the value that it exfiltrated (see
  -- 'Internal.Evaluate.executeMain').
  EvalSucceeded (Maybe Text)
  |
  -- | The module has no entry; the scheduler treats this as a skip rather than a failure.
  EvalNoEntry
  |
  -- | Evaluation was attempted and failed (session setup, a precondition, or the entry itself).
  EvalFailed Text
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Reasons for which the child couldn't attempt to run the entry.
data SetupError =
  -- | The call could not be decoded.
  InvalidCall Text
  |
  -- | The child could not decode or link the bytecode of a 'PlanRequest'.
  -- Its loaded state is unknown afterwards, so the parent forgets what it recorded as loaded.
  LoadFailed Text
  |
  -- | The executor's plan session was started with GHC options different from the request's. Nothing was linked,
  -- but the executor can't serve the request, so the parent respawns it.
  OptionsMismatch Text
  |
  -- | The unit's cache file exists but could not be read.
  CacheUnreadable Text
  |
  -- | The unit has no cache on disk, so its module map cannot be restored.
  CacheMissing
  |
  -- | The child doesn't implement this kind of call.
  Unsupported Text
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | What an evaluation wrote to the process-global stdout and stderr handles.
data Captured =
  Captured {
    stdout :: Text,
    stderr :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | The child's answer to a call.
data CallResult =
  CallResult {
    captured :: Captured,
    -- | The messages the child's in-memory logger accumulated.
    logMessages :: [Text],
    -- | RTS memory-usage stats collected via 'GHC.Stats.getRTSStats' right before the child answers.
    stats :: ProcessStats,
    outcome :: Either SetupError EvalOutcome
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Why the parent has no 'CallResult'.
data CallError =
  -- | The call failed, e.g. because the executor died.
  Unreachable Text
  |
  -- | The child's answer could not be decoded.
  Undecodable Text
  |
  -- | The plan couldn't be computed, e.g. because a module has no bytecode. Nothing was sent.
  PlanUnavailable Text
  deriving stock (Eq, Show)

displayCallError :: CallError -> String
displayCallError = \case
  Unreachable err -> "Executor unreachable: " ++ Text.unpack err
  Undecodable err -> "Could not decode the child's result: " ++ Text.unpack err
  PlanUnavailable err -> "No bytecode plan: " ++ Text.unpack err
