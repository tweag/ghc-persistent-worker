-- | The wire protocol between a parent and a persistent executor subprocess (see
-- 'Types.State.Executor.ExecutorHandle'), which is also the result format of the @ghc-server@'s one-shot
-- execute-task children.
module Types.Executor.Protocol where

import Data.Aeson (FromJSON, ToJSON, Value)
import Data.Text (Text)
import GHC.Generics (Generic)
import Types.Api (ExecutorStatus, OutputStream, ProcessStats, RequestId)

-- | The payload of the executor's @Execute@ RPC: a call, and the task on whose behalf it runs, to which the child
-- attributes the output written during the call.
data ExecutorRequest =
  ExecutorRequest {
    requestId :: RequestId,
    call :: ExecutorCall
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | A line the executor's process wrote to stdout or stderr, with the task it was attributed to, if any.
data OutputLine =
  OutputLine {
    task :: Maybe RequestId,
    stream :: OutputStream,
    line :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | The elements of the executor's @Events@ stream (JSON-encoded).
data ExecutorEvent =
  StatusEvent ExecutorStatus
  |
  OutputEvent OutputLine
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | The payload of the executor's @Execute@ RPC.
data ExecutorCall =
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
  deriving stock (Eq, Show)
