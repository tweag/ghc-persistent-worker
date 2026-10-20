-- | The wire protocol between a GHC server and an executor subprocess.
module Types.Executor.Protocol where

import Data.Aeson (FromJSON, ToJSON, Value)
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import Types.Api (ExecutorStatus, OutputStream, MemoryUsage, RequestId)

-- | Event type for executor requests.
--
-- TODO This is a weird encoding.
data ExecutorCall =
  -- | Execute a module.
  CallModule Value
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | The payload of the @Execute@ method of the gRPC service @executor@.
data ExecutorRequest =
  ExecutorRequest {
    requestId :: RequestId,
    call :: ExecutorCall
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | A line written to stdio by an executor process.
-- If possible, the corresponding task is determined by thread ID.
data OutputLine =
  OutputLine {
    task :: Maybe RequestId,
    stream :: OutputStream,
    line :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | The payload of the streaming @Events@ method of the gRPC service @executor@.
data ExecutorEvent =
  StatusEvent ExecutorStatus
  |
  OutputEvent OutputLine
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | The result of executing a module in an executor process.
data EvalOutcome =
  -- | Success with an optional return value.
  EvalSucceeded { result :: Maybe Text }
  |
  -- | The module is unknown to the executor.
  EvalNoEntry
  |
  -- | Failure.
  EvalFailed { message :: Text }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Reasons for which an executor couldn't start a task.
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
  -- | The executor doesn't implement this kind of call.
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

-- | The result of the @Execute@ method of the gRPC service @executor@.
data CallResult =
  CallResult {
    captured :: Captured,
    -- | Messages the handler sent to the 'Types.Log.Logger'.
    logMessages :: [Text],
    -- | The maximum memory usage during the task's runtime.
    maxMemory :: MemoryUsage,
    outcome :: Either SetupError EvalOutcome
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Why the parent has no 'CallResult'.
--
-- TODO unclear, reword later.
data CallError =
  -- | The call failed, e.g. because the executor died.
  Unreachable Text
  |
  -- | The child's answer could not be decoded.
  Undecodable Text
  deriving stock (Eq, Show)

-- TODO String
displayCallError :: CallError -> String
displayCallError = \case
  Unreachable err -> "Executor unreachable: " ++ Text.unpack err
  Undecodable err -> "Could not decode the child's result: " ++ Text.unpack err
