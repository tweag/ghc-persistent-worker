-- | The wire protocol between a GHC server and an executor subprocess.
module Types.Executor.Protocol where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Types.Api (ExecutorStatus, OutputStream, RequestId)

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
