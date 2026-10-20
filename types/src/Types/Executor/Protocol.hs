-- | The wire protocol between a GHC server and an executor subprocess.
module Types.Executor.Protocol where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

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
