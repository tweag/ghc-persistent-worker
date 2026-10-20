-- | The wire protocol between a parent and a persistent executor subprocess (see
-- 'Types.State.Executor.ExecutorHandle'), which is also the result format of the @ghc-server@'s one-shot
-- execute-task children.
module Types.Executor.Protocol where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

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
