-- | The wire protocol between a GHC server and an executor subprocess.
module Types.Executor.Protocol where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

-- | What an evaluation wrote to the process-global stdout and stderr handles.
data Captured =
  Captured {
    stdout :: Text,
    stderr :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)
