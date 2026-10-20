-- | The wire protocol between a parent and a persistent executor subprocess (see
-- 'Types.State.Executor.ExecutorHandle'), which is also the result format of the @ghc-server@'s one-shot
-- execute-task children.
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
