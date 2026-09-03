module Ghc.Ui.Data.Log where

import Brick.Widgets.List (GenericList, list)
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Ghc.Ui.Data.Name (Name (Log))

-- | A session log message, either received from the server or emitted as for debugging by the UI.
data LogMessage =
  LogMessage {
    category :: Text,
    level :: Text,
    message :: Text,
    time :: UTCTime
  }
  deriving stock (Eq, Show, Generic)

type LogState = GenericList Name Seq LogMessage

initialState :: LogState
initialState = list Log [] 1
