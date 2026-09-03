module Ghc.Ui.Data.OpLog where

import Brick.Widgets.List (GenericList, list)
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Ghc.Ui.Data.Name (Name (..))

data OpLevel =
  OpError
  |
  OpInfo
  |
  OpDebug
  deriving stock (Eq, Show)

-- | A single operational message.
data OpMessage =
  OpMessage {
    message :: Text,
    level :: OpLevel,
    time :: UTCTime
  }
  deriving stock (Eq, Show, Generic)

type OpMessages = GenericList Name Seq OpMessage

-- | All operational messages ever recorded, newest first.
data OpLogState =
  OpLogState {
    messages :: OpMessages,
    debugMessages :: OpMessages
  }
  deriving stock (Generic)

initialState :: OpLogState
initialState =
  OpLogState {
    messages = list OpLog [] 1,
    debugMessages = list OpLogDebug [] 1
  }
