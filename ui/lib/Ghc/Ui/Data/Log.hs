module Ghc.Ui.Data.Log where

import Data.Sequence (Seq)
import Data.Text (Text)
import GHC.Generics (Generic)

-- | A session log message, either received from the server or emitted as for debugging by the UI.
data LogMessage =
  LogMessage {
    category :: Text,
    level :: Text,
    message :: Text,
    timestampMs :: Integer
  }
  deriving stock (Eq, Show, Generic)

data LogState =
  LogState {
    messages :: Seq LogMessage,
    -- | Index into 'entries' of the currently selected row, moved by 'j'/'k'. Independent of the viewport's
    -- scroll offset, which Brick tracks internally for the 'LogViewer' name and which 'd'/'u' adjust directly.
    selected :: Int
  }
  deriving stock (Generic)

initialState :: LogState
initialState =
  LogState {messages = [], selected = 0}
