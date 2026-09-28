module Ghc.Ui.Data.Executors where

import Brick.Widgets.List (GenericList, list, listElements, listReplace, listSelected)
import Control.Applicative ((<|>))
import qualified Data.Sequence as Seq
import Data.Sequence (Seq)
import Ghc.Ui.Data.Name (Name (..))
import GHC.Generics (Generic)
import Types.Api (ExecutorId, ExecutorStatus)

-- | One running executor, as last reported by the server in an 'Types.Api.ExecutorUpdate' event.
data ExecutorRow =
  ExecutorRow {
    executor :: ExecutorId,
    pid :: Maybe Int,
    status :: ExecutorStatus,
    -- | Set when the user requested termination; the row is removed once 'Types.Api.ExecutorGone' arrives.
    killing :: Bool
  }
  deriving stock (Eq, Show)

newtype ExecutorsState =
  ExecutorsState {
    rows :: GenericList Name Seq ExecutorRow
  }
  deriving stock (Generic)

initialState :: ExecutorsState
initialState =
  ExecutorsState {rows = list Executors Seq.empty 1}

-- | Replace the elements of the list, keeping the selection (or selecting the first row if there was none).
replaceRows :: Seq ExecutorRow -> GenericList Name Seq ExecutorRow -> GenericList Name Seq ExecutorRow
replaceRows elements rows =
  listReplace elements (listSelected rows <|> Just 0) rows

-- | Insert the executor's row or update the existing one, preserving the 'killing' marker.
upsert :: ExecutorId -> Maybe Int -> ExecutorStatus -> ExecutorsState -> ExecutorsState
upsert executor pid status state@ExecutorsState {rows} =
  state {rows = replaceRows updated rows}
  where
    updated = case Seq.findIndexL ((== executor) . (.executor)) current of
      Just index -> Seq.adjust' (\ row -> row {pid, status}) index current
      Nothing -> current Seq.|> ExecutorRow {executor, pid, status, killing = False}

    current = listElements rows

remove :: ExecutorId -> ExecutorsState -> ExecutorsState
remove executor state@ExecutorsState {rows} =
  state {rows = replaceRows (Seq.filter ((/= executor) . (.executor)) (listElements rows)) rows}
