module Ghc.Ui.Event.Executors where

import Brick (EventM)
import Brick.Widgets.List (listSelectedElementL)
import Control.Lens ((%%=))
import Data.Monoid (First (..))
import Ghc.Ui.Data.Executors (ExecutorRow (..), ExecutorsState (..))
import Ghc.Ui.Data.Name (Name)
import Types.Api (ExecutorId)

-- | Mark the selected row as being terminated and return its id, so the caller can send the request.
-- Optimistic: the row is only removed once the server reports 'Types.Api.ExecutorGone'.
markSelectedKilling :: EventM Name ExecutorsState (Maybe ExecutorId)
markSelectedKilling =
  fmap getFirst $ #rows . listSelectedElementL %%= \ row ->
    (First (Just row.executor), row {killing = True})
