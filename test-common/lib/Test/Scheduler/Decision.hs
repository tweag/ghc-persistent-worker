module Test.Scheduler.Decision where

import Control.Concurrent.STM (STM, TVar, stateTVar)
import Control.Monad.Trans.Writer.Strict (Writer, runWriter, tell)
import Data.Monoid (Endo (..))
import Test.Data.Scheduler (SchedulerDecision (..))

-- | A computation that records the scheduling decisions it makes, in chronological order.
type Traced key = Writer (Endo [SchedulerDecision key])

-- | Record a decision.
traceDecision :: SchedulerDecision key -> Traced key ()
traceDecision decision =
  tell (Endo (decision :))

-- | Run a traced computation, returning its result and the decisions it recorded, in chronological order.
runTraced :: Traced key a -> (a, [SchedulerDecision key])
runTraced traced =
  (a, appEndo decisions [])
  where
    (a, decisions) = runWriter traced

-- | Run a traced computation, discarding the decisions it recorded.
evalTraced :: Traced key a -> a
evalTraced =
  fst . runTraced

-- | Atomically replace the contents of a variable with the result of a traced transition and return the decisions
-- recorded by it, in chronological order.
--
-- The decisions are returned rather than reported so that the caller can do so in 'IO' after the transaction has
-- committed.
modifyTraced ::
  TVar state ->
  (state -> Traced key state) ->
  STM [SchedulerDecision key]
modifyTraced var transition =
  stateTVar var \ state ->
    case runTraced (transition state) of
      (new, decisions) -> new `seq` (decisions, new)
