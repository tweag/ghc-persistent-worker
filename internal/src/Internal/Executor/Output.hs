{-# LANGUAGE RankNTypes #-}

-- | Task attribution of the output written to stdio in an executor.
--
-- The handles are replaced by "Internal.Executor.CallbackHandle"s whose callback looks up the writing thread in a
-- registry of threads that run a task.
-- Complete lines are published to the subscribers of the executor's @Events@ stream, tagged with the task, and
-- everything a task writes is collected, to be returned in the 'Captured' part of its result.
--
-- Output from threads that aren't registered (e.g. ones forked by the evaluated code, or the executor's own) is
-- published without a task.
-- All output is also copied to the original handles, so that the executor's real stdio shows everything.
--
-- TODO refactor, reword, make optional. maybe not necessary to include in the initial set of changes
module Internal.Executor.Output where

import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as Char8

-- | The remainder and the complete lines of the bytes.
splitLines :: ByteString -> (ByteString, [ByteString])
splitLines bytes = (rest, Char8.lines complete)
  where
    (complete, rest) = ByteString.breakEnd (== 10) bytes
