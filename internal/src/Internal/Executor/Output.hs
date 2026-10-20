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

import Control.Concurrent (ThreadId)
import Control.Concurrent.Chan (Chan)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as Char8
import Data.IORef (IORef, newIORef)
import Data.Maybe (isJust)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import Data.Text.Encoding.Error (lenientDecode)
import Types.Api (RequestId)
import Types.Executor.Protocol (ExecutorEvent (..))

-- | The buffers for one stream of one owner.
data Lane =
  Lane {
    -- | The bytes after the last newline, which are published once the line is complete.
    partial :: IORef ByteString,
    -- | All chunks in reverse order, for tasks only.
    collected :: Maybe (IORef [ByteString])
  }

-- | A task, or the unattributed output if there's no task.
data Owner =
  Owner {
    task :: Maybe RequestId,
    out :: Lane,
    err :: Lane
  }

data OutputRouter =
  OutputRouter {
    threads :: IORef (Map ThreadId Owner),
    subscribers :: IORef [Chan ExecutorEvent],
    unattributed :: Owner
  }

newLane :: Bool -> IO Lane
newLane keep = do
  partial <- newIORef ""
  collected <- if keep then Just <$> newIORef [] else pure Nothing
  pure Lane {partial, collected}

newOwner :: Maybe RequestId -> IO Owner
newOwner task = do
  out <- newLane (isJust task)
  err <- newLane (isJust task)
  pure Owner {task, out, err}

newOutputRouter :: IO OutputRouter
newOutputRouter = do
  threads <- newIORef Map.empty
  subscribers <- newIORef []
  unattributed <- newOwner Nothing
  pure OutputRouter {threads, subscribers, unattributed}

-- | The remainder and the complete lines of the bytes.
splitLines :: ByteString -> (ByteString, [ByteString])
splitLines bytes = (rest, Char8.lines complete)
  where
    (complete, rest) = ByteString.breakEnd (== 10) bytes

decode :: ByteString -> Text
decode = Text.decodeUtf8With lenientDecode
