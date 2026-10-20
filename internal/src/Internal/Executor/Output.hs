{-# LANGUAGE RankNTypes #-}

-- | Attribution of the output written to the process-global @stdout@ and @stderr@ in a persistent executor.
--
-- The handles are replaced by "Internal.Executor.CallbackHandle"s whose callback looks up the writing thread in a
-- registry of threads that run a task.
-- Complete lines are published to the subscribers of the executor's @Events@ stream, tagged with the task, and
-- everything a task writes is collected, to be returned in the 'Captured' part of its result.
--
-- Output from threads that aren't registered (e.g. ones forked by the evaluated code, or the executor's own) is
-- published without a task.
-- All output is also copied to the original handles, so that the executor's real stdio shows everything.
module Internal.Executor.Output where

import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as Char8
import Data.IORef (IORef)
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import Data.Text.Encoding.Error (lenientDecode)
import Types.Api (RequestId)

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

-- | The remainder and the complete lines of the bytes.
splitLines :: ByteString -> (ByteString, [ByteString])
splitLines bytes = (rest, Char8.lines complete)
  where
    (complete, rest) = ByteString.breakEnd (== 10) bytes

decode :: ByteString -> Text
decode = Text.decodeUtf8With lenientDecode
