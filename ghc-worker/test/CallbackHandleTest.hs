module CallbackHandleTest where

import Control.Concurrent (ThreadId, forkIO, myThreadId, yield)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Monad (forM, forM_, unless)
import qualified Data.ByteString as ByteString
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import Internal.Executor.CallbackHandle (Stdio (..), withStdioCallback, writeBytes)
import System.IO (hPutStrLn, stderr)
import Types.Api (OutputStream (..))

type Output = Map (Int, OutputStream) [ByteString.ByteString]

register :: IORef (Map ThreadId Int) -> Int -> IO ()
register registry task = do
  tid <- myThreadId
  atomicModifyIORef' registry \ m -> (Map.insert tid task m, ())

-- | Run several tasks concurrently, each writing to the global @stdout@ and @stderr@, and check that the output was
-- attributed to the right tasks via the thread ID.
--
-- TODO check if this is useful
test_callbackHandle :: IO ()
test_callbackHandle = do
  registry <- newIORef Map.empty
  output <- newIORef Map.empty
  let
    sink original stream bytes = do
      tid <- myThreadId
      known <- readIORef registry
      case Map.lookup tid known of
        Just task -> atomicModifyIORef' output \ m -> (Map.insertWith (flip (<>)) (task, stream) [bytes] m, ())
        Nothing -> writeBytes (if stream == Stdout then original.out else original.err) bytes
  withStdioCallback sink do
    dones <- forM tasks \ task -> do
      done <- newEmptyMVar
      _ <- forkIO do
        register registry task
        forM_ lineNumbers \ line -> do
          putStrLn (expectedOut task line)
          yield
          hPutStrLn stderr (expectedErr task line)
          yield
        putMVar done ()
      pure done
    mapM_ takeMVar dones
    putStrLn "unregistered thread output"
  captured <- readIORef output
  let
    check task stream expected =
      let actual = ByteString.concat (Map.findWithDefault [] (task, stream) captured)
      in unless (actual == encodeUtf8 (Text.pack expected)) (fail ("Mismatch for task " <> show task <> " " <> show stream <> ":\n" <> show actual))
  forM_ tasks \ task -> do
    check task Stdout (concatMap (\ line -> expectedOut task line <> "\n") lineNumbers)
    check task Stderr (concatMap (\ line -> expectedErr task line <> "\n") lineNumbers)
  putStrLn ("OK: " <> show (Map.size captured) <> " streams attributed")
  where
    taskCount, lineCount :: Int
    tasks, lineNumbers :: [Int]
    tasks = enumFromTo 1 taskCount
    lineNumbers = enumFromTo 1 lineCount
    taskCount = 8
    lineCount = 200
    expectedOut task line = "task " <> show task <> " out " <> show line <> " \955"
    expectedErr task line = "task " <> show task <> " err " <> show line
