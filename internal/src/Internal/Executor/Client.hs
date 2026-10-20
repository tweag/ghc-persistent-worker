-- | Logic for controlling executor processes.
module Internal.Executor.Client where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, throwIO, try)
import Control.Monad (void)
import Control.Monad.Extra (loopM)
import Data.Foldable (traverse_)
import System.Directory.OsPath (doesFileExist, removeFile)
import System.OsPath.Extra (OsPath, fromOsPath)
import System.Process (CreateProcess (..), StdStream (..), createProcess, getProcessExitCode, proc)
import Types.Settings (Settings (..))
import Types.State.Executor (ExecutorHandle (..), newCallLock)

-- | Wrap RTS options in @+RTS ... -RTS@, or nothing if there are none.
rtsArgs :: [String] -> [String]
rtsArgs = \case
  [] -> []
  opts -> ["+RTS"] ++ opts ++ ["-RTS"]

captureStdioArg :: String
captureStdioArg = "--capture-stdio"

-- | Additional CLI arguments for executors for the given settings.
captureStdioArgs :: Settings -> [String]
captureStdioArgs settings =
  [captureStdioArg | settings.executorCaptureStdio]

-- | Launch the given executable as an executor child on the socket, with extra arguments (e.g. RTS options).
--
-- TODO typed-process
spawnExecutorWith :: FilePath -> [String] -> OsPath -> IO ExecutorHandle
spawnExecutorWith exe extraArgs socket = do
  removeIfExists socket
  calls <- newCallLock
  (mStdin, _, _, process) <- createProcess (proc exe args) {std_in = CreatePipe}
  case mStdin of
    Just stdin -> pure ExecutorHandle {process, socket, stdin, memory = Nothing, calls, incarnation = 0}
    Nothing -> throwIO (userError "Executor was spawned without a stdin pipe")
  where
    args = ["executor", "--socket", fromOsPath socket] ++ extraArgs

-- TODO removePathForcibly?
removeIfExists :: OsPath -> IO ()
removeIfExists path =
  void (try @SomeException (removeFile path))

isRunning :: ExecutorHandle -> IO Bool
isRunning handle =
  null <$> getProcessExitCode handle.process

-- | Poll until the executor has bound its socket, failing if it exits beforehand or takes longer than 10 seconds.
awaitExecutor :: ExecutorHandle -> IO ()
awaitExecutor handle = do
  status <- loopM check (100 :: Int)
  traverse_ (throwIO . userError) status
  where
    check = \case
      0 ->
        pure (Right (Just "Executor did not open its socket within 10 seconds"))
      n ->
        isRunning handle >>= \case
          False ->
            pure (Right (Just "Executor exited during startup"))
          True ->
            doesFileExist handle.socket >>= \case
              True -> pure (Right Nothing)
              False -> Left (n - 1) <$ threadDelay 100_000
