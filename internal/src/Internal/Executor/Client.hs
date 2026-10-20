-- | Logic for controlling executor processes.
module Internal.Executor.Client where

import Control.Exception (SomeException, throwIO, try)
import Control.Monad (void)
import System.Directory.OsPath (removeFile)
import System.OsPath.Extra (OsPath, fromOsPath)
import System.Process (CreateProcess (..), StdStream (..), createProcess, proc)
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
