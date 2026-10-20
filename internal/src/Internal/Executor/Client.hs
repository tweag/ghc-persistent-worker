-- | Logic for controlling executor processes.
module Internal.Executor.Client where

import Control.Exception (SomeException, try)
import Control.Monad (void)
import System.Directory.OsPath (removeFile)
import System.OsPath.Extra (OsPath)
import Types.Settings (Settings (..))

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

-- TODO removePathForcibly?
removeIfExists :: OsPath -> IO ()
removeIfExists path =
  void (try @SomeException (removeFile path))
