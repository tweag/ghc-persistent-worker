-- | Logic for controlling executor processes.
module Internal.Executor.Client where

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
