-- | Path utilities shared across GHC server modules.
module GhcServer.Path where

import System.OsPath (OsPath, decodeUtf, encodeUtf, unsafeEncodeUtf, (</>))

fp :: OsPath -> FilePath
fp p =
  either (error . msg) id (decodeUtf p)
  where
    msg err = "Decoding path " <> show p <> " failed: " <> show err

osPath :: String -> OsPath
osPath = unsafeEncodeUtf

-- | Directory names under the project root for server artifacts.
outputDirName, tmpDirName, socketDirName :: OsPath
outputDirName = osPath "output"
tmpDirName = osPath "tmp"
socketDirName = osPath "socket"

-- | The Unix socket path for the server, placed under the project root.
socketPath :: OsPath -> Either String FilePath
socketPath projectRoot = do
  socketFile <- onLeft "socket file" (encodeUtf "server.sock")
  onLeft "socket path" (decodeUtf (projectRoot </> socketDirName </> socketFile))
  where
    onLeft desc = \case
      Right p -> Right p
      Left e -> Left ("Failed to decode " ++ desc ++ ": " ++ show e)
