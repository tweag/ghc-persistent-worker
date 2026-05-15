-- | Path utilities shared across GHC server modules.
module GhcServer.Path (fromOsPath, toOsPath, outputDirName, tmpDirName, socketDirName, socketPath) where

import System.OsPath.Extra (OsPath, (</>), fromOsPath, toOsPath)

-- | Directory names under the project root for server artifacts.
outputDirName, tmpDirName, socketDirName :: OsPath
outputDirName = toOsPath "output"
tmpDirName = toOsPath "tmp"
socketDirName = toOsPath "socket"

-- | The Unix socket path for the server, placed under the project root.
socketPath :: OsPath -> OsPath
socketPath projectRoot = do
  let socketFile = toOsPath "server.sock"
   in projectRoot </> socketDirName </> socketFile
