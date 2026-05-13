-- | Path utilities shared across GHC server modules.
module GhcServer.Path (fromOsPath, toOsPath, outputDirName, tmpDirName, socketDirName, socketPath) where

import System.OsPath (OsPath, (</>))
import System.OsPath.Extra (fromOsPath, toOsPath)

-- | Directory names under the project root for server artifacts.
outputDirName, tmpDirName, socketDirName :: OsPath
outputDirName = toOsPath "output"
tmpDirName = toOsPath "tmp"
socketDirName = toOsPath "socket"

-- | The Unix socket path for the server, placed under the project root.
socketPath :: OsPath -> FilePath
socketPath projectRoot = do
  let socketFile = toOsPath "server.sock"
   in fromOsPath (projectRoot </> socketDirName </> socketFile)
