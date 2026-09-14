-- | Path utilities shared across GHC server modules.
module GhcServer.Path where

import System.OsPath.Extra (OsPath, isExtensionOf, osp, (</>))

-- | Directory names under the project root for server artifacts.
outputDirName, tmpDirName, socketDirName, cacheDirName :: OsPath
outputDirName = [osp|output|]
tmpDirName = [osp|tmp|]
socketDirName = [osp|socket|]
cacheDirName = [osp|cache|]

-- | The Unix socket path for the server, placed under the project root.
socketPath :: OsPath -> OsPath
socketPath projectRoot = projectRoot </> socketDirName </> [osp|server.sock|]

isHaskellSource :: OsPath -> Bool
isHaskellSource = isExtensionOf [osp|hs|]
