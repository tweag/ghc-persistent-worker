-- | Path utilities shared across GHC server modules.
module GhcServer.Path where

import Control.Exception (Exception, SomeException, throw)
import System.OsPath (OsPath, decodeUtf, encodeUtf, (</>))

data PathDecodingException = PathDecodingException OsPath SomeException
  deriving stock (Show)

instance Exception PathDecodingException where

data PathEncodingException = PathEncodingException FilePath SomeException
  deriving stock (Show)

instance Exception PathEncodingException where

fromOsPath :: OsPath -> FilePath
fromOsPath p = either (throw . PathDecodingException p) id (decodeUtf p)

toOsPath :: String -> OsPath
toOsPath p = either (throw . PathEncodingException p) id (encodeUtf p)

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
