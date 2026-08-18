module GhcServer.Optparse where

import Options.Applicative (ReadM, str)
import System.OsPath.Extra (OsPath, toOsPath)

readPath :: ReadM OsPath
readPath = toOsPath <$> str
