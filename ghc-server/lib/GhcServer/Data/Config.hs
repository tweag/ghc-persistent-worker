-- | CLI configuration types for the standalone GHC server and client.
module GhcServer.Data.Config where

import System.OsPath (OsPath)
import Types.Settings (Settings)

-- | Configuration for the server, parsed from CLI args.
data ServerConfig =
  ServerConfig {
    -- | Absolute path to the project root directory.
    projectRoot :: OsPath,
    -- | Maximum number of concurrent compilation jobs.
    maxJobs :: Int,
    -- | Print the build log even when steps succeed.
    verbose :: Bool,
    -- | Force @unit.json@-based project discovery even if a @.cabal@ file is present in the project root. Normally
    -- discovery auto-detects: a @.cabal@ file triggers Cabal-based discovery, otherwise @unit.json@ files are used.
    jsonConfig :: Bool,
    -- | Initial settings.
    settings :: Settings
  }
  deriving stock (Show)

-- | Configuration for the client, parsed from CLI args.
data ClientConfig =
  ClientConfig {
    -- | Absolute path to the project root directory.
    projectRoot :: Maybe OsPath,
    -- | Raw schedule arguments to send.
    targets :: [String],
    -- | Whether to wait for the build to complete before returning.
    wait :: Bool,
    -- | Force recompilation of modules even when cached artifacts exist.
    recompile :: Bool,
    -- | Recompute metadata (and recompile) even when cached.
    rebuild :: Bool,
    -- | Run this request's execute tasks in a fresh child process instead of in-process.
    process :: Bool
  }
  deriving stock (Show)
