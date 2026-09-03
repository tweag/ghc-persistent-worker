module Ghc.Ui.Data.ServerProcess where

import Brick (txt, (<+>))
import Brick.Forms (FormFieldState, editTextField, (@@=))
import Control.Concurrent.Async (Async)
import Control.Lens (Iso', iso)
import qualified Data.Text as Text
import Data.Text (Text)
import GHC.Generics (Generic)
import Ghc.Ui.Data.Name (Name (..))
import System.Directory.OsPath (canonicalizePath, getCurrentDirectory)
import System.IO (Handle)
import System.OsPath (OsPath)
import System.OsPath.Extra (fromOsPath, toOsPath)
import System.Process.Typed (Process)

joined :: Iso' [Text] Text
joined =
  iso Text.unwords Text.words

data ServerRoot =
  ServerRootCwd
  |
  ServerRoot OsPath
  deriving stock (Eq, Show)

canonicalServerRoot :: ServerRoot -> IO OsPath
canonicalServerRoot = \case
  ServerRootCwd -> getCurrentDirectory
  ServerRoot path -> canonicalizePath path

asServerRoot :: Iso' ServerRoot Text
asServerRoot =
  iso render parse
  where
    render = \case
      ServerRootCwd -> "."
      ServerRoot path -> Text.pack (fromOsPath path)

    parse = \case
      "" -> ServerRootCwd
      "." -> ServerRootCwd
      path -> ServerRoot (toOsPath (Text.unpack path))

describeServerRoot :: ServerRoot -> Text
describeServerRoot = \case
  ServerRootCwd -> "the current directory"
  ServerRoot path -> Text.pack (fromOsPath path)

-- | Used to back the input fields of the form for starting the server, and to persist the config for restarting the
-- server.
data ServerConfig =
  ServerConfig {
    root :: ServerRoot,
    options :: [Text]
  }
  deriving stock (Eq, Show, Generic)

newServerConfig :: ServerConfig
newServerConfig =
  ServerConfig {
    root = ServerRootCwd,
    options = [
      "--enable", "lazy-byte-code",
      "--max-bytecode", "50"
    ]
  }

data ServerStatus =
  ServerStarting
  |
  ServerStarted {
    process :: Process () Handle Handle,
    listener :: Async (),
    stdoutReader :: Async (),
    stderrReader :: Async ()
  }
  |
  ServerConnected
  |
  ServerInactive

data ServerProcess =
  ServerProcess {
    config :: ServerConfig,
    status :: ServerStatus
  }

serverConfigFields :: [ServerConfig -> FormFieldState ServerConfig e Name]
serverConfigFields =
  [
    (txt "Project path: " <+>) @@= editTextField (#root . asServerRoot) StartServerRoot (Just 1),
    (txt "Extra options: " <+>) @@= editTextField (#options . joined) StartServerOptions (Just 1)
  ]
