-- | File system watching, used to discover workers and servers that start after a client.
module Internal.Watch where

import System.Environment (lookupEnv)
import System.FSNotify (Event (..), EventIsDirectory (..), StopListening, WatchManager, watchDir)
import System.OsPath.Extra (OsPath, fromOsPath, toOsPath)

-- | Directory in which workers create one subdirectory each, configurable with @WORKER_PATH@.
workerDirectory :: IO OsPath
workerDirectory =
  maybe (toOsPath "/tmp/ghc-persistent-worker") toOsPath <$> lookupEnv "WORKER_PATH"

-- | Call the action with the path of each directory that is created inside the given directory.
watchAddedDirs :: WatchManager -> OsPath -> (OsPath -> IO ()) -> IO StopListening
watchAddedDirs manager dir onAdded =
  watchDir manager (fromOsPath dir) (const True) \case
    Added added _ IsDirectory -> onAdded (toOsPath added)
    _ -> pure ()
