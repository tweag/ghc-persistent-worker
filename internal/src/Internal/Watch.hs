-- | File system watching, used to discover workers and servers that start after a client.
module Internal.Watch where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, takeMVar, tryPutMVar)
import Control.Monad (forever, void, when)
import Control.Monad.Extra (whenM)
import Data.Foldable (traverse_)
import Data.IORef (atomicModifyIORef', newIORef)
import Data.List (isInfixOf)
import Data.Set qualified as Set
import System.Directory.OsPath (createDirectoryIfMissing, doesPathExist, listDirectory)
import System.Environment (lookupEnv)
import System.FSNotify (Event (..), EventIsDirectory (..), StopListening, WatchManager, watchDir, withManager)
import System.OsPath.Extra (OsPath, fromOsPath, takeDirectory, takeFileName, toOsPath, (</>))
import Types.Orchestration (primarySocketName)

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

-- | Call the action once with the primary server socket path of each worker directory, both those that already exist
-- and those that are created later.
-- The socket itself may not exist yet when the action is called; use 'waitForPath'.
-- The action runs on the watcher thread and must not block.
-- Never returns.
watchWorkerSockets :: OsPath -> (OsPath -> IO ()) -> IO a
watchWorkerSockets dir onSocket = do
  createDirectoryIfMissing True dir
  seen <- newIORef Set.empty
  let
    isNew key = atomicModifyIORef' seen \ s -> (Set.insert key s, not (Set.member key s))
    visit workerDir =
      when (not ("log" `isInfixOf` fromOsPath (takeFileName workerDir))) do
        whenM (isNew (fromOsPath workerDir)) (onSocket (workerDir </> primarySocketName))
  withManager \ manager -> do
    _ <- watchAddedDirs manager dir visit
    existing <- listDirectory dir
    traverse_ (visit . (dir </>)) existing
    forever (threadDelay maxBound)

-- | Block until something exists at the given path, creating the parent directory if necessary.
-- Waits indefinitely; the existence check happens after the watch is established so that creation between the two
-- cannot be missed.
waitForPath :: OsPath -> IO ()
waitForPath path = do
  createDirectoryIfMissing True parent
  found <- newEmptyMVar
  let signal = void (tryPutMVar found ())
  withManager \ manager -> do
    stop <- watchDir manager (fromOsPath parent) (const True) \case
      Added added _ _ | takeFileName (toOsPath added) == name -> signal
      _ -> pure ()
    whenM (doesPathExist path) signal
    takeMVar found
    stop
  where
    parent = takeDirectory path
    name = takeFileName path
