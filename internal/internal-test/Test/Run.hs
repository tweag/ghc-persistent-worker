module Test.Run where

import Control.Concurrent (MVar)
import Control.Monad.IO.Class (liftIO)
import GHC (Ghc)
import GHC.Stack (HasCallStack, withFrozenCallStack)
import Hedgehog (TestT, evalMaybe, property, test, withTests)
import Internal.Session (simpleSessionWithDebugLog)
import Internal.State (newState)
import System.Directory (removeDirectoryRecursive)
import System.IO.Temp (createTempDirectory, getCanonicalTemporaryDirectory)
import Test.Tasty (TestName, TestTree, withResource)
import Test.Tasty.Hedgehog (testProperty)
import Types.Args (Args (..), emptyArgs)
import Types.State (WorkerState)

unitTest ::
  HasCallStack =>
  TestName ->
  TestT IO () ->
  TestTree
unitTest desc t =
  withFrozenCallStack do
    testProperty desc (withTests 1 (property (test t)))

acquireTemp :: FilePath -> IO FilePath
acquireTemp name = do
  tmpBase <- getCanonicalTemporaryDirectory
  createTempDirectory tmpBase name

-- | Use a temp dir for a Tasty test.
-- We use this instead of @withSystemTempDirectory@ because 'TestT' doesn't have @MonadMask@.
withTemp :: FilePath -> (IO FilePath -> TestTree) -> TestTree
withTemp name =
  withResource (acquireTemp name) removeDirectoryRecursive

-- | Convenience session runner that prints all log messages to stderr afterwards.
persistentSession :: MVar WorkerState -> [String] -> Ghc a -> TestT IO a
persistentSession state ghcOptions ma =
  evalMaybe =<< liftIO (simpleSessionWithDebugLog state (emptyArgs []) {ghcOptions} ma)

-- | Convenience session runner that creates a one-time use @WorkerState@ prints all log messages to stderr afterwards.
transientSession :: [String] -> Ghc a -> TestT IO a
transientSession ghcOptions ma = do
  state <- liftIO $ newState False
  persistentSession state ghcOptions ma
