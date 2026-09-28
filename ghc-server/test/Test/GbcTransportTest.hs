{-# LANGUAGE CPP #-}

-- | In-process variant of 'GhcServer.GbcTransport': loads GBC bytecode (@.gbc@ files) into a fresh session that has
-- no interfaces loaded, as a persistent executor would.
-- The subprocess variant is the @gbc-transport@ command of @ghc-server@ (flake app @gbc-transport@).
module Test.GbcTransportTest where

#if defined(GBC)

import Control.Monad.IO.Class (liftIO)
import GHC (getSession)
import GhcServer.GbcTransport (
  GbcTransportOptions (..),
  compileProject,
  expectedEntry,
  mirrorRehydrateTime,
  runFromGbc,
  timed,
  transportOptions,
  )
import Hedgehog (annotate, evalMaybe, (===))
import Test.Run (transientSession, unitTest, withTemp)
import Test.Tasty (TestTree)

functions :: Int
functions = 500

test_gbcTransport :: TestTree
test_gbcTransport =
  withTemp "gbc-transport" \ tmpResource ->
    unitTest "load .gbc files into an interface-free session" do
      project <- liftIO tmpResource
      (paths, mirror) <- liftIO (compileProject GbcTransportOptions {serverExe = "", project, functions, childRts = [], sharedPayloads = False})
      annotate ("bytecode files: " <> show paths)
      length paths === 3
      ((result, decodeTime), totalTime) <- transientSession transportOptions do
        env <- getSession
        liftIO (timed (runFromGbc paths env))
      annotate ("gbc decode time to WHNF (3 modules): " <> show decodeTime)
      annotate ("gbc decode + link + eval time: " <> show totalTime)
      result === Just (expectedEntry functions 0)
      rehydrateTime <- liftIO . mirrorRehydrateTime =<< evalMaybe mirror
      annotate ("mirror rehydrate time (Fat only): " <> show rehydrateTime)

#else

import Test.Tasty (TestTree, testGroup)

test_gbcTransport :: TestTree
test_gbcTransport = testGroup "gbc transport (GBC only)" []

#endif
