{-# LANGUAGE OverloadedStrings #-}
-- | Feasibility experiment for the multiprocess execution redesign: run metadata and compile for a
-- single unit with two modules in-process (no scheduler -- direct calls to
-- 'GhcServer.Build.Metadata.runMetadata'\/'GhcServer.Build.Compile.compileSingleModule'), then run the
-- @execute@ step for the module that depends on the other one in a genuinely separate OS process, using the
-- production 'GhcServer.Build.Process' machinery (self-relaunch of this test executable via
-- 'GhcServer.Build.Process.evalProcessFlag' in its argv, see @test/Main.hs@).
--
-- This sidesteps the @forkProcess@ deadlock documented in @kb-process-isolation@\/'Test.ForkMetadataTest'
-- entirely: rather than forking a live multi-capability GHC session, the child is a freshly started
-- process that builds its own 'GhcServer.Data.BuildEnv.BuildEnv' and 'Types.State.WorkerState' from scratch,
-- restoring the compiled unit's state purely from the on-disk cache the parent process's metadata\/compile
-- steps wrote (@cached_unit.json@ plus the @.dyn_hi@\/@.dyn_o@ interface files) -- the same mechanism Buck
-- uses to resume a killed-and-restarted worker (see @kb-buck-cache@).
module Test.SubprocessTest where

import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Data.Text (Text)
import GHC (mkModuleName)
import GhcServer.Build (newBuildState)
import GhcServer.Build.Compile (compileSingleModule)
import GhcServer.Build.Metadata (runMetadata)
import GhcServer.Build.Process (spawnProcessEval)
import GhcServer.Data.ProcessEval (
  EvalOutcome (..),
  ProcessEvalConfig (..),
  ProcessEvalOutput (..),
  ProcessEvalResult (..),
  )
import GhcServer.Data.Unit (Project (..))
import GhcServer.Data.UnitConfig (UnitConfig (..))
import Hedgehog (annotate, property, test, withTests, (===))
import System.Directory (createDirectoryIfMissing)
import System.Environment (getExecutablePath)
import System.OsPath.Extra (toOsPath)
import Test.BuildTest (
  TestProject (..),
  acquireProject,
  acquireTemp,
  baseGhcArgs,
  newBuildEnv,
  writeProjectFile,
  writeUnitConfig,
  )
import Test.Tasty (TestName, TestTree)
import Test.Tasty.Hedgehog (testProperty)
import Types.Api (UnitName (..))
import Types.CachedDeps (CachedDeps (..))

-- ---------------------------------------------------------------------------
-- Fixture: one unit, two modules -- M1 (leaf) and Main (imports M1, has 'main')
-- ---------------------------------------------------------------------------

subprocessUnitName :: Text
subprocessUnitName = "unit1"

createSubprocessProject :: FilePath -> IO ()
createSubprocessProject root = do
  createDirectoryIfMissing True (root ++ "/" ++ Text.unpack subprocessUnitName)
  writeUnitConfig root (Text.unpack subprocessUnitName) UnitConfig {deps = [], args = baseGhcArgs}
  writeProjectFile root (Text.unpack subprocessUnitName ++ "/M1.hs") $ unlines
    [ "module M1 where"
    , ""
    , "m1 :: String"
    , "m1 = \"hello from subprocess\""
    ]
  writeProjectFile root (Text.unpack subprocessUnitName ++ "/Main.hs") $ unlines
    [ "module Main where"
    , ""
    , "import M1 (m1)"
    , ""
    , "main :: IO String"
    , "main = pure m1"
    ]

test_subprocessExecute :: TestTree
test_subprocessExecute =
  testProperty testName $ withTests 1 $ property $ test do
    tp <- liftIO do
      root <- acquireTemp "ghc-server-subprocess"
      createSubprocessProject root
      acquireProject (pure root)
    stateVar <- liftIO newBuildState
    (buildEnv, _events) <- liftIO (newBuildEnv tp stateVar)
    let name = UnitName subprocessUnitName
    unit <- maybe (fail "unit not found") pure (Map.lookup name tp.project.units)
    (metaErrs, _) <- liftIO (runMetadata buildEnv unit)
    annotate ("metadata errors: " ++ show metaErrs)
    unless (null metaErrs) (fail "metadata failed")
    (m1Errs, _) <- liftIO (compileSingleModule buildEnv unit (mkModuleName "M1") (CachedDeps []) 0)
    unless (null m1Errs) (fail ("M1 compile failed: " ++ show m1Errs))
    (mainErrs, _) <- liftIO (compileSingleModule buildEnv unit (mkModuleName "Main") (CachedDeps []) 0)
    unless (null mainErrs) (fail ("Main compile failed: " ++ show mainErrs))
    let cfg = ProcessEvalConfig {projectRoot = toOsPath tp.root, unit, moduleName = "Main", sharedBytecodePath = Nothing}
    self <- liftIO (toOsPath <$> getExecutablePath)
    output <- liftIO (spawnProcessEval self cfg)
    annotate ("subprocess stderr: " ++ Text.unpack output.processStderr)
    output.processStderr === ""
    result <- either (fail . Text.unpack) pure output.result
    annotate ("subprocess log: " ++ show result.logMessages)
    annotate ("eval stdout: " ++ Text.unpack result.evalStdout)
    annotate ("eval stderr: " ++ Text.unpack result.evalStderr)
    result.outcome === EvalSuccess (Just "hello from subprocess")
  where
    testName = "execute module in a fresh subprocess restoring cached state" :: TestName

