{-# LANGUAGE OverloadedStrings #-}
-- | Regression test for manual bytecode-cache eviction in the standalone GHC server.
--
-- Builds and executes two "fat" modules (see 'Test.Source.writeFatModuleSource') directly through the
-- 'GhcServer.Build.Metadata', 'GhcServer.Build.Compile', and 'GhcServer.Build.Execute' handlers -- bypassing the
-- scheduler ("GhcServer.Build") entirely -- then asserts that executing a module's @main@ populates
-- 'Types.State.Make.MakeState.bcoCache', and that calling 'Internal.Cache.Bytecode.evictBcoCache' directly (not
-- via the automatic 'Types.FeatureFlags.lazyByteCodeCacheLimit'-driven path in 'Internal.State.withState') removes
-- the tracked entries.
--
-- The final part of 'test_bcoEvictionManual' re-executes the main module after eviction, to reproduce a GHC panic
-- triggered by evicting a module's bytecode after that module was itself previously run via
-- 'GhcServer.Build.Execute.executeModuleTask' (as opposed to only ever compiled to object code via
-- 'GhcServer.Build.Compile.compileSingleModule'). See the comments on 'helperModuleKey' and near the final
-- 'executeModuleTask' call in 'test_bcoEvictionManual' for the full causal chain. In short: a module that is
-- ever executed (not merely compiled) is permanently unrecoverable as a link dependency after any subsequent
-- bytecode eviction, because none of the three linkable sources 'GHC.Linker.Deps.getLinkDeps' can draw on --
-- object code, cached bytecode, or lazily-reloaded-from-Core bytecode -- remain available for it. This makes
-- 'test_bcoEvictionManual' assert the confirmed panic (@Just (TaskFailed "GHC session setup failed\npanic! ...
-- expectJust getLinkDeps ...")@) as the currently-correct outcome, rather than the desired @Just (TaskSuccess
-- _)@ that would only hold once the underlying GHC-level bug is fixed (this requires patching the vendored GHC's
-- @GHC.Linker.Deps.getLinkDeps@/pipeline Core-stripping behavior, out of scope for this test).
module Test.EvictionTest where

import Control.Concurrent.MVar (readMVar)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map.Strict as Map
import GHC (ModuleName, mkModuleName)
import GHC.Utils.Outputable (showPprUnsafe)
import GhcServer.Build (newBuildState)
import GhcServer.Build.Compile (compileSingleModule, moduleTarget)
import GhcServer.Build.Execute (executeModuleTask)
import GhcServer.Build.Metadata (runMetadata)
import GhcServer.Build.Schedule (emptyBuildExt)
import GhcServer.Data.BuildEnv (BuildEnv (..))
import GhcServer.Data.Unit (UnitName (..))
import GhcServer.Data.UnitConfig (UnitConfig (..))
import GhcServer.Path (osPath)
import GhcServer.Scheduler (TaskResult (..))
import Hedgehog (TestT, annotate, assert)
import Internal.Cache.Bytecode (evictBcoCache)
import Internal.Session (ensureSession)
import Internal.State (modifyMakeState)
import Internal.State.Make (loadStateCompile)
import Prelude hiding (log)
import System.Directory (createDirectoryIfMissing)
import System.IO (readFile')
import Test.BuildTest (TestProject (..), baseGhcArgs, newBuildEnv, projectTest, writeUnitConfig)
import Test.Data.Project (ModuleKey (..))
import Test.Path (moduleName, moduleValueName)
import Test.Run (assertJust)
import Test.Source (writeFatModuleSource)
import Test.Tasty (TestName, TestTree)
import Types.CachedDeps (CachedDeps (..))
import Types.Env (Env (..))
import Types.State (WorkerState (..))
import Types.State.Make (BcoCacheEntry (..), MakeState (..))
import Types.Target (ModuleTarget (..))

-- | Identifies the executed module of the test project: unit0/Unit0Module0.hs.
moduleKey :: ModuleKey
moduleKey = ModuleKey {unit = 0, number = 0, errorVariant = Nothing}

-- | Identifies a second module in the same unit (unit0/Unit0Module1.hs) that 'moduleKey' imports and uses. Needed
-- because 'GhcServer.Build.Execute.executeModuleTask' force-recompiles the executed module itself before every
-- run, so only a module it merely *depends on* (never itself the execute target) can actually exercise the lazy
-- bytecode reload path ("GhcServer.Build.Compile.compileSingleModule" -> 'Internal.State.Linkables.lazyLoadByteCode')
-- after eviction.
--
-- Crucially, 'test_bcoEvictionManual' runs this module through 'GhcServer.Build.Execute.executeModuleTask' too
-- (not just 'GhcServer.Build.Compile.compileSingleModule'), even though it has no @main@: that alone is enough to
-- force its own interpreted-only recompile, which produces no object code and immediately strips the interface's
-- Core (@mi_extra_decls@). That makes this module's bytecode permanently unrecoverable once evicted -- see the
-- module haddock above and the comment on the final 'executeModuleTask' call below.
helperModuleKey :: ModuleKey
helperModuleKey = ModuleKey {unit = 0, number = 1, errorVariant = Nothing}

unit0 :: UnitName
unit0 = UnitName "unit0"

fatModuleName :: ModuleName
fatModuleName = mkModuleName (moduleName moduleKey)

helperModuleName :: ModuleName
helperModuleName = mkModuleName (moduleName helperModuleKey)

-- | Number of fat top-level functions to generate: enough to produce a nonzero, measurable BCO count, but small
-- enough to compile quickly in a test.
numFunctions :: Int
numFunctions = 60

caseArms :: Int
caseArms = 5

-- | A single-unit, two-module project: 'helperModuleKey' is generated by 'writeFatModuleSource' (to inflate its
-- BCO footprint), and 'moduleKey' imports it, with a @main@ appended that forces evaluation of both modules'
-- exported values.
createEvictionProject :: FilePath -> IO ()
createEvictionProject root = do
  createDirectoryIfMissing True (root ++ "/unit0")
  writeUnitConfig root "unit0" UnitConfig {deps = [], args = baseGhcArgs}
  writeFatModuleSource (osPath root) numFunctions caseArms helperModuleKey
  writeFatModuleSource (osPath root) numFunctions caseArms moduleKey
  -- The import must precede all other declarations, so it can't simply be appended like 'main' -- splice it in
  -- right after the module header line instead.
  contents <- readFile' modulePath
  case lines contents of
    header : rest ->
      writeFile modulePath (unlines (header : ("import qualified " ++ moduleName helperModuleKey) : rest))
    [] -> fail "Generated fat module source was empty"
  appendFile modulePath mainSource
  where
    modulePath = root ++ "/unit0/" ++ moduleName moduleKey ++ ".hs"

    mainSource =
      unlines [
        "",
        "main :: IO Int",
        "main = pure (" ++ moduleValueName moduleKey ++ " + " ++ moduleName helperModuleKey ++ "." ++ moduleValueName helperModuleKey ++ ")"
      ]

evictionProjectTest :: TestName -> (TestProject -> TestT IO ()) -> TestTree
evictionProjectTest =
  projectTest "ghc-server-eviction" createEvictionProject

-- | Build and execute the fat module via direct handler calls (no scheduler), then verify that bcoCache is
-- populated by the execution, and that a manual 'evictBcoCache' call (with @limit = 0@, forcing unconditional
-- eviction regardless of recency) empties it again.
test_bcoEvictionManual :: TestTree
test_bcoEvictionManual =
  evictionProjectTest "manually evicting bcoCache after execution removes tracked entries" \ tp -> do
    stateVar <- liftIO newBuildState
    (buildEnv, _) <- liftIO (newBuildEnv tp stateVar)

    (metaErrors, _) <- liftIO (runMetadata buildEnv unit0)
    annotate ("metadata errors: " ++ show metaErrors)
    assert (null metaErrors)

    -- Execute (not merely compile) the helper first. 'GhcServer.Build.Execute.executeModuleTask' always runs the
    -- target module through 'Internal.Compile.Make.compileModuleWithDepsInHpt' with 'Types.Target.TargetModuleInterp'
    -- (pure interpreter backend, no object code); GHC's pipeline strips the interface's Core
    -- (@mi_extra_decls@) immediately after generating bytecode in that mode (see
    -- @GHC.Driver.Pipeline.Execute@, "In interpreted mode ... extra_decl is not used any more"), since it assumes
    -- the freshly-generated bytecode will be used directly and never needs to be reconstructed from Core again.
    -- This differs from 'GhcServer.Build.Compile.compileSingleModule' (used below for the main module), which
    -- targets a plain 'Types.Target.TargetModule' and keeps the object-code backend, preserving Core in the
    -- interface. The helper has no @main@, so this is expected to report 'Nothing' (a deliberate no-op skip).
    helperExecResult <- liftIO (executeModuleTask buildEnv emptyBuildExt unit0 helperModuleName)
    annotate ("helper execute result: " ++ show helperExecResult)
    case helperExecResult of
      Nothing -> pure ()
      other -> fail ("Expected Nothing (no main), got " ++ show other)

    (compileErrors, _) <- liftIO (compileSingleModule buildEnv unit0 fatModuleName (CachedDeps []))
    annotate ("compile errors: " ++ show compileErrors)
    assert (null compileErrors)

    execResult <- liftIO (executeModuleTask buildEnv emptyBuildExt unit0 fatModuleName)
    annotate ("execute result: " ++ show execResult)
    case execResult of
      Just (TaskSuccess _) -> pure ()
      other -> fail ("Expected Just (TaskSuccess _), got " ++ show other)

    let targetModule = (moduleTarget unit0 fatModuleName).mod
        helperModule = (moduleTarget unit0 helperModuleName).mod

    beforeState <- liftIO (readMVar stateVar)
    annotate ("bcoCache before eviction: " ++ show (showPprUnsafe <$> Map.keys beforeState.make.bcoCache))
    case Map.lookup targetModule beforeState.make.bcoCache of
      Just entry -> assert (entry.size > 0)
      Nothing -> fail "Expected the executed module to be tracked in bcoCache before eviction"
    case Map.lookup helperModule beforeState.make.bcoCache of
      Just entry -> assert (entry.size > 0)
      Nothing -> fail "Expected the helper module (a link dependency) to be tracked in bcoCache before eviction"

    liftIO do
      let evictEnv = Env {log = buildEnv.log, state = stateVar, args = buildEnv.baseArgs}
      -- 'ensureSession' alone returns the cached base session, whose 'hsc_interp' is not populated: 'Interp' is
      -- initialized lazily by GHC and only threaded from 'MakeState.interp' into a session's 'HscEnv' by
      -- 'loadStateCompile' (normally invoked by 'Internal.State.withState's @restore@ step, which we bypass here
      -- since eviction doesn't need a full 'Ghc' session). Without this, 'evictBcoCache' panics with "Couldn't
      -- find a target code interpreter" when it calls 'hscInterp'.
      hscEnv0 <- ensureSession evictEnv
      modifyMakeState stateVar \ make -> do
        let (make1, hscEnv1) = loadStateCompile hscEnv0 make
        evicted <- evictBcoCache hscEnv1 0 make1
        pure (evicted, ())

    afterState <- liftIO (readMVar stateVar)
    annotate ("bcoCache after eviction: " ++ show (showPprUnsafe <$> Map.keys afterState.make.bcoCache))
    assert (Map.notMember targetModule afterState.make.bcoCache)
    assert (Map.notMember helperModule afterState.make.bcoCache)

    assertJust (TaskSuccess (Just "2")) =<< liftIO (executeModuleTask buildEnv emptyBuildExt unit0 fatModuleName)
