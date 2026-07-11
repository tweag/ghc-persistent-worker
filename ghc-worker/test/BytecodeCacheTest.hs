-- | Description: Unit tests for the LRU cache tracking and eviction of lazily-loaded bytecode
-- (see "Internal.Cache.Bytecode").
module BytecodeCacheTest where

import Control.Concurrent.MVar (readMVar)
import Control.Monad.IO.Class (liftIO)
import Data.Functor ((<&>))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import GHC (moduleNameFS)
import GHC.Unit.Types (Module, unitFS, GenModule (..))
import Hedgehog ((===))
import ProjectBuildTest (enableLazyByteCode, loadedBcos)
import Test.Build (compileTarget, metadataArgs, runCompile, runMetadata)
import Test.Data.Env (SessionEnv (..), TestEnv (..))
import Test.Data.Project (BuildModule (..), GenUnit (..), ModuleKey (..), ModuleSource (..))
import Test.Env (newSessionEnv, withTestEnv)
import Test.Run (unitTest)
import Test.Source (writeProjectSources)
import Test.Tasty (TestTree)
import Types.Args (Args (..))
import Types.Env (Env (..))
import Types.FeatureFlags (FeatureFlags (..))
import Types.State (WorkerState (..))
import Types.State.Make (BcoCacheEntry (..), MakeState (..))
import Types.Target (ModuleTarget (..))

-- | Module keys for the deterministic cache scenario: two unrelated modules (A, B) with equal-sized bytecode, and
-- two Template Haskell modules that each pull in only one of them as a splice dependency (C depends on A, D depends
-- on B).
keyA, keyB, keyC, keyD :: ModuleKey
keyA = ModuleKey {unit = 0, number = 0, errorVariant = Nothing}
keyB = ModuleKey {unit = 1, number = 0, errorVariant = Nothing}
keyC = ModuleKey {unit = 1, number = 1, errorVariant = Nothing}
keyD = ModuleKey {unit = 1, number = 2, errorVariant = Nothing}

-- | 'A' and 'B' are given the same number of bindings so that their bytecode's tracked size (BCO count) is equal,
-- which makes the eviction limit in 'test_evictBySize' predictable without having to guess at GHC's actual bytecode
-- output size.
modA, modB, modC, modD :: BuildModule
modA = BuildModule {key = keyA, deps = mempty, th = False, bindings = 3, extDeps = mempty}
modB = BuildModule {key = keyB, deps = mempty, th = False, bindings = 3, extDeps = mempty}
modC = BuildModule {key = keyC, deps = Set.singleton keyA, th = True, bindings = 1, extDeps = mempty}
modD = BuildModule {key = keyD, deps = Set.singleton keyB, th = True, bindings = 1, extDeps = mempty}

unit0, unit1 :: GenUnit BuildModule
unit0 = GenUnit {key = 0, depUnits = mempty, modules = [modA]}
unit1 = GenUnit {key = 1, depUnits = Set.singleton 0, modules = [modB, modC, modD]}

moduleSource :: BuildModule -> ModuleSource
moduleSource BuildModule {deps, th, bindings, extDeps} =
  ModuleSource {deps = Set.toList deps, th, bindings, extDeps}

allModules :: Map.Map ModuleKey ModuleSource
allModules =
  Map.fromList [(m.key, moduleSource m) | m <- [modA, modB, modC, modD]]

-- | A comparable, showable identifier for a 'Module', since 'Module' itself has no useful 'Show' instance for test
-- failure messages.
moduleKeyFs :: Module -> (String, String)
moduleKeyFs m = (show (unitFS m.moduleUnit), show (moduleNameFS m.moduleName))

-- | The 'Module' identifying a scenario module's key in the Home Package Table / bytecode cache.
moduleFor :: ModuleKey -> Module
moduleFor key = (compileTarget key).mod

-- | Compile modules A, B and the Template Haskell module C (which depends on A), setting up the unit metadata for
-- both units first. Returns the session's 'Env' for inspection.
buildUpToC :: SessionEnv -> IO Env
buildUpToC sessionEnv = do
  writeProjectSources sessionEnv.sourceDir allModules
  _ <- runMetadata sessionEnv (metadataArgs sessionEnv False) unit0
  _ <- runCompile sessionEnv plainArgs keyA
  _ <- runMetadata sessionEnv (metadataArgs sessionEnv False) unit1
  _ <- runCompile sessionEnv plainArgs keyB
  _ <- runCompile sessionEnv plainArgs keyC
  pure sessionEnv.env
  where
    plainArgs _ = (sessionEnv.shared.baseArgs, mempty)

-- | Read the tracked size of module A's cache entry after it has been touched by compiling C, which is used to pick
-- an eviction limit that is guaranteed to be exceeded (but only just) once B is touched by compiling D.
sizeOfA :: Env -> IO Int
sizeOfA env = do
  WorkerState {make} <- readMVar env.state
  case Map.lookup (moduleFor keyA) make.bcoCache of
    Nothing -> error "module A was not tracked in the bytecode cache after compiling its dependent splice"
    Just entry -> pure entry.size

-- | Verifies that compiling the Template Haskell module C causes its splice dependency (module A) to be tracked in
-- the bytecode cache, and that compiling a second, unrelated Template Haskell module D with the cache limit set to
-- exactly the tracked size of A causes A's cache entry (and only A's) to be evicted once B is also tracked, since A
-- is the least recently used entry.
test_evictBySize :: TestTree
test_evictBySize =
  withTestEnv \ getTestEnv ->
    unitTest "bytecode cache: LRU eviction by size" do
      testEnv <- liftIO getTestEnv
      sessionEnv <- liftIO (newSessionEnv (enableLazyByteCode testEnv))
      env <- liftIO (buildUpToC sessionEnv)

      limit <- liftIO (sizeOfA env)

      liftIO do
        let args =
              sessionEnv.shared.baseArgs {
                features = sessionEnv.shared.baseArgs.features {lazyByteCodeCacheLimit = Just limit}
              }
        _ <- runCompile sessionEnv (\ _ -> (args, mempty)) keyD
        pure ()

      cacheAfter <- liftIO do
        WorkerState {make} <- readMVar env.state
        pure make.bcoCache

      (Map.keys cacheAfter <&> moduleKeyFs) === [moduleKeyFs (moduleFor keyB)]

      bcos <- liftIO (loadedBcos env)
      let modules = [modFs | (_, modFs, _) <- bcos]
      any (== "Unit0Module0") modules === False
      any (== "Unit1Module0") modules === True
      any (== "Unit1Module1") modules === True
      any (== "Unit1Module2") modules === True

-- | Verifies the cache-tracking logic without triggering any eviction: compiling the Template Haskell module C
-- causes exactly its splice dependency (module A) to be tracked, and setting a generous cache limit for that same
-- compile job does not evict anything.
test_touchNoEviction :: TestTree
test_touchNoEviction =
  withTestEnv \ getTestEnv ->
    unitTest "bytecode cache: touch without eviction" do
      testEnv <- liftIO getTestEnv
      sessionEnv <- liftIO (newSessionEnv (enableLazyByteCode testEnv))
      let plainArgs (_ :: ModuleKey) = (sessionEnv.shared.baseArgs, mempty)

      env <- liftIO do
        writeProjectSources sessionEnv.sourceDir allModules
        _ <- runMetadata sessionEnv (metadataArgs sessionEnv False) unit0
        _ <- runCompile sessionEnv plainArgs keyA
        _ <- runMetadata sessionEnv (metadataArgs sessionEnv False) unit1
        _ <- runCompile sessionEnv plainArgs keyB
        let args =
              sessionEnv.shared.baseArgs {
                features = sessionEnv.shared.baseArgs.features {lazyByteCodeCacheLimit = Just 1000000}
              }
        _ <- runCompile sessionEnv (\ _ -> (args, mempty)) keyC
        pure sessionEnv.env

      make <- liftIO do
        WorkerState {make} <- readMVar env.state
        pure make

      (Map.keys make.bcoCache <&> moduleKeyFs) === [moduleKeyFs (moduleFor keyA)]
