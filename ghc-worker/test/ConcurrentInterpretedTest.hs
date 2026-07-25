-- | Description: Harness for reproducing a "missing closure" error observed in production when Buck executes
-- multiple tests concurrently against the worker, where test targets are compiled with @IsInterpreted = Interpreted@
-- (bytecode-only) and their test entry point is invoked via the worker's production eval mode
-- ('Internal.Evaluate.evaluate', driven here through 'Test.Build.runEvaluate'), analogous to how Buck's test runner
-- would invoke a compiled test target.
--
-- The project structure is a configurable two-tier dependency graph:
--
-- * "node" units form a linear chain (@unit(i)@ depends on @unit(i-1)@), each with a configurable number of modules,
--   compiled normally (dual object/bytecode, 'Compiled').
-- * "leaf" units represent test targets: each depends on every node unit, has a configurable number of modules, each
--   of which uses a Template Haskell splice (mirroring the production TH usage that triggered the original error) and
--   exports a @test_M_N :: IO ()@ entry point (named via 'Test.Path.testFunctionName').
--
-- This is a single build schedule (metadata + compile for all node units, then metadata for all leaf units), with
-- leaf modules compiled and their test entry points executed *concurrently* across all leaves, mirroring Buck's
-- concurrent test execution. This does not include a resume build, since that was not required to trigger the
-- original error in production.
--
-- NOTE: this test is a debugging harness, not a proven reproduction. It has not been confirmed to reproduce the
-- "missing closure" error; it only provides the scaffolding (configurable graph shape + concurrent interpreted test
-- execution) needed to explore it further.
module ConcurrentInterpretedTest where

import Control.Concurrent.Async (mapConcurrently)
import Control.Concurrent.MVar (readMVar)
import Control.Monad.IO.Class (liftIO)
import Data.Functor ((<&>))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import GHC (moduleNameFS)
import GHC.Unit.Types (GenModule (..), Module, unitFS)
import Hedgehog (annotateShow, assert, (===))
import Test.Build (compileTarget, metadataArgs, runCompile, runEvaluate, runMetadata)
import Test.Bytecode (enableLazyByteCode)
import Test.Data.Env (SessionEnv (..), TestEnv (..))
import Test.Data.Project (BuildModule (..), GenUnit (..), ModuleKey (..), ModuleSource (..), UnitKey)
import Test.Data.Scheduler (RequestResult (..))
import Test.Env (newSessionEnv, withTestEnv)
import Test.Path (testFunctionName)
import Test.Run (unitTest)
import Test.Source (writeProjectSources, writeTestModuleSources)
import Test.Tasty (TestTree)
import Types.Args (Args (..))
import Types.Env (Env (..))
import Types.FeatureFlags (FeatureFlags (..))
import Types.State (WorkerState (..))
import Types.State.Make (BcoCacheEntry (..), MakeState (..))
import Types.Target (ModuleTarget (..))

-- | Tunable shape of the two-tier dependency graph. Field names describe the graph role, not the Buck target type,
-- since this test does not go through Buck.
data GraphConfig =
  GraphConfig {
    -- | Number of internal ("node") units, forming a linear dependency chain.
    nodeUnits :: Int,
    -- | Number of modules per node unit.
    modulesPerNode :: Int,
    -- | Number of leaf ("test") units, each depending on every node unit.
    leafUnits :: Int,
    -- | Number of modules per leaf unit.
    modulesPerLeaf :: Int
  }
  deriving stock (Eq, Show)

-- | A small default shape, deliberately modest so the harness runs quickly; increase via a dedicated 'TestTree' or
-- interactively when investigating the race, since larger graphs and more leaves increase the chance of triggering
-- concurrent interpreter contention.
defaultConfig :: GraphConfig
defaultConfig =
  GraphConfig {
    nodeUnits = 2,
    modulesPerNode = 2,
    leafUnits = 3,
    modulesPerLeaf = 2
  }

-- | Build the chain of node units. Unit @0@ has no deps; unit @i@ depends on unit @i-1@ (both at the unit level and,
-- for its first module, at the module level, to give the compiler an actual cross-unit import to resolve).
nodeUnitsFor :: GraphConfig -> [GenUnit BuildModule]
nodeUnitsFor conf =
  [nodeUnit i | i <- [0 .. conf.nodeUnits - 1]]
  where
    nodeUnit :: Int -> GenUnit BuildModule
    nodeUnit i =
      GenUnit {
        key = fromIntegral i,
        depUnits = if i == 0 then mempty else Set.singleton (fromIntegral (i - 1)),
        modules = [nodeModule i n | n <- [0 .. conf.modulesPerNode - 1]]
      }

    nodeModule :: Int -> Int -> BuildModule
    nodeModule i n =
      BuildModule {
        key = ModuleKey {unit = fromIntegral i, number = n, errorVariant = Nothing},
        deps = if i == 0 || n /= 0 then mempty else Set.singleton (ModuleKey {unit = fromIntegral (i - 1), number = 0, errorVariant = Nothing}),
        th = False,
        bindings = 1,
        extDeps = mempty
      }

-- | Build the leaf ("test") units. Each depends on every node unit; each module imports the last node unit's first
-- module (if any node units exist) and uses a Template Haskell splice, matching the production scenario where the
-- error occurred while executing TH.
leafUnitsFor :: GraphConfig -> [GenUnit BuildModule]
leafUnitsFor conf =
  [leafUnit j | j <- [0 .. conf.leafUnits - 1]]
  where
    lastNode :: Int
    lastNode = conf.nodeUnits - 1

    leafUnit :: Int -> GenUnit BuildModule
    leafUnit j =
      GenUnit {
        key = fromIntegral (conf.nodeUnits + j),
        depUnits = Set.fromList (fromIntegral <$> [0 .. conf.nodeUnits - 1]),
        modules = [leafModule j n | n <- [0 .. conf.modulesPerLeaf - 1]]
      }

    leafModule :: Int -> Int -> BuildModule
    leafModule j n =
      BuildModule {
        key = ModuleKey {unit = fromIntegral (conf.nodeUnits + j), number = n, errorVariant = Nothing},
        deps =
          if conf.nodeUnits == 0
          then mempty
          else Set.singleton (ModuleKey {unit = fromIntegral lastNode, number = 0, errorVariant = Nothing}),
        th = True,
        bindings = 1,
        extDeps = mempty
      }

-- | 'ModuleSource' view of a 'BuildModule', for 'writeProjectSources'/'writeTestModuleSources'.
moduleSourceOf :: BuildModule -> ModuleSource
moduleSourceOf BuildModule {deps, th, bindings, extDeps} =
  ModuleSource {deps = Set.toList deps, th, bindings, extDeps}

moduleSourceMap :: [GenUnit BuildModule] -> Map.Map ModuleKey ModuleSource
moduleSourceMap units =
  Map.fromList [(m.key, moduleSourceOf m) | u <- units, m <- u.modules]

-- | Build all node units (metadata then compile, in chain order) sequentially, since leaves depend on the full node
-- chain being present in the worker's state before their own metadata step runs.
buildNodes :: SessionEnv -> [GenUnit BuildModule] -> IO [RequestResult]
buildNodes sessionEnv units =
  concat <$> mapM buildNodeUnit units
  where
    buildNodeUnit :: GenUnit BuildModule -> IO [RequestResult]
    buildNodeUnit unit = do
      metaResult <- runMetadata sessionEnv (metadataArgs sessionEnv False) unit
      moduleResults <- mapM (runCompile sessionEnv plainArgs . (.key)) unit.modules
      pure (metaResult : moduleResults)

    plainArgs _ = (sessionEnv.shared.baseArgs, mempty)

-- | Run metadata for all leaf units sequentially (mirroring Buck's per-unit metadata requests, which are not the
-- focus of this harness), then compile and execute every leaf module's test entry point *concurrently* across all
-- leaves, mirroring Buck running multiple tests at once against the same worker process.
buildLeaves :: SessionEnv -> [GenUnit BuildModule] -> IO [RequestResult]
buildLeaves sessionEnv units = do
  metaResults <- mapM (runMetadata sessionEnv (metadataArgs sessionEnv False)) units
  let allModules = [m | u <- units, m <- u.modules]
  testResults <- mapConcurrently (runLeafModule . (.key)) allModules
  pure (metaResults ++ testResults)
  where
    runLeafModule :: ModuleKey -> IO RequestResult
    runLeafModule key =
      runEvaluate sessionEnv plainArgs (testFunctionName key) key

    plainArgs _ = (sessionEnv.shared.baseArgs, mempty)

-- | Run the whole harness for a given 'GraphConfig': write sources, build the node chain, then build the leaves
-- concurrently. Returns every 'RequestResult' produced (metadata + compile steps for nodes and leaves).
runGraph :: SessionEnv -> GraphConfig -> IO [RequestResult]
runGraph sessionEnv conf = do
  let nodes = nodeUnitsFor conf
      leaves = leafUnitsFor conf
  writeProjectSources sessionEnv.sourceDir (moduleSourceMap nodes)
  writeTestModuleSources sessionEnv.sourceDir (moduleSourceMap leaves)
  nodeResults <- buildNodes sessionEnv nodes
  leafResults <- buildLeaves sessionEnv leaves
  pure (nodeResults ++ leafResults)

-- | Tasty entry point. Runs 'defaultConfig' through 'runGraph' and asserts that every step succeeded. Since the
-- purpose of this harness is to provide a reproducible scaffold rather than to encode a known-good expectation, a
-- failure here should be treated as a starting point for investigation, not necessarily a regression.
test_concurrentInterpreted :: TestTree
test_concurrentInterpreted =
  withTestEnv \ getTestEnv ->
    unitTest "concurrent interpreted test execution: dependency graph harness" do
      testEnv <- liftIO getTestEnv
      sessionEnv <- liftIO (newSessionEnv testEnv)
      results <- liftIO (runGraph sessionEnv defaultConfig)
      annotateShow results
      assert (all (== RequestSuccess) results)

-- | Silences an unused-import warning for 'UnitKey' (used only via 'fromIntegral' literals, not named directly),
-- while documenting that unit keys throughout this module are constructed via numeric literals rather than the
-- 'UnitKey' constructor.
_unitKeyDoc :: UnitKey -> UnitKey
_unitKeyDoc = id

-- * BCO eviction under concurrent interpreted test execution
--
-- The tests below combine this module's concurrent eval-job harness with the LRU eviction scenario from
-- 'BytecodeCacheTest'. Every leaf's TH-splicing test module depends on two non-TH modules whose bytecode gets
-- tracked in the cache once the splice resolves them (see 'Internal.Cache.Bytecode.touchBcoCache'): one "shared"
-- module common to every leaf, and one "helper" module unique to that leaf. Both are inflated with many trivial
-- top-level bindings (see 'Test.Data.Project.ModuleSource.bindings', documented as the size-proxy knob in
-- 'Internal.Cache.Bytecode.linkableBcoCount') to give them a substantial, clearly non-trivial tracked size,
-- approximating (in BCO-count terms, not actual bytes, per the caveat in that Haddock) the "large per-test bytecode"
-- scenario this harness is meant to exercise. A literal ~50MB per module is not attempted here since that would push
-- this test's runtime well past the sub-second budget expected of the suite; the chosen sizes are merely "clearly
-- large enough to dominate every other module in the graph", which is all the eviction assertions below require.

-- | Number of top-level bindings generated for the module shared by every leaf, and for each leaf's own dedicated
-- helper module. Deliberately equal, so the eviction limit below can be sized in exact multiples of these.
sharedBcoBindings, leafBcoBindings :: Int
sharedBcoBindings = 200
leafBcoBindings = 200

-- | The three leaf unit keys used by the eviction scenario (unit @0@ is reserved for the shared module).
evictionLeafKeys :: [UnitKey]
evictionLeafKeys = [1, 2, 3]

-- | Key of the module shared by every leaf's TH splice (unit @0@, the only module in that unit).
evictionSharedKey :: ModuleKey
evictionSharedKey = ModuleKey {unit = 0, number = 0, errorVariant = Nothing}

-- | Key of a leaf's dedicated helper module (module number @1@ in that leaf's unit; the leaf's own test module is
-- number @0@).
evictionHelperKey :: UnitKey -> ModuleKey
evictionHelperKey leaf = ModuleKey {unit = leaf, number = 1, errorVariant = Nothing}

-- | Key of a leaf's TH-splicing test module.
evictionTestKey :: UnitKey -> ModuleKey
evictionTestKey leaf = ModuleKey {unit = leaf, number = 0, errorVariant = Nothing}

-- | The shared unit's sole module (non-TH, inflated with 'sharedBcoBindings' trivial top-level bindings).
evictionSharedModule :: BuildModule
evictionSharedModule =
  BuildModule {key = evictionSharedKey, deps = mempty, th = False, bindings = sharedBcoBindings, extDeps = mempty}

-- | The shared unit: a single large, non-TH module imported (and spliced against) by every leaf's test module.
evictionSharedUnit :: GenUnit BuildModule
evictionSharedUnit =
  GenUnit {
    key = 0,
    depUnits = mempty,
    modules = [evictionSharedModule]
    }

-- | A leaf's dedicated, non-TH helper module (inflated with 'leafBcoBindings' trivial top-level bindings).
evictionHelperModule :: UnitKey -> BuildModule
evictionHelperModule leaf =
  BuildModule {key = evictionHelperKey leaf, deps = mempty, th = False, bindings = leafBcoBindings, extDeps = mempty}

-- | A leaf's TH-splicing test module, depending on both the shared module and this leaf's own helper module.
evictionTestModule :: UnitKey -> BuildModule
evictionTestModule leaf =
  BuildModule {
    key = evictionTestKey leaf,
    deps = Set.fromList [evictionSharedKey, evictionHelperKey leaf],
    th = True,
    bindings = 1,
    extDeps = mempty
    }

-- | One leaf unit: a large, non-TH helper module unique to this leaf, and a TH test module that splices values from
-- both the shared module and this leaf's own helper module (mirroring 'modC'/'modD' in 'BytecodeCacheTest', but with
-- both dependencies inflated in size).
evictionLeafUnit :: UnitKey -> GenUnit BuildModule
evictionLeafUnit leaf =
  GenUnit {
    key = leaf,
    depUnits = Set.singleton 0,
    modules = [evictionHelperModule leaf, evictionTestModule leaf]
    }

-- | Leaf whose test module is re-evaluated once more, after the concurrent phase, to trigger eviction. Arbitrary;
-- any leaf key would do, since the trigger step deterministically decides which helper module survives.
evictionTriggerLeaf :: UnitKey
evictionTriggerLeaf = 1

-- | The 'Module' identifying a scenario module's key in the bytecode cache, mirroring 'BytecodeCacheTest.moduleFor'.
evictionModuleFor :: ModuleKey -> Module
evictionModuleFor key = (compileTarget key).mod

-- | Sum of the tracked cache sizes of the shared module and 'evictionTriggerLeaf''s helper and test modules, read
-- directly from 'MakeState.bcoCache' rather than assumed from the 'bindings' proxy knob (see 'BytecodeCacheTest.sizeOfA'
-- for the same measure-don't-guess approach): 'Internal.Cache.Bytecode.touchBcoCache' only refreshes a module's access
-- counter when a splice actually triggers a (re-)load of its bytecode, not on every reference, so the tracked BCO
-- count is not simply proportional to the 'bindings' knob.
triggerGroupSize :: SessionEnv -> IO Int
triggerGroupSize sessionEnv = do
  WorkerState {make} <- readMVar sessionEnv.env.state
  pure (sum [entry.size | key <- triggerGroupKeys, Just entry <- [Map.lookup (evictionModuleFor key) make.bcoCache]])
  where
    triggerGroupKeys = [evictionSharedKey, evictionHelperKey evictionTriggerLeaf, evictionTestKey evictionTriggerLeaf]

-- | Build the shared unit and all leaf units, then run every leaf's interpreted test concurrently (mirroring
-- 'buildLeaves'/'test_concurrentInterpreted'), populating the bytecode cache with the shared module and every leaf's
-- helper module with no eviction limit in effect. A real cache limit is deliberately *not* configured during this
-- concurrent phase: 'Internal.Cache.Bytecode.evictBcoCache' calls 'GHC.Linker.Loader.unload', and racing that against
-- another thread's in-flight splice resolution is a genuine, documented hazard (see the "Deterministic proxy test
-- for bytecode cache concurrency race" ADR entry) that this harness is not attempting to reproduce.
--
-- Determinism of which entries survive eviction cannot rely on which concurrent job happens to finish last (real-time
-- scheduling order is not guaranteed), nor on the shared module's own recency (a module's access counter is only
-- refreshed when a splice actually triggers a fresh load of it, not on every reference by a dependent splice -- so
-- once loaded, the shared module stays at whatever counter it was first touched with until something evicts and
-- reloads it). Instead, after the concurrent phase, the two non-trigger leaves are re-evaluated sequentially (bumping
-- the shared module's counter alongside each), then 'evictionTriggerLeaf' is re-evaluated last and on its own,
-- guaranteeing that the shared module and this leaf's helper and test modules end up with the single newest access
-- counter, strictly newer than the other two leaves' entries. A limit sized to exactly this group's measured total
-- size (see 'triggerGroupSize'), applied on one final single-threaded re-evaluation of the same leaf, then
-- deterministically evicts only the other two leaves' entries.
runEvictionGraph :: SessionEnv -> IO [RequestResult]
runEvictionGraph sessionEnv = do
  let leaves = evictionLeafUnit <$> evictionLeafKeys
      plainModules =
        Map.fromList $
          (evictionSharedKey, moduleSourceOf evictionSharedModule)
          : [(evictionHelperKey leaf, moduleSourceOf (evictionHelperModule leaf)) | leaf <- evictionLeafKeys]
      testModules =
        Map.fromList
          [(evictionTestKey leaf, moduleSourceOf (evictionTestModule leaf)) | leaf <- evictionLeafKeys]
  writeProjectSources sessionEnv.sourceDir plainModules
  writeTestModuleSources sessionEnv.sourceDir testModules

  sharedMeta <- runMetadata sessionEnv (metadataArgs sessionEnv False) evictionSharedUnit
  sharedCompile <- runCompile sessionEnv plainArgs evictionSharedKey
  leafMeta <- mapM (runMetadata sessionEnv (metadataArgs sessionEnv False)) leaves
  helperCompiles <- mapM (runCompile sessionEnv plainArgs . evictionHelperKey) evictionLeafKeys

  concurrentResults <-
    mapConcurrently (\ key -> runEvaluate sessionEnv plainArgs (testFunctionName key) key) (evictionTestKey <$> evictionLeafKeys)

  reorderResults <- mapM evalPlain (evictionTestKey <$> otherLeaves)
  prepResult <- evalPlain triggerKey

  limit <- triggerGroupSize sessionEnv
  evictionResult <- runEvaluate sessionEnv (evictionArgs limit) (testFunctionName triggerKey) triggerKey

  pure (
    [sharedMeta, sharedCompile]
      ++ leafMeta
      ++ helperCompiles
      ++ concurrentResults
      ++ reorderResults
      ++ [prepResult, evictionResult]
    )
  where
    triggerKey = evictionTestKey evictionTriggerLeaf

    otherLeaves = filter (/= evictionTriggerLeaf) evictionLeafKeys

    evalPlain key = runEvaluate sessionEnv plainArgs (testFunctionName key) key

    plainArgs _ = (sessionEnv.shared.baseArgs, mempty)

    evictionArgs limit _ =
      (
        sessionEnv.shared.baseArgs {
          features = sessionEnv.shared.baseArgs.features {lazyByteCodeCacheLimit = Just limit}
          },
        mempty
        )

-- | Runs the eviction-focused graph above through concurrent interpreted test execution (mirroring
-- 'test_concurrentInterpreted''s harness), then the deterministic re-ordering and single-threaded eviction trigger
-- described in 'runEvictionGraph', and asserts the outcome: the shared module and 'evictionTriggerLeaf''s helper
-- module both survive, while the other two leaves' helper modules are evicted as the least-recently-used entries
-- once the measured limit is exceeded.
test_bcoEvictionConcurrentInterpreted :: TestTree
test_bcoEvictionConcurrentInterpreted =
  withTestEnv \ getTestEnv ->
    unitTest "concurrent interpreted test execution: BCO eviction under a lazy bytecode cache limit" do
      testEnv <- liftIO getTestEnv
      sessionEnv <- liftIO (newSessionEnv (enableLazyByteCode testEnv))
      results <- liftIO (runEvictionGraph sessionEnv)
      annotateShow results
      assert (all (== RequestSuccess) results)

      cacheAfter <- liftIO do
        WorkerState {make} <- readMVar sessionEnv.env.state
        pure make.bcoCache

      limit <- liftIO (triggerGroupSize sessionEnv)

      let totalSize = sum (size <$> Map.elems cacheAfter)
          survivingHelpers = [leaf | leaf <- evictionLeafKeys, Map.member (evictionModuleFor (evictionHelperKey leaf)) cacheAfter]

      annotateShow (totalSize, Map.keys cacheAfter <&> evictionModuleName)
      totalSize === limit
      assert (Map.member (evictionModuleFor evictionSharedKey) cacheAfter)
      survivingHelpers === [evictionTriggerLeaf]
  where
    evictionModuleName m = (unitFS m.moduleUnit, moduleNameFS m.moduleName)

