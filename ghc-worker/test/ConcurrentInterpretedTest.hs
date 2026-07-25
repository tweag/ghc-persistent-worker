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
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Hedgehog (annotateShow, assert)
import Test.Build (metadataArgs, runCompile, runEvaluate, runMetadata)
import Test.Data.Env (SessionEnv (..), TestEnv (..))
import Test.Data.Project (BuildModule (..), GenUnit (..), ModuleKey (..), ModuleSource (..), UnitKey)
import Test.Data.Scheduler (RequestResult (..))
import Test.Env (newSessionEnv, withTestEnv)
import Test.Path (testFunctionName)
import Test.Run (unitTest)
import Test.Source (writeProjectSources, writeTestModuleSources)
import Test.Tasty (TestTree)

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
