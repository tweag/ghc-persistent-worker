{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
-- | End-to-end tests for the standalone GHC server build pipeline.
--
-- Creates synthetic multi-unit projects in temporary directories and builds
-- them under various scheduling scenarios.
module Test.BuildTest where

import Control.Concurrent.Async (cancel)
import Control.Concurrent.MVar (MVar, newMVar, readMVar)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as LBS
import Data.Char (toLower)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.List (isSuffixOf, sort)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import GHC (mkModuleName, moduleNameString)
import GHC.Stack (HasCallStack, withFrozenCallStack)
import GHC.Unit.Home.Graph (HomeUnitEnv (..), unitEnv_lookup)
import GHC.Unit.Home.PackageTable (lookupHpt)
import GHC.Unit.Types (stringToUnit, toUnitId)
import GhcServer.Build (
  Build (..),
  BuildResult (..),
  awaitBuild,
  newBuild,
  newBuildState,
  runBuild,
  scheduleBatch,
  stopBuild,
  )
import GhcServer.Build.Execute (executeModuleTask)
import GhcServer.Build.Schedule (TaskKey (..), emptyBuildExt)
import GhcServer.Cache (cacheExists)
import GhcServer.Data.BuildEnv (BuildEnv (..))
import GhcServer.Data.BuildEvent (BuildEvent (..), BuildEvents, newBuildEvents, readEvents)
import GhcServer.Data.Request (ScheduleRequest (..), UnitRequest (..))
import GhcServer.Data.Unit (ClientModule (..), Project (..), Unit (..), UnitCache (..), UnitName (..))
import GhcServer.Data.UnitConfig (UnitConfig (..))
import GhcServer.Log (newLogger)
import GhcServer.Path (osPath)
import GhcServer.Project (discoverProject)
import GhcServer.Scheduler (SchedulerDecision (..), TaskResult (..))
import Hedgehog (TestT, annotate, assert, diff, property, test, withTests, (===))
import Prelude hiding (log)
import System.Directory (createDirectoryIfMissing, listDirectory, removeFile, removePathForcibly)
import System.IO.Temp (createTempDirectory, getCanonicalTemporaryDirectory)
import System.OsPath (OsPath)
import System.Timeout (timeout)
import Test.Tasty (DependencyType (..), TestName, TestTree, dependentTestGroup, withResource)
import Test.Tasty.Hedgehog (testProperty)
import Types.Args (emptyArgs)
import Types.State (WorkerState (..))
import Types.State.Make (MakeState (..))

-- ---------------------------------------------------------------------------
-- Low-level helpers
-- ---------------------------------------------------------------------------

acquireTemp :: FilePath -> IO FilePath
acquireTemp name = do
  tmpBase <- getCanonicalTemporaryDirectory
  createTempDirectory tmpBase name

-- | Use a temp dir for a Tasty test.
-- We use this instead of @withSystemTempDirectory@ because 'TestT' doesn't have 'MonadMask'.
withTemp :: FilePath -> (IO FilePath -> TestTree) -> TestTree
withTemp name =
  withResource (acquireTemp name) removePathForcibly

writeProjectFile :: FilePath -> FilePath -> String -> IO ()
writeProjectFile base rel content =
  writeFile (base ++ "/" ++ rel) content

writeUnitConfig :: FilePath -> FilePath -> UnitConfig -> IO ()
writeUnitConfig base unitDir config =
  LBS.writeFile (base ++ "/" ++ unitDir ++ "/unit.json") (encode config)

baseGhcArgs :: [String]
baseGhcArgs = []

-- ---------------------------------------------------------------------------
-- Test environment
-- ---------------------------------------------------------------------------

-- | Discovered project environment, created once per test.
data TestProject =
  TestProject {
    root :: FilePath,
    rootOs :: OsPath,
    project :: Project,
    outputDir :: OsPath,
    tmpDir :: OsPath
  }

acquireProject :: IO FilePath -> IO TestProject
acquireProject acquireRoot = do
  root <- acquireRoot
  let
    rootOs = osPath root
    outputDir = osPath (root ++ "/output")
    tmpDir = osPath (root ++ "/tmp")
  project <- discoverProject rootOs outputDir tmpDir
  pure TestProject {root, rootOs, project, outputDir, tmpDir}

newBuildEnv :: TestProject -> MVar WorkerState -> IO (BuildEnv, BuildEvents)
newBuildEnv tp stateVar = do
  log <- newLogger False
  events <- newBuildEvents
  extDepsDb <- newMVar Nothing
  diffVar <- newMVar Map.empty
  pure (BuildEnv {
    baseArgs = emptyArgs Map.empty,
    projectRoot = tp.rootOs,
    outputDir = tp.outputDir,
    tmpDir = tp.tmpDir,
    stateVar,
    project = tp.project,
    log,
    events,
    instrChan = Nothing,
    extDepsDb,
    diff = diffVar
  }, events)

-- ---------------------------------------------------------------------------
-- Build operations (MonadIO)
-- ---------------------------------------------------------------------------

type Steps = [(UnitName, UnitRequest)]

-- | Per-task timeout for tests (seconds).  Matches the scheduler's @taskTimeout@.
testTaskTimeout :: Int
testTaskTimeout = 3

-- | Overall build timeout for tests (microseconds).  Covers scheduler-level deadlocks.
testBuildTimeoutUs :: Int
testBuildTimeoutUs = 10 * 1_000_000

-- | Wrap a build action with an overall timeout.  Fails hard if the build does not
-- complete within 'testBuildTimeoutUs', covering scheduler-level hangs that the
-- per-task timeout cannot catch.
timedBuild :: IO a -> IO a
timedBuild action =
  timeout testBuildTimeoutUs action >>= \ case
    Just a  -> pure a
    Nothing -> fail ("Build deadlocked (timed out after " ++ show (testBuildTimeoutUs `div` 1_000_000) ++ "s)")

-- | Run a fresh build with the given steps and explicit @recompile@\/@rebuild@ flags.
runFreshWith :: MonadIO m => Bool -> Bool -> TestProject -> Steps -> m (BuildResult, [BuildEvent])
runFreshWith recompile rebuild tp steps = liftIO $ timedBuild do
  stateVar <- newBuildState
  (env, events) <- newBuildEnv tp stateVar
  result <- runBuild 4 testTaskTimeout env ScheduleRequest {steps, recompile, rebuild}
  evs <- readEvents events
  pure (result, evs)

-- | Run a fresh build with the given schedule steps.
runFresh :: MonadIO m => TestProject -> Steps -> m (BuildResult, [BuildEvent])
runFresh = runFreshWith False False

-- | Run a fresh build with @--recompile@ semantics (force explicitly named units stale).
runFreshRecompile :: MonadIO m => TestProject -> Steps -> m (BuildResult, [BuildEvent])
runFreshRecompile = runFreshWith True False

-- | Run a fresh build with empty request (build everything).
runFreshAll :: MonadIO m => TestProject -> m (BuildResult, [BuildEvent])
runFreshAll tp =
  runFresh tp []

-- | 'stopBuild' wrapped with 'timedBuild'.
timedStop :: Build -> IO BuildResult
timedStop cb = timedBuild (stopBuild cb)

-- | Create a new 'Build' for multi-batch tests.
--
-- Every scheduler decision is accumulated into the returned 'IORef', in chronological order, so
-- tests can assert on the scheduler's decisions (see 'buildDecisions'), not merely on the
-- compilation events that resulted from them.
newTestBuild :: MonadIO m => TestProject -> m (Build, BuildEvents, IORef [SchedulerDecision TaskKey])
newTestBuild tp = liftIO do
  stateVar <- newBuildState
  (env, events) <- newBuildEnv tp stateVar
  decisionsRef <- newIORef []
  cb <- newBuild (\ decision -> atomicModifyIORef' decisionsRef \ ds -> (decision : ds, ())) 4 testTaskTimeout env
  pure (cb, events, decisionsRef)

-- | Read a 'newTestBuild' decision log in chronological order.
buildDecisions :: IORef [SchedulerDecision TaskKey] -> IO [SchedulerDecision TaskKey]
buildDecisions = fmap reverse . readIORef

-- | Run a fresh build with @maxJobs=1@ and return both the result and recorded events.
runFreshWithEvents :: MonadIO m => TestProject -> Steps -> m (BuildResult, [BuildEvent])
runFreshWithEvents = runFreshWithEvents' False False

-- | 'runFreshWithEvents' with explicit @recompile@\/@rebuild@ flags.
runFreshWithEvents' :: MonadIO m => Bool -> Bool -> TestProject -> Steps -> m (BuildResult, [BuildEvent])
runFreshWithEvents' recompile rebuild tp steps = liftIO $ timedBuild do
  stateVar <- newBuildState
  (env, events) <- newBuildEnv tp stateVar
  result <- runBuild 1 testTaskTimeout env ScheduleRequest {steps, recompile, rebuild}
  evs <- readEvents events
  pure (result, evs)

-- | Run a fresh build and return the 'WorkerState' MVar alongside the result and events.
-- The returned 'MVar' can be used to inspect the post-build HPT.
runFreshWithState :: MonadIO m => TestProject -> Steps -> m (BuildResult, [BuildEvent], MVar WorkerState)
runFreshWithState = runFreshWithState' False False

-- | 'runFreshWithState' with explicit @recompile@\/@rebuild@ flags.
runFreshWithState' :: MonadIO m => Bool -> Bool -> TestProject -> Steps -> m (BuildResult, [BuildEvent], MVar WorkerState)
runFreshWithState' recompile rebuild tp steps = liftIO $ timedBuild do
  stateVar <- newBuildState
  (env, events) <- newBuildEnv tp stateVar
  result <- runBuild 1 testTaskTimeout env ScheduleRequest {steps, recompile, rebuild}
  evs <- readEvents events
  pure (result, evs, stateVar)

deleteUnitCache :: MonadIO m => TestProject -> String -> m ()
deleteUnitCache tp name = liftIO do
  removePathForcibly (tp.root ++ "/cache/" ++ name)

-- | Delete per-module @.dyn_hi@ interface files for a unit, leaving the metadata cache
-- and object files intact.
deleteModuleHiFiles :: MonadIO m => TestProject -> String -> m ()
deleteModuleHiFiles tp name = liftIO do
  let outputUnitDir = tp.root ++ "/output/" ++ name
  entries <- listDirectory outputUnitDir
  mapM_ removeFile [outputUnitDir ++ "/" ++ e | e <- entries, ".dyn_hi" `isSuffixOf` e]

-- ---------------------------------------------------------------------------
-- Event extraction
-- ---------------------------------------------------------------------------

-- | Extract unit names for which metadata ran.
eventMetadata :: [BuildEvent] -> [String]
eventMetadata events =
  sort [name.string | MetadataRan name <- events]

-- | Extract "unit:module" strings for compiled modules.
eventCompiled :: [BuildEvent] -> [String]
eventCompiled events =
  sort [name.string ++ ":" ++ moduleNameString modName | ModuleCompiled name modName <- events]

-- | Extract unit names that had at least one module compiled.
eventCompiledUnits :: [BuildEvent] -> [String]
eventCompiledUnits events =
  sort $ Set.toList $ Set.fromList [name.string | ModuleCompiled name _ <- events]

-- ---------------------------------------------------------------------------
-- Scheduler decisions
-- ---------------------------------------------------------------------------

-- | Split the scheduler's decision log into per-generation segments, in order.
--
-- Each segment starts at the 'DecisionGeneration' marker the scheduler records when it accepts
-- a request, so segment @n@ contains exactly the decisions attributable to the @n@-th batch.
-- Anything recorded before the first request (there should be nothing) is dropped.
decisionGenerations :: [SchedulerDecision TaskKey] -> [[SchedulerDecision TaskKey]]
decisionGenerations =
  drop 1 . foldr step [[]]
  where
    step d@(DecisionGeneration _) (seg : segs) = [] : (d : seg) : segs
    step d (seg : segs) = (d : seg) : segs
    step _ [] = []

-- | The decisions of the @n@-th batch (1-based), or the empty list if there was no such batch.
decisionBatch :: Int -> [SchedulerDecision TaskKey] -> [SchedulerDecision TaskKey]
decisionBatch n decisions =
  case drop (n - 1) (decisionGenerations decisions) of
    seg : _ -> seg
    [] -> []

-- | \"unit:module\" strings of the compile tasks the scheduler activated.
decisionActivated :: [SchedulerDecision TaskKey] -> [String]
decisionActivated decisions =
  distinct [name.string ++ ":" ++ moduleNameString modName | DecisionActivated (ResolvedModule name modName) _ <- decisions]

-- | \"unit:module\" strings of the compile tasks the scheduler declined to activate because
-- their resolution was not newer than their last completion.
--
-- Deduplicated: a task that stays in the pending pool is re-examined by every promotion pass of
-- the batch, so the same verdict is legitimately recorded several times.
decisionUpToDate :: [SchedulerDecision TaskKey] -> [String]
decisionUpToDate decisions =
  distinct [name.string ++ ":" ++ moduleNameString modName | DecisionUpToDate (ResolvedModule name modName) _ _ <- decisions]

-- | \"unit:module\" strings of the compile tasks that were deduped against an in-flight run.
decisionDeduped :: [SchedulerDecision TaskKey] -> [String]
decisionDeduped decisions =
  distinct [name.string ++ ":" ++ moduleNameString modName | DecisionDeduped (ResolvedModule name modName) _ <- decisions]

-- | \"unit:module\" strings of the compile tasks that were judged up to date and never activated
-- during the batch.
--
-- The subtraction matters: a module can be judged up to date early in a batch (against the
-- previous batch's resolution) and be activated later, once the metadata task that finds it
-- stale has propagated.  Only the modules that were never activated were actually skipped.
decisionSkipped :: [SchedulerDecision TaskKey] -> [String]
decisionSkipped decisions =
  filter (`notElem` decisionActivated decisions) (decisionUpToDate decisions)

distinct :: Ord a => [a] -> [a]
distinct = Set.toAscList . Set.fromList

-- ---------------------------------------------------------------------------
-- Assertions
-- ---------------------------------------------------------------------------

prettyBuildResult :: String -> BuildResult -> String
prettyBuildResult label result =
  unlines $
    [label ++ ":",
     "  success: " ++ show result.success,
     "  metadata errors:"]
    ++ ["    " ++ u.string ++ ": " ++ msg | (u, msg) <- result.metadataErrors]
    ++ ["  compile errors:"]
    ++ ["    " ++ u.string ++ ":" ++ show modName ++ ": " ++ msg | (u, modName, msg) <- result.compileErrors]

assertSuccess :: HasCallStack => String -> BuildResult -> TestT IO ()
assertSuccess label result =
  withFrozenCallStack do
    annotate (prettyBuildResult label result)
    assert result.success

assertHasMetadata :: HasCallStack => String -> [BuildEvent] -> TestT IO ()
assertHasMetadata unitName events =
  withFrozenCallStack do
    diff unitName elem (eventMetadata events)

assertNoMetadata :: HasCallStack => String -> [BuildEvent] -> TestT IO ()
assertNoMetadata unitName events =
  withFrozenCallStack do
    diff unitName notElem (eventMetadata events)

assertHasCompiled :: HasCallStack => String -> [BuildEvent] -> TestT IO ()
assertHasCompiled unitName events =
  withFrozenCallStack do
    diff unitName elem (eventCompiledUnits events)

assertNoCompiled :: HasCallStack => String -> [BuildEvent] -> TestT IO ()
assertNoCompiled unitName events =
  withFrozenCallStack do
    diff unitName notElem (eventCompiledUnits events)

assertCacheExists :: HasCallStack => TestProject -> String -> TestT IO ()
assertCacheExists tp name =
  withFrozenCallStack do
    exists <- liftIO (cacheExists (unitCache tp.project (UnitName name)))
    assert exists

unitCache :: Project -> UnitName -> UnitCache
unitCache project name =
  case Map.lookup name project.units of
    Just unit -> unit.cache
    Nothing -> error ("Unit not found: " ++ name.string)

-- | Look up a module in a specific unit's HPT from the 'WorkerState'.
lookupHptModule :: MVar WorkerState -> String -> String -> IO Bool
lookupHptModule stateVar unitStr modStr = do
  state <- readMVar stateVar
  let uid = toUnitId (stringToUnit unitStr)
      hue = unitEnv_lookup uid state.make.hug
      hpt = homeUnitEnv_hpt hue
      modName = mkModuleName modStr
  maybe False (const True) <$> lookupHpt hpt modName

-- | Assert that a module is present in a unit's HPT in the 'WorkerState'.
assertHptHasModule :: HasCallStack => MVar WorkerState -> String -> String -> TestT IO ()
assertHptHasModule stateVar unitStr modStr =
  withFrozenCallStack do
    present <- liftIO (lookupHptModule stateVar unitStr modStr)
    annotate ("Expected module " ++ modStr ++ " in unit " ++ unitStr ++ " HPT")
    assert present

-- ---------------------------------------------------------------------------
-- Project-scoped test combinators
-- ---------------------------------------------------------------------------

-- | Run a test with the small 2-unit project.
smallTest :: TestName -> (TestProject -> TestT IO ()) -> TestTree
smallTest =
  projectTest "ghc-server-small" createSmallProject

-- | Run a test with the 4-unit project.
largeTest :: TestName -> (TestProject -> TestT IO ()) -> TestTree
largeTest =
  projectTest "ghc-server-large" createLargeProject

-- | Run a test with the 3-unit chain project.
chainTest :: TestName -> (TestProject -> TestT IO ()) -> TestTree
chainTest =
  projectTest "ghc-server-chain" createChainProject

-- | Run a test with the intra-dep project.
intraDepTest :: TestName -> (TestProject -> TestT IO ()) -> TestTree
intraDepTest =
  projectTest "ghc-server-intradep" createIntraDepProject

projectTest :: FilePath -> (FilePath -> IO ()) -> TestName -> (TestProject -> TestT IO ()) -> TestTree
projectTest dirName create name body =
  withTemp dirName \ acquire ->
    testProperty name $ withTests 1 $ property $ test do
      tp <- liftIO do
        root <- acquire
        create root
        acquireProject (pure root)
      body tp

-- ---------------------------------------------------------------------------
-- Two-unit project: unit0 (leaf) \u2192 unit1 (depends on unit0)
-- ---------------------------------------------------------------------------

createSmallProject :: FilePath -> IO ()
createSmallProject root = do
  createDirectoryIfMissing True (root ++ "/unit0")
  createDirectoryIfMissing True (root ++ "/unit1")

  writeUnitConfig root "unit0" UnitConfig {deps = [], args = baseGhcArgs}
  writeProjectFile root "unit0/A.hs" $ unlines
    [ "module A where"
    , ""
    , "hello :: String"
    , "hello = \"hello from unit0\""
    ]

  writeUnitConfig root "unit1" UnitConfig {deps = ["unit0"], args = baseGhcArgs}
  writeProjectFile root "unit1/B.hs" $ unlines
    [ "module B where"
    , ""
    , "import A (hello)"
    , ""
    , "greeting :: String"
    , "greeting = hello ++ \" world\""
    ]

-- ---------------------------------------------------------------------------
-- Four-unit project: unit0 (leaf), unit1/unit2 (dep on unit0),
-- unit3 (dep on unit1 + unit2).
-- Four modules each.
-- ---------------------------------------------------------------------------

createLargeProject :: FilePath -> IO ()
createLargeProject root = do
  writeUnit "unit0" [] \ u -> do
    writeModule u "A0" [] "a0 = \"a0\""
    writeModule u "B0" [] "b0 = \"b0\""
    writeModule u "C0" [] "c0 = \"c0\""
    writeModule u "D0" [] "d0 = \"d0\""

  writeUnit "unit1" ["unit0"] \ u -> do
    writeModule u "A1" ["A0"] "a1 = a0 ++ \"_a1\""
    writeModule u "B1" [] "b1 = \"b1\""
    writeModule u "C1" [] "c1 = \"c1\""
    writeModule u "D1" [] "d1 = \"d1\""

  writeUnit "unit2" ["unit0"] \ u -> do
    writeModule u "A2" ["B0"] "a2 = b0 ++ \"_a2\""
    writeModule u "B2" [] "b2 = \"b2\""
    writeModule u "C2" [] "c2 = \"c2\""
    writeModule u "D2" [] "d2 = \"d2\""

  writeUnit "unit3" ["unit1", "unit2"] \ u -> do
    writeModule u "A3" ["A1"] "a3 = a1 ++ \"_a3\""
    writeModule u "B3" ["A2"] "b3 = a2 ++ \"_b3\""
    writeModule u "C3" [] "c3 = \"c3\""
    writeModule u "D3" [] "d3 = \"d3\""
  where
    writeUnit name deps body = do
      let dir = root ++ "/" ++ name
      createDirectoryIfMissing True dir
      writeUnitConfig root name UnitConfig {deps, args = baseGhcArgs}
      body name

    writeModule unitName modName imports body =
      writeProjectFile root (unitName ++ "/" ++ modName ++ ".hs") $ unlines $
        ["module " ++ modName ++ " where"]
        ++ ["import " ++ imp ++ " (" ++ lcFirst imp ++ ")" | imp <- imports]
        ++ ["", lcFirst modName ++ " :: String", body]

    lcFirst (c : cs) = toLower c : cs
    lcFirst [] = []

-- ---------------------------------------------------------------------------
-- Three-unit chain: unit0 -> unit1 -> unit2 (leaf)
-- Each unit has 2 modules. unit0 imports from unit1, unit1 imports from unit2.
-- ---------------------------------------------------------------------------

createChainProject :: FilePath -> IO ()
createChainProject root = do
  writeUnit "unit2" [] \ u -> do
    writeModule u "A2" [] "a2 = \"a2\""
    writeModule u "B2" [] "b2 = \"b2\""

  writeUnit "unit1" ["unit2"] \ u -> do
    writeModule u "A1" ["A2"] "a1 = a2 ++ \"_a1\""
    writeModule u "B1" [] "b1 = \"b1\""

  writeUnit "unit0" ["unit1"] \ u -> do
    writeModule u "A0" ["A1"] "a0 = a1 ++ \"_a0\""
    writeModule u "B0" [] "b0 = \"b0\""
  where
    writeUnit name deps body = do
      let dir = root ++ "/" ++ name
      createDirectoryIfMissing True dir
      writeUnitConfig root name UnitConfig {deps, args = baseGhcArgs}
      body name

    writeModule unitName modName imports body =
      writeProjectFile root (unitName ++ "/" ++ modName ++ ".hs") $ unlines $
        ["module " ++ modName ++ " where"]
        ++ ["import " ++ imp ++ " (" ++ lcFirst imp ++ ")" | imp <- imports]
        ++ ["", lcFirst modName ++ " :: String", body]

    lcFirst (c : cs) = toLower c : cs
    lcFirst [] = []

-- ---------------------------------------------------------------------------
-- Intra-dep project: unit0 (U0M0), unit1 -> unit0 (U1M0, U1M1 -> [U1M0, U0M0])
-- ---------------------------------------------------------------------------

createIntraDepProject :: FilePath -> IO ()
createIntraDepProject root = do
  createDirectoryIfMissing True (root ++ "/unit0")
  writeUnitConfig root "unit0" UnitConfig {deps = [], args = baseGhcArgs}
  writeProjectFile root "unit0/U0M0.hs" $ unlines
    [ "module U0M0 where"
    , ""
    , "u0m0 :: String"
    , "u0m0 = \"u0m0\""
    ]

  createDirectoryIfMissing True (root ++ "/unit1")
  writeUnitConfig root "unit1" UnitConfig {deps = ["unit0"], args = baseGhcArgs}
  writeProjectFile root "unit1/U1M0.hs" $ unlines
    [ "module U1M0 where"
    , ""
    , "u1m0 :: String"
    , "u1m0 = \"u1m0\""
    ]
  writeProjectFile root "unit1/U1M1.hs" $ unlines
    [ "module U1M1 where"
    , ""
    , "import U1M0 (u1m0)"
    , "import U0M0 (u0m0)"
    , ""
    , "u1m1 :: String"
    , "u1m1 = u1m0 ++ u0m0"
    ]

-- ---------------------------------------------------------------------------
-- Test group: Basic dispatch
-- ---------------------------------------------------------------------------

test_buildAll :: TestTree
test_buildAll =
  smallTest "build entire project" \ tp -> do
    (result, events) <- runFreshAll tp
    assertSuccess "build all" result
    assertHasMetadata "unit0" events
    assertHasMetadata "unit1" events
    assertHasCompiled "unit0" events
    assertHasCompiled "unit1" events

test_metadataOnly :: TestTree
test_metadataOnly =
  smallTest "metadata only" \ tp -> do
    (result, events) <- runFresh tp [(UnitName "unit0", UnitMetadata)]
    assertSuccess "metadata only" result
    assertHasMetadata "unit0" events
    assertNoCompiled "unit0" events
    assertNoMetadata "unit1" events

test_singleUnit :: TestTree
test_singleUnit =
  smallTest "build single unit" \ tp -> do
    (result, events) <- runFresh tp [(UnitName "unit0", UnitAll)]
    assertSuccess "single unit" result
    assertHasMetadata "unit0" events
    assertHasCompiled "unit0" events
    assertNoMetadata "unit1" events
    assertNoCompiled "unit1" events

test_specificModule :: TestTree
test_specificModule =
  smallTest "build specific module" \ tp -> do
    (result, _events) <- runFresh tp
      [ (UnitName "unit0", UnitAll)
      , (UnitName "unit1", UnitMetadata)
      , (UnitName "unit1", UnitModules [ClientModule "B"])
      ]
    assertSuccess "specific module" result

test_modulesOnly :: TestTree
test_modulesOnly =
  smallTest "modules only after prior build" \ tp -> do
    (result1, _) <- runFreshAll tp
    assertSuccess "initial build" result1
    (result2, _) <- runFresh tp
      [ (UnitName "unit0", UnitMetadata)
      , (UnitName "unit1", UnitModulesOnly)
      ]
    assertSuccess "modules only" result2

test_basicDispatch :: TestTree
test_basicDispatch =
  dependentTestGroup "Basic dispatch" AllFinish
    [ test_buildAll
    , test_metadataOnly
    , test_singleUnit
    , test_specificModule
    , test_modulesOnly
    ]

-- ---------------------------------------------------------------------------
-- Test group: Cache restore
-- ---------------------------------------------------------------------------

test_cacheRestoreAll :: TestTree
test_cacheRestoreAll =
  smallTest "full rebuild from cache" \ tp -> do
    (result1, _) <- runFreshAll tp
    assertSuccess "first build" result1
    assertCacheExists tp "unit0"
    assertCacheExists tp "unit1"
    (result2, events2) <- runFreshAll tp
    assertSuccess "second build" result2
    [] === eventMetadata events2
    -- Sources unchanged: nothing is stale, so no module is recompiled.
    [] === eventCompiled events2

test_cacheMetadataNoOp :: TestTree
test_cacheMetadataNoOp =
  smallTest "metadata-only for cached unit is a no-op" \ tp -> do
    (result1, _) <- runFreshAll tp
    assertSuccess "initial build" result1
    (result2, events2) <- runFresh tp [(UnitName "unit0", UnitMetadata)]
    assertSuccess "cached metadata" result2
    [] === eventMetadata events2
    [] === eventCompiled events2

test_cacheModulesOnly :: TestTree
test_cacheModulesOnly =
  smallTest "modules-only for cached unit" \ tp -> do
    (result1, _) <- runFreshAll tp
    assertSuccess "initial build" result1
    -- Unchanged sources: nothing stale, nothing compiled.
    (result2, events2) <- runFresh tp [(UnitName "unit1", UnitModulesOnly)]
    assertSuccess "cached modules-only" result2
    assertNoMetadata "unit1" events2
    assertNoCompiled "unit1" events2
    -- With --recompile, the explicitly named unit's modules are forced stale and recompiled.
    (result3, events3) <- runFreshRecompile tp [(UnitName "unit1", UnitModulesOnly)]
    assertSuccess "forced modules-only" result3
    assertNoMetadata "unit1" events3
    assertHasCompiled "unit1" events3
    assertNoMetadata "unit0" events3
    assertNoCompiled "unit0" events3

test_cacheMixedFreshAndCached :: TestTree
test_cacheMixedFreshAndCached =
  smallTest "mixed cached and fresh units" \ tp -> do
    (result1, _) <- runFreshAll tp
    assertSuccess "initial build" result1
    assertCacheExists tp "unit0"
    deleteUnitCache tp "unit1"
    (result2, events2) <- runFresh tp [(UnitName "unit1", UnitAll)]
    assertSuccess "rebuild unit1" result2
    assertNoMetadata "unit0" events2
    assertHasMetadata "unit1" events2
    assertHasCompiled "unit1" events2

test_cacheSpecificModules :: TestTree
test_cacheSpecificModules =
  smallTest "specific modules for cached unit" \ tp -> do
    (result1, _) <- runFreshAll tp
    assertSuccess "initial build" result1
    (result2, events2) <- runFreshRecompile tp [(UnitName "unit1", UnitModules [ClientModule "B"])]
    assertSuccess "cached specific module" result2
    assertNoMetadata "unit0" events2
    assertNoMetadata "unit1" events2
    assertHasCompiled "unit1" events2

-- | Regression test for a bug where 'UnitModules' enabled compilation for every source
-- file of the unit instead of just the requested module(s), which allowed unrelated
-- sibling modules to be scheduled\/compiled alongside the one actually requested (and, when
-- the same unit received multiple concurrent per-module requests -- as the UI's
-- project-root and unit-header 'build' actions do -- caused the same module to be
-- redundantly resubmitted and dispatched twice).
test_cacheSpecificModuleDoesNotCompileSiblings :: TestTree
test_cacheSpecificModuleDoesNotCompileSiblings =
  largeTest "requesting a specific module does not compile independent sibling modules" \ tp -> do
    (result1, _) <- runFreshAll tp
    assertSuccess "initial build" result1
    (result2, events2) <- runFreshRecompile tp [(UnitName "unit1", UnitModules [ClientModule "B1"])]
    assertSuccess "cached specific module" result2
    assertNoMetadata "unit1" events2
    assertEventsContain [ModuleCompiled (UnitName "unit1") (mkModuleName "B1")] events2
    assertNoEvent isUnrelatedUnit1Compile events2
  where
    isUnrelatedUnit1Compile = \case
      ModuleCompiled (UnitName "unit1") m -> m /= mkModuleName "B1"
      _ -> False

test_cacheDeleteLeafRebuildsChain :: TestTree
test_cacheDeleteLeafRebuildsChain =
  smallTest "deleting leaf cache: leaf and dependents recompiled" \ tp -> do
    (result1, _) <- runFreshAll tp
    assertSuccess "initial build" result1
    deleteUnitCache tp "unit0"
    (result2, events2) <- runFresh tp [(UnitName "unit1", UnitAll)]
    assertSuccess "chain rebuild" result2
    assertHasMetadata "unit0" events2
    -- unit0 has no previous module graph, so all of its modules are stale and recompiled,
    -- along with the downstream closure in unit1.
    assertHasCompiled "unit0" events2
    assertNoMetadata "unit1" events2
    assertHasCompiled "unit1" events2

test_cacheDeleteMiddleUnit :: TestTree
test_cacheDeleteMiddleUnit =
  largeTest "delete middle unit cache in chain" \ tp -> do
    (result1, _) <- runFreshAll tp
    assertSuccess "initial build" result1
    deleteUnitCache tp "unit1"
    (result2, events2) <- runFresh tp [(UnitName "unit1", UnitAll)]
    assertSuccess "middle unit rebuild" result2
    assertNoMetadata "unit0" events2
    assertHasMetadata "unit1" events2
    assertHasCompiled "unit1" events2

test_cacheRestore :: TestTree
test_cacheRestore =
  dependentTestGroup "Cache restore" AllFinish
    [ test_cacheRestoreAll
    , test_cacheMetadataNoOp
    , test_cacheModulesOnly
    , test_cacheMixedFreshAndCached
    , test_cacheSpecificModules
    , test_cacheSpecificModuleDoesNotCompileSiblings
    , test_cacheDeleteLeafRebuildsChain
    , test_cacheDeleteMiddleUnit
    ]

-- ---------------------------------------------------------------------------
-- Test group: Pending pool and promotion
-- ---------------------------------------------------------------------------

test_implicitDeps :: TestTree
test_implicitDeps =
  largeTest "implicit dep units are built" \ tp -> do
    (result, events) <- runFresh tp [(UnitName "unit3", UnitAll)]
    assertSuccess "implicit deps" result
    assertHasMetadata "unit0" events
    assertHasMetadata "unit1" events
    assertHasMetadata "unit2" events
    assertHasMetadata "unit3" events
    assertHasCompiled "unit0" events
    assertHasCompiled "unit1" events
    assertHasCompiled "unit2" events
    assertHasCompiled "unit3" events

test_pendingThenEnable :: TestTree
test_pendingThenEnable =
  smallTest "pending tasks enabled by later batch" \ tp -> do
    (cb, evRef, _decisions) <- newTestBuild tp
    liftIO $ scheduleBatch cb ScheduleRequest {
      steps = [(UnitName "unit0", UnitMetadata)],
      recompile = False, rebuild = False
    }
    liftIO $ scheduleBatch cb ScheduleRequest {
      steps = [(UnitName "unit0", UnitAll)],
      recompile = False, rebuild = False
    }
    result <- liftIO (timedStop cb)
    events <- liftIO (readEvents evRef)
    assertSuccess "pending then enable" result
    assertHasMetadata "unit0" events
    assertHasCompiled "unit0" events

test_metadataOnlyLeavesTasksPending :: TestTree
test_metadataOnlyLeavesTasksPending =
  smallTest "metadata-only leaves compile tasks pending" \ tp -> do
    (cb, evRef, _decisions) <- newTestBuild tp
    liftIO $ scheduleBatch cb ScheduleRequest {
      steps = [(UnitName "unit0", UnitMetadata)],
      recompile = False, rebuild = False
    }
    result <- liftIO (timedStop cb)
    events <- liftIO (readEvents evRef)
    assertSuccess "metadata pending" result
    assertHasMetadata "unit0" events
    assertNoCompiled "unit0" events

test_enabledNotDowngraded :: TestTree
test_enabledNotDowngraded =
  smallTest "enabled flag not downgraded by metadata request" \ tp -> do
    (cb, evRef, _decisions) <- newTestBuild tp
    liftIO $ scheduleBatch cb ScheduleRequest {
      steps = [(UnitName "unit1", UnitAll)],
      recompile = False, rebuild = False
    }
    liftIO $ scheduleBatch cb ScheduleRequest {
      steps = [(UnitName "unit0", UnitMetadata)],
      recompile = False, rebuild = False
    }
    result <- liftIO (timedStop cb)
    events <- liftIO (readEvents evRef)
    assertSuccess "enabled not downgraded" result
    assertHasCompiled "unit0" events
    assertHasCompiled "unit1" events

test_sameUnitMultipleRequestTypes :: TestTree
test_sameUnitMultipleRequestTypes =
  smallTest "same unit with metadata then all in one batch" \ tp -> do
    (result, events) <- runFresh tp
      [ (UnitName "unit0", UnitMetadata)
      , (UnitName "unit0", UnitAll)
      ]
    assertSuccess "same unit multi" result
    assertHasMetadata "unit0" events
    assertHasCompiled "unit0" events

test_metadataOnlyForDep :: TestTree
test_metadataOnlyForDep =
  smallTest "metadata-only dep with compiled dependent" \ tp -> do
    (result, events) <- runFresh tp
      [ (UnitName "unit0", UnitMetadata)
      , (UnitName "unit1", UnitAll)
      ]
    assertSuccess "metadata dep" result
    assertHasMetadata "unit0" events
    assertHasMetadata "unit1" events
    assertHasCompiled "unit0" events
    assertHasCompiled "unit1" events

test_pendingPool :: TestTree
test_pendingPool =
  dependentTestGroup "Pending pool and promotion" AllFinish
    [ test_implicitDeps
    , test_pendingThenEnable
    , test_metadataOnlyLeavesTasksPending
    , test_enabledNotDowngraded
    , test_sameUnitMultipleRequestTypes
    , test_metadataOnlyForDep
    ]

-- ---------------------------------------------------------------------------
-- Test group: Multi-batch scheduling
-- ---------------------------------------------------------------------------

test_multiBatch :: TestTree
test_multiBatch =
  largeTest "three batches with overlapping deps" \ tp -> do
    (cb, evRef, _decisions) <- newTestBuild tp
    liftIO $ scheduleBatch cb ScheduleRequest {
      steps = [(UnitName "unit0", UnitAll)],
      recompile = False, rebuild = False
    }
    liftIO $ scheduleBatch cb ScheduleRequest {
      steps = [(UnitName "unit3", UnitAll)],
      recompile = False, rebuild = False
    }
    liftIO $ scheduleBatch cb ScheduleRequest {
      steps = [(UnitName "unit1", UnitAll), (UnitName "unit2", UnitAll)],
      recompile = False, rebuild = False
    }
    result <- liftIO (timedStop cb)
    events <- liftIO (readEvents evRef)
    assertSuccess "multi-batch" result
    ["unit0", "unit1", "unit2", "unit3"] === eventMetadata events

test_redundantBatch :: TestTree
test_redundantBatch =
  smallTest "redundant batch for completed units" \ tp -> do
    (cb, _, _) <- newTestBuild tp
    liftIO $ scheduleBatch cb ScheduleRequest {steps = [], recompile = False, rebuild = False}
    result1 <- liftIO (awaitBuild cb)
    assertSuccess "first batch" result1
    liftIO $ scheduleBatch cb ScheduleRequest {steps = [], recompile = False, rebuild = False}
    result2 <- liftIO (awaitBuild cb)
    assertSuccess "redundant batch" result2
    liftIO (cancel cb.thread)

-- | Regression test for a bug where an identical single-module request submitted twice in a
-- row against a persistent scheduler recompiles the requested module's entire cross-unit
-- dependency chain again on the second batch, even though nothing changed on disk and the
-- metadata step reports zero changed sources.
--
-- Root cause: 'GhcServer.Build.Propagate.propagateCompletion' recomputes and re-adds resolution
-- entries for every module in the request's stale closure on every batch, and
-- 'Test.Scheduler.Concurrent.addResolutions' unconditionally stamps every entry it (re-)inserts
-- with the current generation. If the stale closure is wrongly non-empty for unchanged modules,
-- 'activation''s version comparison (@resolution.computedAt <= completedGeneration@) is
-- defeated: a resolution re-stamped with the new (higher) generation always looks newer than
-- the key's prior completion, so the up-to-date module is reactivated and recompiled again.
-- | Regression test mirroring the instrument UI's project-root and unit-header 'b' (build)
-- actions, which fire one separate 'ScheduleRequest' per module (see
-- 'UI.TaskTree.selectedCompileTargets') without waiting for any of them to complete before
-- submitting the next. Pressing 'b' twice in a row on an unchanged project must not recompile
-- anything the second time.
test_repeatedRootBuildNoRecompile :: TestTree
test_repeatedRootBuildNoRecompile =
  chainTest "repeated root-build (one request per module, fired concurrently) is a no-op" \ tp -> do
    (cb, evRef, _decisions) <- newTestBuild tp
    let
      moduleRequest unit modName = ScheduleRequest {
        steps = [(UnitName unit, UnitModules [ClientModule modName])],
        recompile = False, rebuild = False
      }
      rootBuild :: [ScheduleRequest]
      rootBuild = [
        moduleRequest "unit0" "A0", moduleRequest "unit0" "B0",
        moduleRequest "unit1" "A1", moduleRequest "unit1" "B1",
        moduleRequest "unit2" "A2", moduleRequest "unit2" "B2"
        ]
    liftIO $ mapM_ (scheduleBatch cb) rootBuild
    result1 <- liftIO (awaitBuild cb)
    assertSuccess "first root build" result1
    events1 <- liftIO (readEvents evRef)
    sort (eventCompiled events1) ===
      ["unit0:A0", "unit0:B0", "unit1:A1", "unit1:B1", "unit2:A2", "unit2:B2"]
    liftIO $ mapM_ (scheduleBatch cb) rootBuild
    result2 <- liftIO (timedStop cb)
    assertSuccess "repeated root build" result2
    events2 <- liftIO (drop (length events1) <$> readEvents evRef)
    [] === eventCompiled events2

test_repeatedModuleRequestNoRecompile :: TestTree
test_repeatedModuleRequestNoRecompile =
  chainTest "repeated single-module request against a persistent scheduler is a no-op" \ tp -> do
    (cb, evRef, _decisions) <- newTestBuild tp
    let request = ScheduleRequest {
          steps = [(UnitName "unit0", UnitModules [ClientModule "A0"])],
          recompile = False, rebuild = False
        }
    liftIO $ scheduleBatch cb request
    result1 <- liftIO (awaitBuild cb)
    assertSuccess "first build" result1
    events1 <- liftIO (readEvents evRef)
    -- Sanity check: the first build actually compiles the whole chain (unit2:A2, unit1:A1,
    -- unit0:A0), otherwise the second assertion below would pass vacuously.
    ["unit0:A0", "unit1:A1", "unit2:A2"] === eventCompiled events1
    liftIO $ scheduleBatch cb request
    result2 <- liftIO (timedStop cb)
    assertSuccess "repeated build" result2
    -- 'readEvents' returns the full accumulated log, not just events from the latest batch (see
    -- 'test_stateAccumulation'), so the second batch's own events are the suffix past what the
    -- first batch already produced.
    events2 <- liftIO (drop (length events1) <$> readEvents evRef)
    [] === eventCompiled events2

test_stateAccumulation :: TestTree
test_stateAccumulation =
  smallTest "state accumulates across batches" \ tp -> do
    (cb, evRef, _decisions) <- newTestBuild tp
    liftIO $ scheduleBatch cb ScheduleRequest {
      steps = [(UnitName "unit0", UnitAll)],
      recompile = False, rebuild = False
    }
    result1 <- liftIO (awaitBuild cb)
    assertSuccess "batch 1" result1
    liftIO $ scheduleBatch cb ScheduleRequest {
      steps = [(UnitName "unit1", UnitAll)],
      recompile = False, rebuild = False
    }
    result2 <- liftIO (timedStop cb)
    assertSuccess "batch 2" result2
    -- Events accumulate across batches, but metadata for unit0 should run exactly once
    events <- liftIO (readEvents evRef)
    let metaUnit0Count = length [() | MetadataRan (UnitName "unit0") <- events]
    1 === metaUnit0Count
    assertHasMetadata "unit1" events

test_multiBatchWithCache :: TestTree
test_multiBatchWithCache =
  largeTest "batches with cache in same scheduler" \ tp -> do
    (result1, _) <- runFreshAll tp
    assertSuccess "round 1" result1
    (cb, evRef, _decisions) <- newTestBuild tp
    liftIO $ scheduleBatch cb ScheduleRequest {
      steps = [(UnitName "unit0", UnitAll)],
      recompile = False, rebuild = False
    }
    liftIO $ scheduleBatch cb ScheduleRequest {
      steps = [(UnitName "unit2", UnitAll), (UnitName "unit3", UnitAll)],
      recompile = False, rebuild = False
    }
    result2 <- liftIO (timedStop cb)
    events2 <- liftIO (readEvents evRef)
    assertSuccess "round 2" result2
    [] === eventMetadata events2

test_largeFreshBuild :: TestTree
test_largeFreshBuild =
  largeTest "full fresh build of large project" \ tp -> do
    (result, events) <- runFreshAll tp
    assertSuccess "large fresh" result
    ["unit0", "unit1", "unit2", "unit3"] === eventMetadata events
    ["unit0", "unit1", "unit2", "unit3"] === eventCompiledUnits events
    16 === length (eventCompiled events)

test_multiBatchScheduling :: TestTree
test_multiBatchScheduling =
  dependentTestGroup "Multi-batch scheduling" AllFinish
    [ test_multiBatch
    , test_redundantBatch
    , test_repeatedModuleRequestNoRecompile
    , test_repeatedRootBuildNoRecompile
    , test_stateAccumulation
    , test_multiBatchWithCache
    , test_largeFreshBuild
    ]

-- ---------------------------------------------------------------------------
-- Test group: Home-unit dep regression
-- ---------------------------------------------------------------------------

-- | Test that when @unit1@ is cached (metadata only) and @U1M1@ imports @U1M0@,
-- requesting only @U1M1@ correctly compiles @U1M0@ first.
test_cachedUnitIntraDep :: TestTree
test_cachedUnitIntraDep =
  intraDepTest "cached unit: U1M1 compiles U1M0 as dep" \ tp -> do
    -- Phase 1: build unit0 fully and run metadata for unit1 only.
    (result1, events1) <- runFresh tp
      [ (UnitName "unit0", UnitAll)
      , (UnitName "unit1", UnitMetadata)
      ]
    assertSuccess "phase 1" result1
    assertHasMetadata "unit0" events1
    assertHasMetadata "unit1" events1
    assertHasCompiled "unit0" events1
    assertNoCompiled "unit1" events1

    assertCacheExists tp "unit1"

    -- Phase 2: fresh WorkerState + fresh scheduler.
    -- unit1's digest record was not committed in phase 1 (its stale modules were never
    -- compiled), so metadata re-runs and all unit1 modules are stale.
    (result2, events2) <- runFresh tp
      [(UnitName "unit1", UnitModules [ClientModule "U1M1"])]
    annotate (prettyBuildResult "phase 2" result2)
    assertSuccess "phase 2" result2
    assertHasCompiled "unit1" events2
    assertHasMetadata "unit1" events2

test_homeUnitDep :: TestTree
test_homeUnitDep =
  dependentTestGroup "Home-unit dep regression" AllFinish
    [ test_cachedUnitIntraDep
    ]

-- ---------------------------------------------------------------------------
-- Test group: Build event flows
-- ---------------------------------------------------------------------------

-- | Filter events to only metadata and resolution events (not per-module compile detail).
metaEvents :: [BuildEvent] -> [BuildEvent]
metaEvents =
  filter isMeta
  where
    isMeta = \case
      MetadataSkipped {} -> True
      MetadataRan {} -> True
      ResolutionComputed {} -> True
      _ -> False

assertEventsContain :: HasCallStack => [BuildEvent] -> [BuildEvent] -> TestT IO ()
assertEventsContain expected actual =
  withFrozenCallStack do
    annotate ("Expected events (subset):\n" ++ unlines (map show expected))
    annotate ("Actual events:\n" ++ unlines (map show actual))
    mapM_ (\e -> diff e elem actual) expected

assertNoEvent :: HasCallStack => (BuildEvent -> Bool) -> [BuildEvent] -> TestT IO ()
assertNoEvent predicate actual =
  withFrozenCallStack do
    let matches = filter predicate actual
    annotate ("Unexpected events:\n" ++ unlines (map show matches))
    assert (null matches)

test_eventsFreshBuild :: TestTree
test_eventsFreshBuild =
  smallTest "events: fresh build" \ tp -> do
    (result, events) <- runFreshWithEvents tp []
    assertSuccess "fresh build" result
    let u0 = UnitName "unit0"
        u1 = UnitName "unit1"
        modA = mkModuleName "A"
        modB = mkModuleName "B"
    -- Both units should run metadata fresh
    assertEventsContain [MetadataRan u0, MetadataRan u1] events
    -- Both resolved from cache
    assertEventsContain [ResolutionComputed u0, ResolutionComputed u1] events
    -- No metadata was skipped
    assertNoEvent (\case MetadataSkipped {} -> True; _ -> False) events
    -- Modules were compiled
    assertEventsContain [ModuleCompiled u0 modA, ModuleCompiled u1 modB] events

test_eventsFullCacheRestore :: TestTree
test_eventsFullCacheRestore =
  smallTest "events: full cache restore" \ tp -> do
    -- First build: populate cache
    (result1, _) <- runFresh tp []
    assertSuccess "initial build" result1
    -- Second build: everything from cache
    (result2, events) <- runFreshWithEvents tp []
    assertSuccess "cache restore" result2
    let u0 = UnitName "unit0"
        u1 = UnitName "unit1"
    -- Both units should skip metadata
    assertEventsContain [MetadataSkipped u0, MetadataSkipped u1] events
    -- Both resolved from cache
    assertEventsContain [ResolutionComputed u0, ResolutionComputed u1] events
    -- No fresh metadata
    assertNoEvent (\case MetadataRan {} -> True; _ -> False) events

test_eventsDeleteLeafCache :: TestTree
test_eventsDeleteLeafCache =
  smallTest "events: delete leaf cache" \ tp -> do
    -- First build: populate cache
    (result1, _) <- runFresh tp []
    assertSuccess "initial build" result1
    -- Delete unit0's cache
    deleteUnitCache tp "unit0"
    -- Rebuild: unit0 fresh, unit1 cached
    (result2, events) <- runFreshWithEvents tp []
    assertSuccess "rebuild" result2
    let u0 = UnitName "unit0"
        u1 = UnitName "unit1"
    -- unit0: fresh metadata, unit1: skipped
    assertEventsContain [MetadataRan u0, MetadataSkipped u1] events
    -- Both units resolved
    assertEventsContain [ResolutionComputed u0, ResolutionComputed u1] events

test_eventsMixedCacheFresh :: TestTree
test_eventsMixedCacheFresh =
  smallTest "events: mixed cached and fresh" \ tp -> do
    -- First build
    (result1, _) <- runFresh tp []
    assertSuccess "initial build" result1
    -- Delete unit1's cache, keep unit0
    deleteUnitCache tp "unit1"
    -- Rebuild unit1 explicitly
    (result2, events) <- runFreshWithEvents tp [(UnitName "unit1", UnitAll)]
    assertSuccess "rebuild" result2
    let u0 = UnitName "unit0"
        u1 = UnitName "unit1"
    -- unit0 is an implicit dep, still cached -> skip
    assertEventsContain [MetadataSkipped u0] events
    -- unit1 is fresh
    assertEventsContain [MetadataRan u1] events
    -- Both units resolved
    assertEventsContain [ResolutionComputed u1, ResolutionComputed u0] events

test_eventsMetadataOnly :: TestTree
test_eventsMetadataOnly =
  smallTest "events: metadata only" \ tp -> do
    (result, events) <- runFreshWithEvents tp [(UnitName "unit0", UnitMetadata)]
    assertSuccess "metadata only" result
    let u0 = UnitName "unit0"
    -- Metadata ran
    assertEventsContain [MetadataRan u0] events
    assertEventsContain [ResolutionComputed u0] events
    -- No modules compiled
    assertNoEvent (\case ModuleCompiled {} -> True; _ -> False) events
    -- No unit1 activity
    assertNoEvent (\case MetadataRan (UnitName "unit1") -> True; MetadataSkipped (UnitName "unit1") -> True; _ -> False) events

test_eventFlow :: TestTree
test_eventFlow =
  dependentTestGroup "Build event flows" AllFinish
    [ test_eventsFreshBuild
    , test_eventsFullCacheRestore
    , test_eventsDeleteLeafCache
    , test_eventsMixedCacheFresh
    , test_eventsMetadataOnly
    ]

-- ---------------------------------------------------------------------------
-- Test group: Implicit dep compile skip
-- ---------------------------------------------------------------------------

-- | Both units fully cached and unchanged. Request @unit1:modules@ with @--recompile@.
-- unit0 is an implicit dep with unchanged sources: it is not stale, so no compile task is
-- created for it at all. unit1 is explicitly requested with force, so its modules recompile.
test_implicitDepCachedSkip :: TestTree
test_implicitDepCachedSkip =
  smallTest "implicit dep: unchanged modules not recompiled" \ tp -> do
    -- Phase 1: full build to populate all caches
    (result1, _) <- runFreshAll tp
    assertSuccess "initial build" result1
    assertCacheExists tp "unit0"
    assertCacheExists tp "unit1"
    -- Phase 2: fresh WorkerState, request unit1:modules with force
    (result2, events2) <- runFreshWithEvents' True False tp [(UnitName "unit1", UnitModulesOnly)]
    assertSuccess "cached skip" result2
    let u0 = UnitName "unit0"
        u1 = UnitName "unit1"
    -- unit0 metadata skipped (unchanged)
    assertEventsContain [MetadataSkipped u0] events2
    -- unit1 metadata skipped (unchanged, --recompile does not force metadata)
    assertEventsContain [MetadataSkipped u1] events2
    -- unit0's modules are not stale: no compile tasks exist for them
    assertNoCompiled "unit0" events2
    -- unit1's modules recompiled because it's explicitly requested with force
    assertEventsContain [ModuleCompiled u1 (mkModuleName "B")] events2
    assertHasCompiled "unit1" events2

-- | Both units have metadata cached, but the compiled artifacts are deleted behind the
-- server's back.  Digest-based invalidation cannot see this (sources are unchanged), so
-- recovery requires @--rebuild@, which invalidates everything.
test_implicitDepNoCacheCompiled :: TestTree
test_implicitDepNoCacheCompiled =
  smallTest "deleted artifacts recovered via rebuild" \ tp -> do
    -- Phase 1: full build to populate all caches
    (result1, _) <- runFreshAll tp
    assertSuccess "initial build" result1
    assertCacheExists tp "unit0"
    assertCacheExists tp "unit1"
    -- Delete unit0's interface files but keep metadata cache
    deleteModuleHiFiles tp "unit0"
    -- Phase 2: fresh WorkerState, full rebuild
    (result2, events2) <- runFreshWithEvents' False True tp []
    assertSuccess "rebuild" result2
    let u0 = UnitName "unit0"
        u1 = UnitName "unit1"
    -- Everything re-runs: metadata and all modules
    assertEventsContain [MetadataRan u0, MetadataRan u1] events2
    assertEventsContain [ModuleCompiled u0 (mkModuleName "A")] events2
    assertEventsContain [ModuleCompiled u1 (mkModuleName "B")] events2

test_implicitDepCompileSkip :: TestTree
test_implicitDepCompileSkip =
  dependentTestGroup "Implicit dep compile skip" AllFinish
    [ test_implicitDepCachedSkip
    , test_implicitDepNoCacheCompiled
    ]

-- ---------------------------------------------------------------------------
-- Test group: HPT assembly
-- ---------------------------------------------------------------------------

-- | Cache restore with a forced recompile of the dependent unit:
-- unit0:A is unchanged (restored from cache, not recompiled), unit1:B is forced stale.
-- If the HPT is correctly assembled, unit0:A should be present in unit0's HPT
-- because 'loadCachedDeps' (or 'loadHomeUnit') loaded it before compiling B.
test_hptCacheRestore :: TestTree
test_hptCacheRestore =
  smallTest "HPT: cache restore populates cross-unit deps" \ tp -> do
    -- Phase 1: full build to populate cache and CachedDeps
    (result1, _) <- runFreshAll tp
    assertSuccess "initial build" result1
    -- Phase 2: fresh WorkerState, force unit1's modules stale
    (result2, events, stateVar) <- runFreshWithState' True False tp [(UnitName "unit1", UnitAll)]
    assertSuccess "cache restore" result2
    -- Both metadata skipped (unchanged)
    assertEventsContain [MetadataSkipped (UnitName "unit0"), MetadataSkipped (UnitName "unit1")] events
    -- unit1:B was compiled; unit0:A should be in unit0's HPT so that unit1:B compilation
    -- can find it via hugSomeThingsBelowUs
    assertHptHasModule stateVar "unit0" "A"
    assertHptHasModule stateVar "unit1" "B"

-- | After deleting the leaf unit's @.dyn_hi@ files, a @--rebuild@ restores the HPT
-- because every module is recompiled.
test_hptCacheRestoreNoCachedDeps :: TestTree
test_hptCacheRestoreNoCachedDeps =
  smallTest "HPT: cache restore without interface files" \ tp -> do
    -- Phase 1: full build
    (result1, _) <- runFreshAll tp
    assertSuccess "initial build" result1
    -- Delete unit0's interface files but keep metadata cache
    deleteModuleHiFiles tp "unit0"
    -- Phase 2: fresh WorkerState, full rebuild (digests cannot detect deleted outputs)
    (result2, _events, stateVar) <- runFreshWithState' False True tp []
    assertSuccess "cache restore" result2
    -- unit0:A should still be in the HPT (it was recompiled)
    assertHptHasModule stateVar "unit0" "A"
    assertHptHasModule stateVar "unit1" "B"

-- | Build only @unit0@ in session 1, then @unit1@ in session 2 (fresh 'WorkerState').
-- @unit1:B@ imports @unit0:A@, so the compilation of @B@ needs @A@'s interface in the HPT.
-- Since @unit0@ was fully built in session 1 and is unchanged, it is not stale and no
-- compile task exists for it.  @B@ still compiles because 'CachedDeps' are assembled
-- from the module map (populated from @cached_unit.json@).
test_hptCrossSessionCachedDeps :: TestTree
test_hptCrossSessionCachedDeps =
  smallTest "HPT: cross-session implicit dep skip" \ tp -> do
    -- Session 1: build unit0 fully
    (result1, events1) <- runFreshWithEvents tp [(UnitName "unit0", UnitAll)]
    assertSuccess "session 1" result1
    assertHasMetadata "unit0" events1
    assertHasCompiled "unit0" events1
    assertNoMetadata "unit1" events1
    assertNoCompiled "unit1" events1
    assertCacheExists tp "unit0"
    -- Session 2: fresh WorkerState, build unit1
    (result2, events2) <- runFreshWithEvents tp [(UnitName "unit1", UnitAll)]
    assertSuccess "session 2" result2
    assertHasMetadata "unit1" events2
    assertHasCompiled "unit1" events2
    let u1 = UnitName "unit1"
        modB = mkModuleName "B"
    -- unit0 is an implicit dep with cached artifacts from session 1 and unchanged sources:
    -- it is not stale, so no compile task exists for it.
    assertNoCompiled "unit0" events2
    -- B imports A from unit0 — compilation succeeds because CachedDeps are
    -- assembled from the module map (populated from cached_unit.json).
    assertEventsContain [ModuleCompiled u1 modB] events2

test_hptAssembly :: TestTree
test_hptAssembly =
  dependentTestGroup "HPT assembly" AllFinish
    [ test_hptCacheRestore
    , test_hptCacheRestoreNoCachedDeps
    , test_hptCrossSessionCachedDeps
    ]

-- ---------------------------------------------------------------------------
-- Test group: Transitive dep cache restore
-- ---------------------------------------------------------------------------

-- | Delete the leaf unit's cache in a 3-unit chain (unit0 -> unit1 -> unit2).
-- After a full build, delete unit0's cache and output but keep unit1 and unit2.
-- Rebuild unit0: unit1 and unit2 should be restored from cache.
test_cacheTransitiveChain :: TestTree
test_cacheTransitiveChain =
  chainTest "transitive dep cache: 3-unit chain" \ tp -> do
    -- Phase 1: full build
    (result1, events1) <- runFreshAll tp
    assertSuccess "initial build" result1
    assertCacheExists tp "unit0"
    assertCacheExists tp "unit1"
    assertCacheExists tp "unit2"
    -- Verify initial build compiled everything
    assertHasMetadata "unit0" events1
    assertHasMetadata "unit1" events1
    assertHasMetadata "unit2" events1
    assertHasCompiled "unit0" events1
    assertHasCompiled "unit1" events1
    assertHasCompiled "unit2" events1
    -- Delete unit0's cache and output
    deleteUnitCache tp "unit0"
    liftIO $ removePathForcibly (tp.root ++ "/output/unit0")
    liftIO $ createDirectoryIfMissing True (tp.root ++ "/output/unit0")
    -- Phase 2: rebuild unit0
    (result2, events2) <- runFreshWithEvents tp [(UnitName "unit0", UnitAll)]
    annotate (prettyBuildResult "rebuild unit0" result2)
    assertSuccess "rebuild unit0" result2
    assertHasMetadata "unit0" events2
    assertHasCompiled "unit0" events2
    -- unit1 and unit2 should be restored from cache
    assertNoMetadata "unit1" events2
    assertNoMetadata "unit2" events2

test_transitiveDepRestore :: TestTree
test_transitiveDepRestore =
  dependentTestGroup "Transitive dep cache restore" AllFinish
    [ test_cacheTransitiveChain
    , test_cacheTransitiveMultipleRoots
    ]

-- | Delete two root units' cache and output in the 4-unit project.
-- unit3 depends on unit1 and unit2, which depend on unit0.
-- After a full build, delete unit3's cache and output, rebuild unit3.
-- unit0, unit1, and unit2 (transitive deps) should be restored from cache.
test_cacheTransitiveMultipleRoots :: TestTree
test_cacheTransitiveMultipleRoots =
  largeTest "transitive dep cache: 4-unit project" \ tp -> do
    -- Phase 1: full build
    (result1, _) <- runFreshAll tp
    assertSuccess "initial build" result1
    assertCacheExists tp "unit0"
    assertCacheExists tp "unit1"
    assertCacheExists tp "unit2"
    assertCacheExists tp "unit3"
    -- Delete unit3's cache and output
    deleteUnitCache tp "unit3"
    liftIO $ removePathForcibly (tp.root ++ "/output/unit3")
    liftIO $ createDirectoryIfMissing True (tp.root ++ "/output/unit3")
    -- Phase 2: rebuild unit3
    (result2, events2) <- runFreshWithEvents tp [(UnitName "unit3", UnitAll)]
    annotate (prettyBuildResult "rebuild unit3" result2)
    assertSuccess "rebuild unit3" result2
    assertHasMetadata "unit3" events2
    assertHasCompiled "unit3" events2
    -- all transitive deps should be restored from cache
    assertNoMetadata "unit0" events2
    assertNoMetadata "unit1" events2
    assertNoMetadata "unit2" events2

-- ---------------------------------------------------------------------------
-- Top-level test tree
-- ---------------------------------------------------------------------------

-- ---------------------------------------------------------------------------
-- Test group: Execute module (@x@ key / 'GhcServer.Build.Execute.executeModule')
-- ---------------------------------------------------------------------------

-- | Project with a single unit whose module exports @main@, used to regression-test
-- 'GhcServer.Build.Execute.executeModule' -- specifically, that it recompiles the module
-- to bytecode before running @main@, rather than relying on a stale object-code HPT entry
-- from a prior plain compile (which fails with \"Cannot add module ... to context: not interpreted\").
createExecProject :: FilePath -> IO ()
createExecProject root = do
  createDirectoryIfMissing True (root ++ "/unit0")
  writeUnitConfig root "unit0" UnitConfig {deps = [], args = baseGhcArgs}
  writeProjectFile root "unit0/Main.hs" $ unlines
    [ "module Main where"
    , ""
    , "main :: IO ()"
    , "main = putStrLn \"hello from execute test\""
    ]

execProjectTest :: TestName -> (TestProject -> TestT IO ()) -> TestTree
execProjectTest =
  projectTest "ghc-server-exec" createExecProject

-- | Regression test for the \"not interpreted\" execute failure: compile the module as
-- object code (mirroring a normal build / the UI's @b@ key), then execute it (mirroring
-- the UI's @x@ key) and assert it actually runs @main@ successfully rather than failing.
test_executeAfterCompile :: TestTree
test_executeAfterCompile =
  execProjectTest "execute module after plain compile succeeds" \ tp -> do
    (result, _) <- runFreshAll tp
    assertSuccess "compile" result
    stateVar <- liftIO newBuildState
    (buildEnv, _) <- liftIO (newBuildEnv tp stateVar)
    let name = UnitName "unit0"
    mresult <- liftIO (executeModuleTask buildEnv emptyBuildExt name (mkModuleName "Main"))
    annotate ("executeModuleTask result: " ++ show mresult)
    case mresult of
      Just (TaskSuccess Nothing) -> pure ()
      other -> fail ("Expected Just (TaskSuccess Nothing), got " ++ show other)

-- | Project with a single unit whose module's @main@ has type @IO String@, used to regression-test
-- 'Internal.Evaluate.classifyMainResultType'\'s 'ResultString' branch and 'GhcServer.Build.Execute.executeModule'\'s
-- propagation of the returned string as a build-task result.
createExecStringProject :: FilePath -> IO ()
createExecStringProject root = do
  createDirectoryIfMissing True (root ++ "/unit0")
  writeUnitConfig root "unit0" UnitConfig {deps = [], args = baseGhcArgs}
  writeProjectFile root "unit0/Main.hs" $ unlines
    [ "module Main where"
    , ""
    , "main :: IO String"
    , "main = pure \"hello from IO String main\""
    ]

execStringProjectTest :: TestName -> (TestProject -> TestT IO ()) -> TestTree
execStringProjectTest =
  projectTest "ghc-server-exec-string" createExecStringProject

-- | Exercises 'Internal.Evaluate.classifyMainResultType'\'s 'ResultString' branch end-to-end: a module whose
-- @main :: IO String@ is compiled then executed, and its return value must be exfiltrated verbatim as the
-- companion 'String' returned by 'executeModule' (rather than merely succeeding, which the ordinary
-- @main :: IO ()@ case would also do).
test_executeStringMain :: TestTree
test_executeStringMain =
  execStringProjectTest "execute module with main :: IO String succeeds" \ tp -> do
    (result, _) <- runFreshAll tp
    assertSuccess "compile" result
    stateVar <- liftIO newBuildState
    (buildEnv, _) <- liftIO (newBuildEnv tp stateVar)
    let name = UnitName "unit0"
    mresult <- liftIO (executeModuleTask buildEnv emptyBuildExt name (mkModuleName "Main"))
    annotate ("executeModuleTask result: " ++ show mresult)
    case mresult of
      Just (TaskSuccess (Just "hello from IO String main")) -> pure ()
      other -> fail ("Expected Just (TaskSuccess (Just \"hello from IO String main\")), got " ++ show other)

test_executeModule :: TestTree
test_executeModule =
  dependentTestGroup "Execute module" AllFinish
    [ test_executeAfterCompile
    , test_executeStringMain
    , test_executeNonexistentModuleFails
    ]

-- | Regression test for the previously-conflated "no main" \/ "session setup failed" outcomes of
-- 'executeModuleTask': targeting a module that does not exist in the unit at all must surface as 'TaskFailed',
-- not silently as a skip ('Nothing'), which is reserved for the genuinely no-@main@ case.
test_executeNonexistentModuleFails :: TestTree
test_executeNonexistentModuleFails =
  execProjectTest "execute nonexistent module surfaces a failure, not a silent skip" \ tp -> do
    (result, _) <- runFreshAll tp
    assertSuccess "compile" result
    stateVar <- liftIO newBuildState
    (buildEnv, _) <- liftIO (newBuildEnv tp stateVar)
    let name = UnitName "unit0"
    mresult <- liftIO (executeModuleTask buildEnv emptyBuildExt name (mkModuleName "DoesNotExist"))
    annotate ("executeModuleTask result: " ++ show mresult)
    case mresult of
      Just (TaskFailed _) -> pure ()
      other -> fail ("Expected Just (TaskFailed _), got " ++ show other)

-- ---------------------------------------------------------------------------
-- Test group: Incremental recompilation
-- ---------------------------------------------------------------------------

-- | Project for partial-invalidation tests:
--
-- @
-- unit0: A, B (imports A), C (standalone)
-- unit1 (deps unit0): D (imports B), E (standalone)
-- @
createIncrementalProject :: FilePath -> IO ()
createIncrementalProject root = do
  createDirectoryIfMissing True (root ++ "/unit0")
  createDirectoryIfMissing True (root ++ "/unit1")
  writeUnitConfig root "unit0" UnitConfig {deps = [], args = baseGhcArgs}
  writeProjectFile root "unit0/A.hs" $ unlines
    [ "module A where"
    , "a :: String"
    , "a = \"a\""
    ]
  writeProjectFile root "unit0/B.hs" $ unlines
    [ "module B where"
    , "import A (a)"
    , "b :: String"
    , "b = a ++ \"_b\""
    ]
  writeProjectFile root "unit0/C.hs" $ unlines
    [ "module C where"
    , "c :: String"
    , "c = \"c\""
    ]
  writeUnitConfig root "unit1" UnitConfig {deps = ["unit0"], args = baseGhcArgs}
  writeProjectFile root "unit1/D.hs" $ unlines
    [ "module D where"
    , "import B (b)"
    , "d :: String"
    , "d = b ++ \"_d\""
    ]
  writeProjectFile root "unit1/E.hs" $ unlines
    [ "module E where"
    , "e :: String"
    , "e = \"e\""
    ]

incrementalTest :: TestName -> (TestProject -> TestT IO ()) -> TestTree
incrementalTest =
  projectTest "ghc-server-incremental" createIncrementalProject

-- | Editing one module recompiles exactly its downstream closure, across unit boundaries:
-- @B@ (edited) and @D@ (imports @B@, in a different unit), but not @A@ (dependency of @B@),
-- @C@ or @E@ (unrelated).
test_partialInvalidation :: TestTree
test_partialInvalidation =
  incrementalTest "editing a module recompiles only its downstream closure" \ tp -> do
    -- Session 1: full build, commits digest records.
    (result1, events1) <- runFreshAll tp
    assertSuccess "initial build" result1
    ["unit0:A", "unit0:B", "unit0:C", "unit1:D", "unit1:E"] === eventCompiled events1
    -- Edit B without changing its interface to the module graph.
    liftIO $ writeProjectFile tp.root "unit0/B.hs" $ unlines
      [ "module B where"
      , "import A (a)"
      , "b :: String"
      , "b = a ++ \"_b_edited\""
      ]
    -- Session 2: fresh scheduler and WorkerState over the same on-disk state.
    (result2, events2) <- runFreshAll tp
    assertSuccess "incremental build" result2
    -- unit0 has a changed source: metadata re-runs (incrementally).
    assertEventsContain [MetadataRan (UnitName "unit0")] events2
    -- unit1 is unchanged: metadata skipped.
    assertEventsContain [MetadataSkipped (UnitName "unit1")] events2
    -- Exactly the downstream closure of B is recompiled.
    ["unit0:B", "unit1:D"] === eventCompiled events2

-- | A touch without a content change (same digest, new mtime) invalidates nothing.
test_touchWithoutChange :: TestTree
test_touchWithoutChange =
  incrementalTest "touching a file without changing content recompiles nothing" \ tp -> do
    (result1, _) <- runFreshAll tp
    assertSuccess "initial build" result1
    content <- liftIO (readFile (tp.root ++ "/unit0/B.hs"))
    liftIO (length content `seq` writeProjectFile tp.root "unit0/B.hs" content)
    (result2, events2) <- runFreshAll tp
    assertSuccess "touch build" result2
    [] === eventMetadata events2
    [] === eventCompiled events2

test_incrementalRecompilation :: TestTree
test_incrementalRecompilation =
  dependentTestGroup "Incremental recompilation" AllFinish
    [ test_partialInvalidation
    , test_touchWithoutChange
    ]

-- ---------------------------------------------------------------------------
-- Test group: Persistent scheduler sessions
-- ---------------------------------------------------------------------------

-- | Schedule a full build of both units and wait for it, returning the events and scheduler
-- decisions recorded by that batch alone.
persistentBatch :: Build -> BuildEvents -> TestT IO (BuildResult, [BuildEvent])
persistentBatch cb evRef = do
  before <- liftIO (readEvents evRef)
  liftIO $ scheduleBatch cb ScheduleRequest {
    steps = [(UnitName "unit0", UnitAll), (UnitName "unit1", UnitAll)],
    recompile = False, rebuild = False
  }
  result <- liftIO (awaitBuild cb)
  after <- liftIO (readEvents evRef)
  pure (result, drop (length before) after)

-- | Repeating an identical request against a scheduler that survives the first one must compile
-- nothing the second time.
--
-- This is the redundant-rebuild regression in its purest form.  Asserting only on the absence of
-- compile events would be satisfiable by accident (e.g. by the second request never producing
-- resolutions at all), so the decision log is checked as well: every module must be explicitly
-- classified as up to date, which can only happen if resolutions /were/ computed and then
-- compared against the recorded completions.
test_persistentRepeatedRequest :: TestTree
test_persistentRepeatedRequest =
  incrementalTest "an identical repeated request on a live scheduler compiles nothing" \ tp -> do
    (cb, evRef, _decisions) <- newTestBuild tp
    (result1, events1) <- persistentBatch cb evRef
    assertSuccess "batch 1" result1
    allModules === eventCompiled events1
    (result2, events2) <- persistentBatch cb evRef
    assertSuccess "batch 2" result2
    [] === eventCompiled events2
    decisions <- liftIO (buildDecisions _decisions)
    let batch2 = decisionBatch 2 decisions
    allModules === decisionSkipped batch2
    [] === decisionActivated batch2
    liftIO (cancel cb.thread)
  where
    allModules = ["unit0:A", "unit0:B", "unit0:C", "unit1:D", "unit1:E"]

-- | The complement of 'test_persistentRepeatedRequest': after an edit, the same live scheduler
-- must activate exactly the downstream closure of the edited module and leave the rest alone.
--
-- 'test_partialInvalidation' covers the same scenario across two /fresh/ schedulers, where the
-- completion bookkeeping starts empty; here the bookkeeping from batch 1 is still present and
-- has to be overridden selectively.
test_persistentEditBetweenRequests :: TestTree
test_persistentEditBetweenRequests =
  incrementalTest "an edit between requests on a live scheduler recompiles only the closure" \ tp -> do
    (cb, evRef, _decisions) <- newTestBuild tp
    (result1, _) <- persistentBatch cb evRef
    assertSuccess "batch 1" result1
    liftIO $ writeProjectFile tp.root "unit0/B.hs" $ unlines
      [ "module B where"
      , "import A (a)"
      , "b :: String"
      , "b = a ++ \"_b_edited\""
      ]
    (result2, events2) <- persistentBatch cb evRef
    assertSuccess "batch 2" result2
    decisions <- liftIO (buildDecisions _decisions)
    let batch2 = decisionBatch 2 decisions
    ["unit0:B", "unit1:D"] === decisionActivated batch2
    ["unit0:A", "unit0:C", "unit1:E"] === decisionSkipped batch2
    ["unit0:B", "unit1:D"] === eventCompiled events2
    liftIO (cancel cb.thread)

-- | Two requests submitted back-to-back, with the second overlapping only part of the first's
-- target set, must not lose or duplicate any target from either request's union.  Since
-- 'scheduleBatch' only enqueues (it doesn't await completion), the second request is classified
-- while the first request's compile tasks are almost certainly still executing -- this exercises
-- the 'accepted'-based in-flight dedup against a genuinely concurrent scheduler, as opposed to
-- the sequential-batch tests above, where each 'persistentBatch' call awaits full completion
-- before the next request is submitted.
test_concurrentOverlappingRequests :: TestTree
test_concurrentOverlappingRequests =
  incrementalTest "two overlapping in-flight requests do not drop or duplicate a rebuild" \ tp -> do
    (cb, evRef, _decisions) <- newTestBuild tp
    liftIO $ scheduleBatch cb ScheduleRequest {
      steps =
        [ (UnitName "unit0", UnitModules [ClientModule "A", ClientModule "B"])
        , (UnitName "unit1", UnitModules [ClientModule "D"])
        ],
      recompile = False, rebuild = False
    }
    -- Submitted immediately, without awaiting the request above. Overlaps on unit0:B, and
    -- additionally targets unit0:C and unit1:E, which the first request did not.
    liftIO $ scheduleBatch cb ScheduleRequest {
      steps =
        [ (UnitName "unit0", UnitModules [ClientModule "B", ClientModule "C"])
        , (UnitName "unit1", UnitModules [ClientModule "E"])
        ],
      recompile = False, rebuild = False
    }
    result <- liftIO (timedStop cb)
    events <- liftIO (readEvents evRef)
    assertSuccess "concurrent overlapping requests" result
    -- The union of both targets is compiled exactly once each: a dropped module would be missing
    -- from this list, and a double-dispatched one would appear twice, breaking the equality either
    -- way.
    ["unit0:A", "unit0:B", "unit0:C", "unit1:D", "unit1:E"] === eventCompiled events
    decisions <- liftIO (buildDecisions _decisions)
    -- unit0:B, requested by both batches, must have been activated exactly once: whichever
    -- request's classification observes the other as already accepted/completed must dedupe or
    -- skip, never both independently activate it.
    let activatedB =
          [() | DecisionActivated (ResolvedModule (UnitName "unit0") modName) _ <- decisions, moduleNameString modName == "B"]
    1 === length activatedB
    liftIO (cancel cb.thread)

-- | 'GhcServer.Build.Diff.commitDigests' commits the digest of every module that compiled
-- successfully in a batch, even when other modules in the same request failed: a compiler error
-- is a normal outcome of a build request, not a reason to withhold bookkeeping for the rest of
-- the unit. Only the source file of the module that actually failed keeps its prior (or absent)
-- digest, so it alone is re-detected as "changed" and retried; modules that compiled
-- successfully, and anything that only depends on them, are correctly treated as up to date on
-- the next identical request.
test_failedTaskNotRetried :: TestTree
test_failedTaskNotRetried =
  incrementalTest "a request repeated after a compile failure only retries the failed module" \ tp -> do
    -- Break C (a standalone module: no imports, nothing depends on it) so its compile task
    -- fails without changing anything else's source.
    liftIO $ writeProjectFile tp.root "unit0/C.hs" $ unlines
      [ "module C where"
      , "c :: String"
      , "c = (((("
      ]
    (cb, evRef, _decisions) <- newTestBuild tp
    (result1, events1) <- persistentBatch cb evRef
    assert (not result1.success)
    1 === length result1.compileErrors
    -- 'ModuleCompiled' is logged unconditionally at dispatch time (see 'GhcServer.Build.Propagate.compile'),
    -- before the actual GHC invocation, so it also appears here for the module that then fails.
    ["unit0:A", "unit0:B", "unit0:C", "unit1:D", "unit1:E"] === eventCompiled events1
    (result2, events2) <- persistentBatch cb evRef
    -- The identical repeated request is not a full no-op, but it is no longer redundant either:
    -- unit0:A and unit0:B compiled successfully, so their digests were committed and they are
    -- correctly seen as up to date. unit1:D (which imports unit0:B) is likewise left alone, since
    -- its dependency did not actually change. Only unit0:C, whose digest was withheld because it
    -- failed, is redispatched. The build still reports the same failure.
    assert (not result2.success)
    result1.compileErrors === result2.compileErrors
    ["unit0:C"] === eventCompiled events2
    decisions <- liftIO (buildDecisions _decisions)
    let batch2 = decisionBatch 2 decisions
    diff "unit0:C" elem (decisionActivated batch2)
    diff "unit0:A" elem (decisionSkipped batch2)
    diff "unit0:B" elem (decisionSkipped batch2)
    diff "unit1:D" elem (decisionSkipped batch2)
    diff "unit1:E" elem (decisionSkipped batch2)
    liftIO (cancel cb.thread)

test_persistentSessions :: TestTree
test_persistentSessions =
  dependentTestGroup "Persistent scheduler sessions" AllFinish
    [ test_persistentRepeatedRequest
    , test_persistentEditBetweenRequests
    , test_concurrentOverlappingRequests
    , test_failedTaskNotRetried
    ]

test_serverBuild :: TestTree
test_serverBuild =
  dependentTestGroup "GhcServer.Build" AllFinish
    [ test_basicDispatch
    , test_cacheRestore
    , test_pendingPool
    , test_multiBatchScheduling
    , test_homeUnitDep
    , test_eventFlow
    , test_implicitDepCompileSkip
    , test_hptAssembly
    , test_transitiveDepRestore
    , test_incrementalRecompilation
    , test_persistentSessions
    , test_executeModule
    ]

