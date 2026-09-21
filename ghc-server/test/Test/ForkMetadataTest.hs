{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedLists #-}
-- | Feasibility experiment: run the metadata (build plan) step for two units with an inter-unit import dependency in
-- two separate forked processes, using a shared @mmap@ region to hand the serialized module graph back to the
-- parent. Everything else (the GHC session, 'WorkerState', logger, ...) is recreated independently in each process
-- rather than shared, following the "message-passing instead of shared live state" conclusion from
-- 'Test.ForkTest'/@kb-process-isolation@.
--
-- Unit @unit2@ depends on @unit1@ via a package DB written by the parent *before* forking (as in
-- @test_buildPlan_oneshot@); this only requires a config file plus @ghc-pkg recache@, not a running GHC session, so
-- both children can run truly in parallel rather than being sequenced.
--
-- __Result: this hangs.__ 'forkMetadataIO' reproducibly deadlocks (confirmed twice, not a transient issue) when run
-- under ghcid, even with 'System.Posix.Process.forkProcess' restricted to a single capability as in 'Test.ForkTest'.
-- Unlike that module's trivial @n * n@ workload, the child here runs a real GHC session/downsweep, which touches far
-- more RTS-managed global state (timer manager, IO managers, GHC's own top-level 'unsafePerformIO' \'IORef\'s,
-- locale/encoding state, etc.) than a bare computation does. 'setNumCapabilities 1' only limits how many Haskell
-- execution contexts run after the call; it does not stop already-running OS threads (e.g. the RTS's per-capability
-- IO managers, spawned before the test body even executes) from holding a lock at the moment of @fork()@, which is
-- exactly the failure mode documented for 'forkProcess' under a threaded RTS. This confirms, with a slightly more
-- realistic workload, the assessment already recorded in @kb-process-isolation@: forking a live GHC session is not a
-- viable IPC/parallelism strategy for this codebase. The test is therefore deliberately left unwired from
-- @test/Main.hs@ (it would hang the whole suite) but kept as documented, reproducible evidence.
module Test.ForkMetadataTest where

import Control.Exception (SomeException, bracket, onException, try)
import Control.Monad (forM, forM_)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (eitherDecodeStrict, encode)
import qualified Data.ByteString as ByteString
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.ByteString.Unsafe as ByteString.Unsafe
import Data.Foldable (toList)
import Data.Int (Int64)
import qualified Data.Set as Set
import Foreign.C.Types (CInt (..), CSize (..))
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (Ptr, castPtr, nullPtr, plusPtr)
import Foreign.Storable (peek, poke)
import GHC (DynFlags (..), Ghc, GhcMode (..), Target, getSession)
import GHC.Conc (getNumCapabilities, setNumCapabilities)
import GHC.Unit (UnitId, stringToUnitId)
import Hedgehog (annotate, (===))
import Internal.BuildPlan (buildPlanForTargets)
import Internal.DynFlags (modifyActiveUnitFlags)
import Internal.Log (newLogger)
import Internal.Session (simpleSessionWithDebugLog)
import Internal.State (newState)
import Prelude hiding (log)
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath ((</>))
import System.Posix.Process (ProcessStatus (..), forkProcess, getProcessStatus)
import System.Posix.Signals (sigKILL, signalProcess)
import System.Posix.Types (ProcessID)
import Test.PackageDb (UnitSpec (..), createEmptyHomeUnitDb, moduleSpec)
import Test.Run (unitTest, withTemp)
import Test.Target (fileUnitTargets, ghcOptions, pureUnitTargets)
import Test.Tasty (TestName, TestTree)
import Types.Args (Args (..), buildPlanAll, emptyArgs)
import Types.BuildPlan (BuildPlan (..), BuildPlanJson (..), BuildPlanSchema (..))
import Types.Log (newLog)
import Types.Settings (defaultSettings)

-- ---------------------------------------------------------------------------
-- Raw @mmap@ FFI (byte-oriented; see 'Test.ForkTest' for the @Int64@-slot variant)
-- ---------------------------------------------------------------------------

foreign import ccall unsafe "mmap"
  c_mmap :: Ptr () -> CSize -> CInt -> CInt -> CInt -> CSize -> IO (Ptr ())

foreign import ccall unsafe "munmap"
  c_munmap :: Ptr () -> CSize -> IO CInt

protReadWrite :: CInt
protReadWrite = 0x1 + 0x2

mapSharedAnon :: CInt
mapSharedAnon = 0x01 + 0x20

-- | Bytes reserved per unit for its serialized build plan schema (JSON is small here, but leave headroom).
slotBytes :: Int
slotBytes = 65536

regionBytes :: CSize
regionBytes = fromIntegral (2 * slotBytes)

mmapRegion :: IO (Ptr ())
mmapRegion = c_mmap nullPtr regionBytes protReadWrite mapSharedAnon (-1) 0

munmapRegion :: Ptr () -> IO ()
munmapRegion ptr = () <$ c_munmap ptr regionBytes

-- | Write a length-prefixed (8-byte 'Int64') payload into the given slot.
writeSlot :: Ptr () -> Int -> ByteString -> IO ()
writeSlot base index payload = do
  let slotPtr = base `plusPtr` (index * slotBytes)
  poke (castPtr slotPtr :: Ptr Int64) (fromIntegral (ByteString.length payload))
  ByteString.Unsafe.unsafeUseAsCStringLen payload \(src, len) ->
    copyBytes (slotPtr `plusPtr` 8) (castPtr src) len

readSlot :: Ptr () -> Int -> IO ByteString
readSlot base index = do
  let slotPtr = base `plusPtr` (index * slotBytes)
  len <- peek (castPtr slotPtr :: Ptr Int64)
  ByteString.packCStringLen (castPtr (slotPtr `plusPtr` 8), fromIntegral len)

-- ---------------------------------------------------------------------------
-- Unit fixtures: two units, one module each, unit2 imports unit1's module.
-- ---------------------------------------------------------------------------

unit1 :: UnitId
unit1 = stringToUnitId "unit1"

unit2 :: UnitId
unit2 = stringToUnitId "unit2"

unit1Spec :: UnitSpec
unit1Spec =
  UnitSpec {name = "unit1", deps = [], modules = [moduleSpec "M1" ["module M1 where", "m1 :: Int", "m1 = 1"]]}

unit2Spec :: UnitSpec
unit2Spec =
  UnitSpec {
    name = "unit2",
    deps = ["unit1"],
    modules = [moduleSpec "M2" ["module M2 where", "import M1", "m2 :: Int", "m2 = m1 + 1"]]
  }

-- | Run the metadata (build plan) step for a set of targets, mirroring 'BuildPlanTest.Test1.runBuildPlan', but
-- returning only the JSON schema (the part that's actually serializable/comparable) since 'HscEnv'/'ModuleGraph'
-- can't cross a process boundary anyway.
runBuildPlan :: [Target] -> Ghc BuildPlanSchema
runBuildPlan targets = do
  modifyActiveUnitFlags \d -> d {ghcMode = MkDepend}
  log <- liftIO $ newLog Nothing
  plan <- buildPlanForTargets (newLogger log) (Set.fromList (toList buildPlanAll)) mempty [] targets
  _ <- getSession
  pure plan.json.schema

-- | Run one unit's metadata step in a fresh, unshared 'WorkerState'/session (oneshot-style: package DBs on disk
-- instead of an in-memory HUG) and write the JSON-encoded result into its shared-memory slot.
runUnitChild :: Ptr () -> Int -> [String] -> [Target] -> IO ()
runUnitChild shm slot options targets = do
  state <- newState defaultSettings
  result <- simpleSessionWithDebugLog state (emptyArgs []) {ghcOptions = options} (runBuildPlan targets)
  case result of
    Nothing -> error "metadata step failed in forked child (see stderr for diagnostics)"
    Just schema -> writeSlot shm slot (LazyByteString.toStrict (encode schema))

killAll :: [ProcessID] -> IO ()
killAll pids =
  forM_ pids \pid -> do
    _ <- try @SomeException (signalProcess sigKILL pid)
    _ <- try @SomeException (getProcessStatus True False pid)
    pure ()

withSingleCapability :: IO Int
withSingleCapability = do
  n <- getNumCapabilities
  setNumCapabilities 1
  pure n

-- | The experiment: fork one child per unit, each independently running a full metadata step (own 'WorkerState', own
-- GHC session, own logger), and collect both results from the shared @mmap@ region.
forkMetadataIO :: FilePath -> IO [BuildPlanSchema]
forkMetadataIO tmp =
  bracket withSingleCapability setNumCapabilities \_ ->
    bracket mmapRegion munmapRegion \shm -> do
      unit1Targets <- fileUnitTargets (tmp </> "src") unit1Spec
      unit1Db <- createEmptyHomeUnitDb unit1Spec (tmp </> "unit1-db") ["M1"]
      let dummyFile = tmp </> "Dummy.hs"
      writeFile dummyFile ""
      pids <-
        forM [
          (0 :: Int, ghcOptions unit1 [], toList unit1Targets),
          (1, ghcOptions unit2 [(unit1, Just unit1Db)], toList (pureUnitTargets dummyFile unit2Spec))
          ] \(slot, options, targets) ->
          forkProcess (runUnitChild shm slot options targets)
      (do
        statuses <- forM pids \pid -> getProcessStatus True False pid
        forM_ statuses \status ->
          if status == Just (Exited ExitSuccess)
          then pure ()
          else error ("unexpected child exit status: " ++ show status)
        payloads <- forM [0, 1] (readSlot shm)
        either error pure (traverse eitherDecodeStrict payloads))
        `onException` killAll pids

test_forkMetadata :: TestName -> TestTree
test_forkMetadata name =
  withTemp "fork-metadata" \tmpResource ->
    unitTest name do
      tmp <- liftIO tmpResource
      schemas <- liftIO (forkMetadataIO tmp)
      case schemas of
        [schema1, schema2] -> do
          annotate (show (schema1, schema2))
          schema1.exposed_modules === Just ["M1"]
          schema1.module_graph === Just [("M1", [])]
          schema2.exposed_modules === Just ["M2"]
          schema2.module_graph === Just [("M2", [])]
        _ -> error "expected exactly two build plan schemas"
