{-# LANGUAGE CPP #-}

-- | Standalone harness for the executor bytecode plan protocol (see the project note
-- @task-executor-bytecode-transport@).
--
-- The parent ('runGbcTransport') compiles a generated project in-process with @-fbyte-code-and-object-code@, which
-- writes @.gbc@ files, and stores the resulting home unit graph and module graph in its 'Types.State.WorkerState'
-- like the build server would.
-- It then spawns the given executable as a persistent executor ('GhcServer.Build.ExecutorChild') and runs the entry
-- via 'GhcServer.Build.ExecutorPlan.executePlanned' in several rounds that exercise the delta planning:
--
-- 1. Fresh executor: the whole closure is sent (@Fat@, @Ffi@ with a foreign stub, @Entry@ importing @containers@).
-- 2. Same request: nothing is sent.
-- 3. After recompiling @Fat@ with a different offset: @Fat@ and its dependent @Entry@ are resent and relinked.
-- 4. After respawning the executor: the whole closure is sent again.
--
-- For comparison, it also measures 'GhcServer.Build.BytecodeMirror' rehydration in-process.
module GhcServer.GbcTransport where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as ByteString
import Data.List.NonEmpty (NonEmpty (..))
import GHC.Unit.Types (stringToUnitId)
import Test.PackageDb (UnitSpec (..), moduleSpec)
import Test.Target (ghcOptions)

#if defined(GBC)

import Control.Concurrent.MVar (MVar, modifyMVar_, readMVar)
import Control.Monad (join, unless)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (toList, traverse_)
import qualified Data.Map.Strict as Map
import Data.Text (unpack)
import qualified Data.Text as Text
import GHC (
  Ghc,
  LoadHowMuch (LoadAllTargets),
  Module,
  getModuleGraph,
  getSession,
  load,
  mkModuleName,
  moduleName,
  moduleNameString,
  setTargets,
  )
import GHC.ByteCode.Serialize (readBinByteCode)
import GHC.ByteCode.Types (CompiledByteCode (..), UnlinkedBCO (..))
import GHC.Clock (getMonotonicTime)
import GHC.Compact (Compact, compact, getCompact)
import GHC.Data.FlatBag (elemsFlatBag)
import GHC.Driver.DynFlags (targetProfile)
import GHC.Driver.Env (HscEnv (..), hscInterp, hsc_HPT)
import GHC.Linker.Loader (loadName)
import GHC.Linker.Types (Linkable, LinkableWith (..), ModuleByteCode (..), mkModuleByteCodeLinkable)
import GHC.Runtime.Interpreter.Types (Interp)
import GHCi.RemoteTypes (ForeignHValue)
#if defined(PROFILING)
import GHC.Runtime.Interpreter (evalString)
#else
import GHC.Runtime.Interpreter (wormhole)
import GHCi.RemoteTypes (HValue (..))
import Unsafe.Coerce (unsafeCoerce)
#endif
import GHC.Types.Name (Name, isExternalName, nameOccName)
import GHC.Types.Name.Occurrence (occNameString)
import GHC.Unit.Env (UnitEnv (..))
import GHC.Unit.Home.ModInfo (HomeModInfo)
import GHC.Unit.Home.PackageTable (lookupHpt)
import GHC.Unit.Module.Graph (mgModSummaries, mgModSummaries')
import GHC.Unit.Module.Location (ModLocation (..))
import GHC.Unit.Module.ModSummary (ModSummary (..))
import GhcServer.Build.BytecodeMirror (
  MirrorLinkable,
  homeModBytecodeLinkable,
  mirrorLinkable,
  mirrorSourceFor,
  rehydrateLinkable,
  )
import GhcServer.Build.Executor (awaitExecutor, rtsArgs, spawnExecutorWith, stopExecutor)
import GhcServer.Build.ExecutorLoad (planLinkables)
import GhcServer.Build.ExecutorPlan (displayPlanError, executePlanned, showHomeModuleId)
import GhcServer.Build.PayloadArena (payloadSegmentPath, removeSegment)
import GhcServer.Data.ExecutorPlan (EntryKind (..), ExecutorPlan (..), PayloadTransport (..), PlannedModule (..))
import GhcServer.Data.ProcessEval (ProcessEvalOutput (..), ProcessEvalResult (..), EvalOutcome (..))
import Internal.Session (simpleSessionWithDebugLog)
import Internal.State (newState)
import Internal.State.Make (storeModuleGraphNodes)
import System.Directory (getModificationTime)
import System.Exit (die, exitFailure)
import System.FilePath ((</>))
import System.OsPath.Extra (toOsPath)
import Test.Target (fileUnitTargets)
import Text.Read (readMaybe)
import qualified Types.Args as Args
import Types.Args (emptyArgs)
import Types.Api (ExecutorId, ProcessStats (..))
import Data.List (isPrefixOf, isSuffixOf, sortOn)
import Numeric (readHex)
import System.Process (getPid)
import Types.Settings (defaultSettings)
import Types.State (WorkerState (..))
import Types.State.Executor (ExecutorHandle (..), PayloadArena (..), PayloadCache (..))
import Types.State.Make (MakeState (..))

#else

import System.Exit (die)

#endif

-- | Options for the parent side.
data GbcTransportOptions =
  GbcTransportOptions {
    -- | The executable spawned as the executor, invoked with @executor --socket@.
    serverExe :: FilePath,
    -- | Directory into which the generated project is written.
    project :: FilePath,
    -- | Number of generated top-level functions in the fat module.
    functions :: Int,
    -- | RTS options for the child, passed as @+RTS ... -RTS@.
    childRts :: [String],
    -- | Transfer payloads through a shared memory segment instead of files.
    sharedPayloads :: Bool
  }
  deriving stock (Show)

-- | A module with many distinct top-level bindings, each compiling to its own BCO, plus an exported sum that
-- includes the given offset, so that recompiling with another offset changes the module's bytecode.
fatModule :: Int -> Int -> [ByteString]
fatModule count offset =
  ["module Fat where", "fat :: Int", "fat = " <> int offset <> " + " <> sumExpr] ++ concatMap fun indexes
  where
    indexes = [0 .. count - 1] :: [Int]

    sumExpr = ByteString.intercalate " + " [name i <> " " <> int i | i <- indexes]

    fun :: Int -> [ByteString]
    fun i =
      [
        name i <> " :: Int -> Int",
        name i <> " x = case x of { " <> int i <> " -> " <> int (i * 3) <> "; " <> int (i + 1) <> " -> "
          <> int (i * 7) <> "; _ -> x + " <> int (i * 11) <> " }"
      ]

    name i = "fat_" <> int i

    int = ByteString.pack . show

-- | Expected value of 'Entry.entry' for the given function count and offset.
expectedEntry :: Int -> Int -> Int
expectedEntry count offset =
  sum [i * 3 | i <- [0 .. count - 1] :: [Int]] + 42 + offset

unitSpec :: Int -> Int -> UnitSpec
unitSpec count offset =
  UnitSpec {
    name = "unit1",
    deps = [],
    modules =
      moduleSpec "Fat" (fatModule count offset) :|
      [
      moduleSpec "Ffi" [
        "{-# LANGUAGE CApiFFI #-}",
        "module Ffi where",
        "import Foreign.C.Types (CInt (..))",
        "foreign import capi \"stdlib.h abs\" c_abs :: CInt -> CInt"
      ],
      moduleSpec "Entry" [
        "module Entry where",
        "import qualified Data.Map.Strict as Map",
        "import Fat (fat)",
        "import Ffi (c_abs)",
        "entry :: IO String",
        "entry = pure (show (fat + 42 + fromIntegral (c_abs 0) + Map.size (Map.fromList [(0 :: Int, ())]) - 1))"
      ]]
  }

-- | GHC options shared by the compiling parent session and the decoding child session.
-- @containers@ and the @capi@ import in @Ffi@ cover non-@base@ packages and foreign stub objects in the payloads.
transportOptions :: [String]
transportOptions =
  ghcOptions (stringToUnitId "unit1") [] ++ ["-package", "containers", "-fbyte-code-and-object-code", "-fprefer-byte-code"]

#if defined(GBC)

-- | Run a 'Ghc' action in the given 'Types.State.WorkerState' with the given GHC options.
sessionIn :: MVar WorkerState -> [String] -> Ghc a -> IO a
sessionIn state options ma =
  maybe (die "GHC session failed") pure =<< simpleSessionWithDebugLog state (emptyArgs []) {Args.ghcOptions = options} ma

-- | Run a 'Ghc' action in a fresh 'Types.State.WorkerState' with the given GHC options.
freshSession :: [String] -> Ghc a -> IO a
freshSession options ma = do
  state <- newState defaultSettings
  sessionIn state options ma

timed :: IO a -> IO (a, Double)
timed ma = do
  start <- getMonotonicTime
  a <- ma
  end <- a `seq` getMonotonicTime
  pure (a, end - start)

-- | The bytecode files of all modules in the module graph, dependencies first.
bytecodeFiles :: [ModSummary] -> [FilePath]
bytecodeFiles summaries =
  [ml_bytecode_file s.ms_location | name <- ["Fat", "Ffi", "Entry"], s <- summaries, moduleNameString (moduleName s.ms_mod) == name]

-- | Mirror and compact the bytecode of the given home module, as 'GhcServer.Build.SharedBytecode' does.
mirrorOf :: HscEnv -> HomeModInfo -> IO (Maybe (Module, Compact MirrorLinkable))
mirrorOf env hmi = do
  src <- mirrorSourceFor (targetProfile env.hsc_dflags) hmi
  traverse (\ (m, ml) -> (m,) <$> compact ml) do
    lnk <- homeModBytecodeLinkable hmi
    (lnk.linkableModule,) <$> mirrorLinkable src lnk

linkableFor :: HscEnv -> FilePath -> IO (ModuleByteCode, Linkable)
linkableFor env path = do
  time <- getModificationTime path
  mbc <- readBinByteCode env path
  pure (mbc, mkModuleByteCodeLinkable time mbc)

entryName :: ModuleByteCode -> Maybe Name
entryName mbc =
  case filter isEntry (unlinkedBCOName <$> elemsFlatBag mbc.gbc_compiled_byte_code.bc_bcos) of
    [n] -> Just n
    _ -> Nothing
  where
    isEntry n = isExternalName n && occNameString (nameOccName n) == "entry"

-- | Decode all bytecode files in the current (interface-free) session, link them via a constant hook and run the
-- entry action (@IO String@) in the last one.
-- Profiled GHCs force the external interpreter when bytecode is used, so profiled builds evaluate the entry with
-- 'evalString'; otherwise the internal interpreter is used and the entry runs in this process via 'wormhole'.
-- Returns the result and the decode time (to WHNF of the decoded list).
runFromGbc :: [FilePath] -> HscEnv -> IO (Maybe Int, Double)
runFromGbc paths env0 = do
  (decoded, decodeTime) <- timed (traverse (linkableFor env0) paths)
  let env = env0 {hsc_linkables = \ _ _ -> pure (planLinkables (snd <$> decoded) [])}
      interp = hscInterp env
  result <- traverse (\ name -> do
    (fhv, _, _) <- loadName interp env name
    readMaybe <$> runEntry interp fhv
    ) (entryName . fst =<< lastMaybe decoded)
  pure (join result, decodeTime)
  where
    lastMaybe = foldl (const Just) Nothing

-- | Run an @IO String@ entry.
runEntry :: Interp -> ForeignHValue -> IO String
#if defined(PROFILING)
runEntry = evalString
#else
runEntry interp fhv = do
  HValue value <- wormhole interp fhv
  unsafeCoerce value :: IO String
#endif

-- | Compile the generated project in the given state and store the home unit graph and module graph in its
-- 'MakeState', as the build server's compile steps do.
-- Returns the @.gbc@ paths and the compacted mirror of @Fat@.
compileInto ::
  MVar WorkerState ->
  GbcTransportOptions ->
  Int ->
  IO ([FilePath], Maybe (Module, Compact MirrorLinkable))
compileInto stateVar options offset = do
  targets <- fileUnitTargets options.project (unitSpec options.functions offset)
  (env, graph, mirror) <- sessionIn stateVar transportOptions do
    setTargets (toList targets)
    _ <- load LoadAllTargets
    env <- getSession
    graph <- getModuleGraph
    mhmi <- liftIO (lookupHpt (hsc_HPT env) (mkModuleName "Fat"))
    mirror <- liftIO (maybe (pure Nothing) (mirrorOf env) mhmi)
    pure (env, graph, mirror)
  modifyMVar_ stateVar \ state ->
    pure state {
      make = storeModuleGraphNodes (mgModSummaries' graph) state.make {hug = env.hsc_unit_env.ue_home_unit_graph}
    }
  pure (bytecodeFiles (mgModSummaries graph), mirror)

-- | Compile the generated project in a fresh state, returning the @.gbc@ paths and the compacted mirror of @Fat@.
compileProject :: GbcTransportOptions -> IO ([FilePath], Maybe (Module, Compact MirrorLinkable))
compileProject options = do
  stateVar <- newState defaultSettings
  compileInto stateVar options 0

-- | Time rehydration of the mirror in a fresh in-process session.
mirrorRehydrateTime :: (Module, Compact MirrorLinkable) -> IO Double
mirrorRehydrateTime (mod_, region) =
  freshSession transportOptions do
    env <- getSession
    liftIO (snd <$> timed (rehydrateLinkable env mod_ (getCompact region)))

executorId :: ExecutorId
executorId = "gbc"

-- | Spawn the executor, register it in the state (replacing and stopping a previous one) and wait for its socket.
spawnRegistered :: MVar WorkerState -> GbcTransportOptions -> IO ()
spawnRegistered stateVar options = do
  old <- Map.lookup executorId . (.executors) <$> readMVar stateVar
  traverse_ stopExecutor old
  handle <- spawnExecutorWith options.serverExe (rtsArgs options.childRts) (toOsPath (options.project </> "executor.sock"))
  modifyMVar_ stateVar \ state -> pure state {executors = Map.insert executorId handle state.executors}
  awaitExecutor handle
  childRss stateVar >>= traverse_ (putStrLn . ("[idle] child " ++))
  childMappings stateVar >>= traverse_ (putStrLn . ("[idle] map " ++))

-- | Run one round and check its result and the number of sent modules, returning whether both match.
round_ :: MVar WorkerState -> GbcTransportOptions -> PayloadTransport -> String -> Int -> Int -> IO Bool
round_ stateVar options transport label offset expectedSent = do
  (result, seconds) <- timed (executePlanned transport stateVar executorId transportOptions root "entry" EntryString)
  putStrLn ("[" ++ label ++ "] seconds: " ++ show seconds)
  case result of
    Left err -> False <$ putStrLn ("[" ++ label ++ "] failed: " ++ displayPlanError err)
    Right (plan, output) -> do
      let sent = showHomeModuleId . (.home) <$> plan.modules
          value = either ("undecodable: " ++) outcomeValue (describe output)
      putStrLn ("[" ++ label ++ "] generation " ++ show plan.generation ++ ", sent: " ++ show sent)
      putStrLn ("[" ++ label ++ "] result: " ++ value)
      traverse_ (putStrLn . (("[" ++ label ++ "] child stderr: ") ++)) (childStderr output)
      traverse_ (putStrLn . ((("[" ++ label ++ "] child heap: ") ++) . describeStats) . (.stats)) output.result
      childRss stateVar >>= traverse_ (putStrLn . ((("[" ++ label ++ "] child ") ++)))
      childMappings stateVar >>= traverse_ (putStrLn . ((("[" ++ label ++ "] map ") ++)))
      let ok = value == show (expectedEntry options.functions offset) && length sent == expectedSent
      unless ok (putStrLn ("[" ++ label ++ "] expected " ++ show expectedSent ++ " sent modules and result "
        ++ show (expectedEntry options.functions offset)))
      pure ok
  where
    root = (stringToUnitId "unit1", mkModuleName "Entry")

    describe output = either (Left . unpack) Right output.result

    outcomeValue r =
      case r.outcome of
        EvalSuccess (Just v) -> unpack v
        other -> show other

    childStderr output =
      case output.result of
        Right r | not (Text.null r.evalStderr) -> Just (unpack r.evalStderr)
        _ -> Nothing

describeStats :: ProcessStats -> String
describeStats s =
  "max mem in use " ++ mib s.maxMemInUseBytes ++ ", max live " ++ mib s.maxLiveBytes
  where
    mib b = show (b `div` (1024 * 1024)) ++ " MiB"

-- | The resident memory lines of the executor's @\/proc\/PID\/status@: total, anonymous, file-backed and shared
-- memory (the latter includes mapped @\/dev\/shm@ pages).
childRss :: MVar WorkerState -> IO [String]
childRss stateVar = do
  handle <- Map.lookup executorId . (.executors) <$> readMVar stateVar
  pid <- join <$> traverse (getPid . (.process)) handle
  maybe (pure []) (fmap (filter isRss . lines) . readFile . (\ p -> "/proc/" ++ show p ++ "/status")) pid
  where
    isRss :: String -> Bool
    isRss l = any (`isPrefixOf` l) (["VmRSS", "RssAnon", "RssFile", "RssShmem", "VmHWM"] :: [String])

-- | The executor's largest mappings from @\/proc\/PID\/smaps@, with resident kB summed per backing path (anonymous
-- mappings grouped by size class, since the RTS linker and allocator don't name them).
childMappings :: MVar WorkerState -> IO [String]
childMappings stateVar = do
  handle <- Map.lookup executorId . (.executors) <$> readMVar stateVar
  pid <- join <$> traverse (getPid . (.process)) handle
  maybe (pure []) (fmap summarize . readFile . (\ p -> "/proc/" ++ show p ++ "/smaps")) pid
  where
    summarize = fmap render . take 8 . sortOn (negate . snd) . Map.toList . Map.fromListWith (+) . mappings . lines

    render (name, kb) = show kb ++ " kB " ++ name

    mappings = \case
      header : rest
        | (range : _perms : _off : _dev : _inode : path) <- words header
        , '-' `elem` range
        , (fields, next) <- break isHeader rest
        -> (label range path, rssOf fields) : mappings next
      _ : rest -> mappings rest
      [] -> []

    isHeader l = case words l of
      w : _ -> '-' `elem` w && not (":" `isSuffixOf` w)
      [] -> False

    rssOf fields = sum [read n | l <- fields, ["Rss:", n, _] <- [words l]] :: Int

    label range = \case
      [] -> "[anon " ++ sizeClass range ++ "]"
      path -> unwords path

    sizeClass range =
      let (lo, hi) = break (== '-') range
          size = (fst (head (readHex (drop 1 hi))) - fst (head (readHex lo))) :: Integer
      in if size >= 1024 * 1024 * 1024 then ">=1G" else if size >= 64 * 1024 * 1024 then ">=64M"
         else if size >= 1024 * 1024 then ">=1M" else "<1M"

-- | Shared memory if requested, otherwise files, with in-memory payloads written to the project directory.
payloadTransportFor :: GbcTransportOptions -> IO PayloadTransport
payloadTransportFor options
  | options.sharedPayloads = PayloadSegment <$> payloadSegmentPath
  | otherwise = pure (PayloadFiles (options.project </> "payloads"))

-- | Parent side: compile, run the rounds described in the module header in an executor, and print a report.
runGbcTransport :: GbcTransportOptions -> IO ()
runGbcTransport options = do
  stateVar <- newState defaultSettings
  ((paths, mirror), compileTime) <- timed (compileInto stateVar options 0)
  putStrLn ("compile seconds: " ++ show compileTime)
  putStrLn ("bytecode files: " ++ unwords paths)
  spawnRegistered stateVar options
  transport <- payloadTransportFor options
  let run = round_ stateVar options transport
  fresh <- run "fresh" 0 3
  warm <- run "warm" 0 0
  (_, recompileTime) <- timed (compileInto stateVar options 1)
  putStrLn ("recompile seconds: " ++ show recompileTime)
  replaced <- run "replaced" 1 2
  spawnRegistered stateVar options
  respawned <- run "respawned" 1 3
  rehydrate <- traverse mirrorRehydrateTime mirror
  putStrLn ("in-process mirror rehydrate seconds (Fat only, WHNF): " ++ maybe "no mirror" show rehydrate)
  state <- readMVar stateVar
  traverse_ (\ arena -> putStrLn ("shared segment bytes: " ++ show arena.end)) state.payloads.arena
  traverse_ stopExecutor (Map.lookup executorId state.executors)
  traverse_ (removeSegment . (.segment)) state.payloads.arena
  if and ([fresh, warm, replaced, respawned] :: [Bool])
  then putStrLn "success"
  else putStrLn "failure" >> exitFailure

#else

runGbcTransport :: GbcTransportOptions -> IO ()
runGbcTransport _ = die "gbc-transport requires a GHC with the GBC backport (cabal flag gbc)"

#endif
