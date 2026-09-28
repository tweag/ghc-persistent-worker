{-# LANGUAGE CPP #-}

-- | Parent-side planning of the bytecode that a persistent executor has to load for an execution, for executors
-- that don't load interfaces (see the project note @task-executor-bytecode-transport@).
--
-- For a requested module, the parent computes the transitive closure of home modules from their interfaces and the
-- package units they import, resolves a serialized GBC payload for each module (preferably the @.gbc@ file GHC wrote
-- during compilation, otherwise a file written from the in-memory bytecode), and subtracts what the executor has
-- already loaded.
--
-- GHC's loader only relinks a module if its linkable time changed, and dependents that were linked earlier keep
-- referring to the closures of the replaced version.
-- Therefore a module is resent if its payload hash changed or if one of its dependencies is resent, and loaded
-- modules outside the closure that depend on a replaced module are dropped from the loaded set.
module GhcServer.Build.ExecutorPlan where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import GHC.Unit.Types (unitIdString)
import GhcServer.Data.ExecutorPlan (PlanDelta (..), PlannedModule (..))
import Language.Haskell.Syntax.Module.Name (moduleNameString)
import Types.State.Executor (ExecutorLoaded (..), HomeModuleId, LoadedModule (..))

#if defined(GBC)

import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, readMVar, withMVar)
import Control.Exception (IOException, SomeException, displayException, try)
import Control.Monad (unless)
import Data.List (nub)
import qualified Data.ByteString as ByteString
import Data.ByteString (ByteString)
import Data.Functor ((<&>))
import GHC.ByteCode.Serialize (writeBinByteCode)
import GHC.Fingerprint (Fingerprint, getFileHash)
import GHC.Linker.Types (LinkableWith (..), ModuleByteCode (..))
import GHC.Unit.Home.Graph (lookupHug)
import GHC.Unit.Home.ModInfo (HomeModInfo (..), HomeModLinkable (..))
import GHC.Unit.Module.Deps (Dependencies (..))
import GHC.Unit.Module.Graph (ModuleGraphNode (..), NodeKey (..), moduleNodeInfoLocation)
import GHC.Unit.Module.Location (ModLocation (..))
import GHC.Unit.Module.ModIface (mi_deps)
import GHC.Unit.Module.ModNodeKey (ModNodeKeyWithUid (..))
import GHC.Unit.Types (GenWithIsBoot (..), IsBootInterface (..), UnitId)
import GhcServer.Build.Executor (callExecutorWith)
import GhcServer.Build.GbcBuffer (encodeByteCode)
import GhcServer.Build.PayloadArena (appendPayload, ensureArena)
import GhcServer.Data.ExecutorPlan (
  EntryKind,
  ExecutorPlan (..),
  ExtraLib (..),
  Payload (..),
  PayloadTransport (..),
  PlanRequest (..),
  )
import GhcServer.Data.ProcessEval (EvalOutcome (..), ExecutorCall (..), ProcessEvalOutput (..), ProcessEvalResult (..))
import System.Directory (createDirectoryIfMissing, getFileSize, getModificationTime)
import System.FilePath ((</>))
import Types.Api (ExecutorId)
import Types.State (WorkerState (..))
import Types.State.Executor (ExecutorHandle (..), FileStamp (..), PayloadCache (..))
import Types.State.Make (LibLoadState (requested), MakeState (..))

#endif

showHomeModuleId :: HomeModuleId -> String
showHomeModuleId (uid, name) =
  unitIdString uid ++ ":" ++ moduleNameString name

-- | Collect the values of all keys reachable from the roots, failing with the first key that can't be resolved.
collectReachable :: (Monad m, Ord k) => (k -> m (Maybe v)) -> (v -> [k]) -> [k] -> m (Either k (Map k v))
collectReachable resolve next =
  go Map.empty
  where
    go acc = \case
      [] -> pure (Right acc)
      k : rest
        | Map.member k acc -> go acc rest
        | otherwise ->
          resolve k >>= \case
            Nothing -> pure (Left k)
            Just v -> go (Map.insert k v acc) (next v ++ rest)

-- | The keys reachable from the roots, dependencies first.
-- Keys in a cycle (only possible through @SOURCE@ imports) are ordered arbitrarily among themselves.
dependencyOrder :: Ord k => (k -> [k]) -> [k] -> [k]
dependencyOrder deps roots =
  reverse (snd (foldl' visit (Set.empty, []) roots))
  where
    visit (seen, acc) k
      | Set.member k seen = (seen, acc)
      | otherwise =
        let (seen', acc') = foldl' visit (Set.insert k seen, acc) (deps k)
        in (seen', k : acc')

-- | Transitive dependents of the roots within the loaded set, excluding the roots themselves.
loadedDependents :: Map HomeModuleId LoadedModule -> [HomeModuleId] -> Set.Set HomeModuleId
loadedDependents loaded roots =
  go (Set.fromList roots) roots Set.\\ Set.fromList roots
  where
    reverseDeps = Map.fromListWith (++) [(dep, [k]) | (k, m) <- Map.toList loaded, dep <- m.deps]

    go seen = \case
      [] -> seen
      k : rest ->
        let new = filter (`Set.notMember` seen) (Map.findWithDefault [] k reverseDeps)
        in go (foldr Set.insert seen new) (new ++ rest)

-- | Whether the executor has loaded the module with the same payload hash.
isCurrent :: ExecutorLoaded -> PlannedModule -> Bool
isCurrent loaded m =
  ((.hash) <$> Map.lookup m.home loaded.modules) == Just m.hash

-- | Determine what to send to an executor for a closure given in dependency order.
planDelta :: ExecutorLoaded -> [PlannedModule] -> PlanDelta
planDelta loaded closure =
  PlanDelta {send, invalidated}
  where
    send = reverse (snd (foldl' step (Set.empty, []) closure))

    step (sent, acc) m
      | isCurrent loaded m && not (any (`Set.member` sent) m.deps) = (sent, acc)
      | otherwise = (Set.insert m.home sent, m : acc)

    replaced = [m.home | m <- send, Map.member m.home loaded.modules]

    invalidated = Set.toList (loadedDependents loaded.modules replaced Set.\\ Set.fromList ((.home) <$> closure))

-- | Record a delta as loaded, advancing the generation.
applyDelta :: PlanDelta -> ExecutorLoaded -> ExecutorLoaded
applyDelta delta loaded =
  ExecutorLoaded {
    modules = foldl' insert (foldl' (flip Map.delete) loaded.modules delta.invalidated) delta.send,
    generation = loaded.generation + 1
  }
  where
    insert acc m = Map.insert m.home LoadedModule {hash = m.hash, deps = m.deps} acc

#if defined(GBC)

-- | The dependencies of a home module, read from its interface.
data ModuleSource =
  ModuleSource {
    -- | Direct home-module dependencies. @SOURCE@ imports refer to the actual module, since that is what the
    -- executor must link.
    deps :: [HomeModuleId],
    -- | Directly imported package units.
    packages :: [UnitId],
    hmi :: HomeModInfo
  }

moduleSource :: HomeModInfo -> ModuleSource
moduleSource hmi =
  ModuleSource {
    deps = nub [(uid, gwib_mod name) | (_, uid, name) <- Set.toList (dep_direct_mods deps)],
    packages = [uid | (_, uid) <- Set.toList (dep_direct_pkgs deps)],
    hmi
  }
  where
    deps = mi_deps hmi.hm_iface

-- | The home-module closure of the roots with their dependency information, failing with the first module that isn't
-- in the home unit graph.
homeClosure :: MakeState -> [HomeModuleId] -> IO (Either HomeModuleId (Map HomeModuleId ModuleSource))
homeClosure make =
  collectReachable lookupSource (.deps)
  where
    lookupSource (uid, name) = fmap moduleSource <$> lookupHug make.hug uid name

-- | The location of the @.gbc@ file GHC writes for a module compiled with @-fwrite-byte-code@, taken from the module
-- graph.
bytecodeFile :: MakeState -> HomeModuleId -> Maybe FilePath
bytecodeFile make (uid, name) =
  Map.lookup key make.moduleGraphNodes >>= \case
    ModuleNode _ info -> Just (ml_bytecode_file (moduleNodeInfoLocation info))
    _ -> Nothing
  where
    key = NodeKey_Module ModNodeKeyWithUid {mnkModuleName = GWIB name NotBoot, mnkUnitId = uid}

-- | The stamp of an existing file, or 'Nothing' if it can't be accessed.
fileStamp :: FilePath -> IO (Maybe FileStamp)
fileStamp path =
  either (const Nothing) Just <$> try @IOException do
    modified <- getModificationTime path
    size <- getFileSize path
    pure FileStamp {modified, size}

-- | The content hash of a file, reused from the cache if the file's stamp is unchanged.
cachedFileHash :: PayloadCache -> FilePath -> FileStamp -> IO (PayloadCache, Fingerprint)
cachedFileHash cache path stamp =
  case Map.lookup path cache.fileHashes of
    Just (old, hash) | old == stamp -> pure (cache, hash)
    _ -> do
      hash <- getFileHash path
      pure (cache {fileHashes = Map.insert path (stamp, hash) cache.fileHashes}, hash)

-- | A payload file for in-memory bytecode, written to the payload directory once per content hash.
writtenPayload :: FilePath -> PayloadCache -> ModuleByteCode -> IO (PayloadCache, FilePath)
writtenPayload dir cache mbc =
  case Map.lookup mbc.gbc_hash cache.written of
    Just existing -> pure (cache, existing)
    Nothing -> do
      createDirectoryIfMissing True dir
      writeBinByteCode path mbc
      pure (cache {written = Map.insert mbc.gbc_hash path cache.written}, path)
  where
    path = dir </> show mbc.gbc_hash <> ".gbc"

-- | Where the parent finds a module's bytecode.
data PayloadSource =
  -- | The module's @.gbc@ file.
  SourceFile FilePath FileStamp
  |
  -- | The bytecode in the home package table.
  SourceMemory ModuleByteCode

-- | The module's @.gbc@ file if it exists, otherwise the bytecode in the home package table.
--
-- TODO Modules that have neither (e.g. compiled without @-fwrite-byte-code@ and with lazily loaded bytecode that was
-- never requested) could be compiled from Core like 'Internal.State.Linkables.lazyLoadByteCode' does.
payloadSource :: MakeState -> HomeModuleId -> HomeModInfo -> IO (Either String PayloadSource)
payloadSource make home hmi =
  traverse (\ path -> fmap (SourceFile path) <$> fileStamp path) (bytecodeFile make home) <&> \case
    Just (Just file) -> Right file
    _ -> maybe (Left ("No bytecode available for " ++ showHomeModuleId home)) (Right . SourceMemory . (.linkableParts))
      hmi.hm_linkable.homeMod_bytecode

sourceHash :: PayloadCache -> PayloadSource -> IO (PayloadCache, Fingerprint)
sourceHash cache = \case
  SourceFile path stamp -> cachedFileHash cache path stamp
  SourceMemory mbc -> pure (cache, mbc.gbc_hash)

-- | The serialized bytecode of a source.
sourceBytes :: PayloadSource -> IO ByteString
sourceBytes = \case
  SourceFile path _ -> ByteString.readFile path
  SourceMemory mbc -> encodeByteCode mbc

-- | Make a payload available to executors with the given transport.
placePayload :: PayloadTransport -> PayloadCache -> Fingerprint -> PayloadSource -> IO (PayloadCache, Payload)
placePayload transport cache hash source =
  case (transport, source) of
    (PayloadFiles _, SourceFile path _) -> pure (cache, PayloadFile path)
    (PayloadFiles dir, SourceMemory mbc) -> fmap PayloadFile <$> writtenPayload dir cache mbc
    (PayloadSegment segment, _) -> do
      arena <- ensureArena segment cache.arena
      (arena', slice) <- appendPayload arena hash (sourceBytes source)
      pure (cache {arena = Just arena'}, PayloadShared slice)

-- | Resolve the payload of a home module and its content hash.
resolvePayload ::
  PayloadTransport ->
  MakeState ->
  PayloadCache ->
  HomeModuleId ->
  HomeModInfo ->
  IO (Either String (PayloadCache, (Fingerprint, Payload)))
resolvePayload transport make cache home hmi =
  payloadSource make home hmi >>= traverse \ source -> do
    (hashed, hash) <- sourceHash cache source
    fmap (hash,) <$> placePayload transport hashed hash source

-- | Resolve the payloads of a closure given in dependency order.
plannedModules ::
  PayloadTransport ->
  MakeState ->
  PayloadCache ->
  [(HomeModuleId, ModuleSource)] ->
  IO (Either String (PayloadCache, [PlannedModule]))
plannedModules transport make cache0 =
  fmap (fmap (fmap reverse)) . go cache0 []
  where
    go cache acc = \case
      [] -> pure (Right (cache, acc))
      (home, source) : rest ->
        resolvePayload transport make cache home source.hmi >>= \case
          Left err -> pure (Left err)
          Right (cache', (hash, payload)) ->
            go cache' (PlannedModule {home, hash, deps = source.deps, payload} : acc) rest

-- | The full closure of an execution root: package units and planned home modules in dependency order.
planClosure ::
  PayloadTransport ->
  MakeState ->
  PayloadCache ->
  HomeModuleId ->
  IO (Either String (PayloadCache, ([UnitId], [PlannedModule])))
planClosure transport make cache root =
  homeClosure make [root] >>= \case
    Left missing -> pure (Left ("Module not in the home unit graph: " ++ showHomeModuleId missing))
    Right sources -> do
      let ordered = [(k, s) | k <- dependencyOrder (depsOf sources) [root], Just s <- [Map.lookup k sources]]
          units = nub (concatMap (\ (_, s) -> s.packages) ordered)
      fmap (fmap (units,)) <$> plannedModules transport make cache ordered
  where
    depsOf sources k = maybe [] (.deps) (Map.lookup k sources)

-- | The native libraries requested by the home units of a closure, as recorded in 'MakeState.extraLib'.
closureExtraLibs :: MakeState -> [PlannedModule] -> [ExtraLib]
closureExtraLibs make closure =
  nub [
    ExtraLib {paths, name}
    | uid <- nub (fst . (.home) <$> closure)
    , Just (paths, names) <- [Map.lookup uid make.extraLib.requested]
    , name <- names
  ]

-- | Plan an execution of the root module on the given executor, which must be registered, and record the sent
-- modules as loaded.
--
-- The loaded set is updated before the executor has linked anything; if the execution fails, the caller should reset
-- it (or respawn the executor), since the executor's state is unknown.
-- The payloads are resolved while holding the state lock, which includes hashing new or changed @.gbc@ files.
prepareExecution ::
  PayloadTransport ->
  MVar WorkerState ->
  ExecutorId ->
  HomeModuleId ->
  IO (Either String (ExecutorHandle, ExecutorPlan))
prepareExecution transport stateVar executorId root =
  modifyMVar stateVar \ state ->
    case Map.lookup executorId state.executors of
      Nothing -> pure (state, Left ("Executor not registered: " ++ show executorId))
      Just handle ->
        planClosure transport state.make state.payloads root >>= \case
          Left err -> pure (state, Left err)
          Right (payloads, (units, closure)) -> do
            let delta = planDelta handle.loaded closure
                loaded = applyDelta delta handle.loaded
                plan = ExecutorPlan {
                  units,
                  modules = delta.send,
                  generation = loaded.generation,
                  extraLibs = closureExtraLibs state.make closure
                }
                executors = Map.insert executorId handle {loaded} state.executors
            pure (state {executors, payloads}, Right (handle {loaded}, plan))

-- | Forget which modules the executor has loaded, keeping the generation so that modules sent afterwards get newer
-- linkable times than anything the process may still have linked.
resetLoaded :: MVar WorkerState -> ExecutorId -> IO ()
resetLoaded stateVar executorId =
  modifyMVar_ stateVar \ state ->
    pure state {executors = Map.adjust reset executorId state.executors}
  where
    reset handle = handle {loaded = ExecutorLoaded {modules = Map.empty, generation = handle.loaded.generation}}

-- | The wire request for a plan.
planRequest :: [String] -> HomeModuleId -> String -> EntryKind -> ExecutorPlan -> PlanRequest
planRequest ghcOptions (unit, name) entry entryKind plan =
  PlanRequest {
    ghcOptions,
    units = unitIdString <$> plan.units,
    payloads = (.payload) <$> plan.modules,
    generation = plan.generation,
    extraLibs = plan.extraLibs,
    entryUnit = unitIdString unit,
    entryModule = moduleNameString name,
    entry,
    entryKind
  }

-- | Whether the executor's loaded state is still what the parent recorded after a call.
planApplied :: ProcessEvalOutput -> Bool
planApplied output =
  case output.result of
    Right ProcessEvalResult {outcome = EvalLoadFailed _} -> False
    Right ProcessEvalResult {outcome = EvalOptionsMismatch _} -> False
    Right ProcessEvalResult {outcome = EvalConfigInvalid _} -> False
    Right _ -> True
    Left _ -> False

-- | Why 'executePlanned' produced no executor output.
data PlanError =
  -- | The plan couldn't be computed, e.g. because a module has no bytecode. Nothing was sent.
  PlanUnavailable String
  |
  -- | The call failed, e.g. because the executor died.
  ExecutorUnreachable String
  deriving stock (Eq, Show)

displayPlanError :: PlanError -> String
displayPlanError = \case
  PlanUnavailable err -> "No bytecode plan: " ++ err
  ExecutorUnreachable err -> "Executor unreachable: " ++ err

-- | Plan an execution of the root module's entry on the registered executor, send it and return the plan with the
-- executor's output.
-- If the executor fails to link the plan (or can't be reached), the recorded loaded set is reset with
-- 'resetLoaded', so that the next execution resends the whole closure with newer linkable times.
-- Planning and sending are serialized per executor with the handle's @calls@ lock, since a plan only contains the
-- modules missing from the loaded set recorded by earlier plans, which the executor must have received beforehand.
executePlanned ::
  PayloadTransport ->
  MVar WorkerState ->
  ExecutorId ->
  [String] ->
  HomeModuleId ->
  String ->
  EntryKind ->
  IO (Either PlanError (ExecutorPlan, ProcessEvalOutput))
executePlanned transport stateVar executorId ghcOptions root entry kind =
  lookupCalls >>= \case
    Nothing -> pure (Left (PlanUnavailable ("Executor not registered: " ++ show executorId)))
    Just calls -> withMVar calls \ () -> planAndCall
  where
    lookupCalls = fmap (.calls) . Map.lookup executorId . (.executors) <$> readMVar stateVar

    planAndCall =
      prepareExecution transport stateVar executorId root >>= \case
        Left err -> pure (Left (PlanUnavailable err))
        Right (handle, plan) ->
          try @SomeException (callExecutorWith handle (ExecutePlan (planRequest ghcOptions root entry kind plan))) >>= \case
            Left err -> Left (ExecutorUnreachable (displayException err)) <$ resetLoaded stateVar executorId
            Right output -> do
              unless (planApplied output) (resetLoaded stateVar executorId)
              pure (Right (plan, output))

#endif
