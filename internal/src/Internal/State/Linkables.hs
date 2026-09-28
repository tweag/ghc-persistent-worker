{-# LANGUAGE CPP #-}

module Internal.State.Linkables where

import Control.Concurrent (MVar)
import GHC.Driver.Env.Types (HscEnv)
import Types.Log (Logger (..))
import Types.State (WorkerState)

#if defined(LINKABLES)

import Control.Concurrent (modifyMVar, modifyMVar_)
import Control.Monad (foldM)
import Data.Foldable (for_, traverse_)
import Data.Map.Strict qualified as M
import Data.Maybe (mapMaybe)
import Data.Set qualified as S
import GHC (Module)
import GHC.Driver.DynFlags (targetPlatform)
import GHC.Driver.Env (hscInterp)
import GHC.Driver.Env.Types (HscEnv (..))
import GHC.Linker.Deps (LinkDepsOpts, ldUseByteCode, resolveLinkDeps, selectLinkDeps)
import GHC.Linker.Loader (initLinkDepsOpts)
import qualified GHC.Linker.Types
import GHC.Linker.Types (LinkDeps, Linkables (Linkables), LoaderState)
import GHC.Platform (platformSOName)
import GHC.Runtime.Interpreter (Interp)
import GHC.Types.SrcLoc (SrcSpan)
import GHC.Types.Unique.DSet (UniqDSet)
import GHC.Unit (UnitId, moduleUnitId)
import GHC.Unit.Finder (findExactModule)
import GHC.Unit.Finder.Types (InstalledFindResult (..))
import GHC.Unit.Home.Graph (HomeUnitEnv (..), unitEnv_lookup_maybe)
import GHC.Unit.Home.ModInfo (HomeModInfo (..), HomeModLinkable (..))
import GHC.Unit.Home.PackageTable (addHomeModInfoToHpt)
import GHC.Unit.Module.Location (ModLocation)
import GHC.Unit.Module.ModIface (mi_module)
import GHC.Unit.Types (moduleName, toUnitId)
import GHC.Utils.Outputable (parens, ppr, text, (<+>))
import Internal.Cache.Bytecode (linkableBcoUsage, touchBcoCache)
import Internal.Cache.Hpt (loadByteCodeFile, loadCachedByteCodeFrom, reloadIfaceFromDisk)
import Internal.Error (workerErrorIO)
import Internal.State (modifyMakeState)
import Language.Haskell.Syntax.ImpExp (IsBootInterface (..))
import System.Directory (findFile)
import Types.State (WorkerState (..))
import Types.State.Make (HomeByteCode, LibLoadState (..), MakeState (..))

#if defined(GBC)

import Control.Monad (void, when)
import Data.Maybe (catMaybes)
import GHC.Runtime.Interpreter (loadDLLs)
import GHC.Unit.Env (UnitEnv (..))
import GHC.Unit.Home.Graph (lookupHugByModule)
import GHC.Unit.Module.Deps (LinkablePartUsage (..))

#else

import GHC.Driver.Config.Finder (initFinderOpts)
import GHC.Driver.Env (hsc_home_unit, hsc_units)
import GHC.Linker.Deps (LinkModule (..))
import GHC.Linker.Types (Linkable)
import GHC.Runtime.Interpreter (loadDLL)
import GHC.Unit.Finder.Types (FinderCache, FinderOpts)
import GHC.Unit.Home.Graph (HomeUnitGraph, UnitEnvGraph)

#endif


-- | An absent 'ModLocation' should be fatal even if we don't end up loading bytecode, since we always add it to the
-- Finder when restoring from cache.
requireLocation ::
  HscEnv ->
  Module ->
  InstalledFindResult ->
  IO ModLocation
requireLocation hsc_env module_ = \case
  InstalledFound location -> pure location
  InstalledNoPackage {} -> emitError "no package"
  InstalledNotFound {} -> emitError "not found"
  where
    emitError msg =
      workerErrorIO hsc_env ("Lazy bytecode loader could not find location of" <+> ppr module_ <+> parens msg)

#if !defined(GBC)

withFinder :: HscEnv -> HomeUnitGraph -> (FinderCache -> FinderOpts -> UnitEnvGraph FinderOpts -> a) -> a
withFinder hsc_env hug f =
  f hsc_env.hsc_FC (initFinderOpts hsc_env.hsc_dflags) other_fopts
  where
    other_fopts = initFinderOpts . homeUnitEnv_dflags <$> hug

#endif

-- | Load bytecode from an interface given an HPT entry.
--
-- Query the Finder for a 'ModLocation', since we don't have a file path available, which is normally used when loading
-- bytecode from cache upfront.
-- Then, compile bytecode from Core as usual.
-- If the in-memory interface has no Core, reload the interface from disk.
-- If that still yields no Core, skip and return 'Nothing'.
-- Otherwise, add the bytecode 'Linkable' to the HPT in the state and return it.
-- HPTs are stored in 'IORef's, so the bytecode will be available to other compile tasks immediately.
--
-- TODO possible race: when two splices are linked concurrently, and both enter @addLazyByteCode@ with @Nothing@, we'll
-- probably compile bytecode twice.
-- Maybe just look it up in the HUG inside of @modifyMVar@ once more.
lazyLoadByteCode ::
  Logger ->
  MVar WorkerState ->
  HscEnv ->
  HomeModInfo ->
  IO (Maybe HomeByteCode)
lazyLoadByteCode logger stateVar hsc_env hmi = do
  logger.debugD ("Loading lazy bytecode for " <+> ppr module_)
  modifyMVar stateVar \ state -> do
    location <- findLocation state.make.hug
    bytecode <- loadByteCodeFile hsc_env location module_ >>= maybe (fromCore location) (pure . Just)
    maybe (pure (state, Nothing)) (insertBytecode state) bytecode
  where
    fromCore location =
      loadCachedByteCodeFrom hsc_env location (hm_iface hmi) (hm_details hmi) >>= \case
        Just bytecode -> pure (Just bytecode)
        -- The in-memory interface may be missing Core bindings even though the on-disk interface has them, so reload
        -- it from disk and try once more before giving up.
        Nothing -> do
          iface <- reloadIfaceFromDisk hsc_env location
          loadCachedByteCodeFrom hsc_env location iface (hm_details hmi)
    findLocation hug = do
      result <- findExact hug
      requireLocation hsc_env module_ result

#if defined(GBC)
    findExact _ = findExactModule hsc_env (toUnitId <$> module_) NotBoot
#else
    findExact hug = withFinder hsc_env hug findExactModule (hsc_units hsc_env) (Just homeUnit) (toUnitId <$> module_) NotBoot

    homeUnit = hsc_home_unit hsc_env
#endif

    insertBytecode state bytecode = do
      state' <- insertIntoHpt state hmi bytecode
      pure (state', Just bytecode)

    module_ = mi_module hmi.hm_iface

-- | Overwrite a 'HomeModInfo''s bytecode with the given 'Linkable' and insert the updated entry into the HPT of
-- whichever unit it belongs to, per the 'WorkerState''s currently persisted 'HomeUnitGraph'. Shared by
-- 'lazyLoadByteCode' (Core-based reconstruction) and 'loadImportedByteCode' (externally supplied bytecode), both of
-- which need to make a freshly obtained 'Linkable' visible to the rest of the HPT immediately.
insertIntoHpt :: WorkerState -> HomeModInfo -> HomeByteCode -> IO WorkerState
insertIntoHpt state hmi bytecode =
  state <$ traverse_ insertOne (unitEnv_lookup_maybe unit state.make.hug)
  where
    new = hmi {hm_linkable = hmi.hm_linkable {homeMod_bytecode = Just bytecode}}
    unit = moduleUnitId (mi_module hmi.hm_iface)
    insertOne hue = addHomeModInfoToHpt new (homeUnitEnv_hpt hue)

-- | Consult 'MakeState.bytecodeImport' for the given module's bytecode before reconstructing it from Core
-- ('lazyLoadByteCode'). A hit rehydrates the entry via the closure the caller installed (e.g.
-- 'GhcServer.Build.SharedBytecode', mirrored bytecode imported from a parent process over shared memory), inserts
-- it into the HPT the same way 'lazyLoadByteCode' would, and removes the entry from the map -- once inserted, the
-- module's 'HomeModLinkable.homeMod_bytecode' is no longer 'Nothing', so 'addLazyByteCode' will never look it up
-- again. A miss falls through to 'lazyLoadByteCode' unchanged.
loadImportedByteCode ::
  Logger ->
  MVar WorkerState ->
  HscEnv ->
  HomeModInfo ->
  IO (Maybe HomeByteCode)
loadImportedByteCode logger stateVar hsc_env hmi = do
  imported <- modifyMVar stateVar \ state ->
    case M.lookup key state.make.bytecodeImport of
      Nothing -> pure (state, Nothing)
      Just rehydrate -> do
        bytecode <- rehydrate hsc_env module_
        state' <- insertIntoHpt state hmi bytecode
        pure (state' {make = state'.make {bytecodeImport = M.delete key state'.make.bytecodeImport}}, Just bytecode)
  case imported of
    Just bytecode -> do
      logger.debugD ("Restored imported bytecode for " <+> ppr module_)
      pure (Just bytecode)
    Nothing -> lazyLoadByteCode logger stateVar hsc_env hmi
  where
    module_ = mi_module hmi.hm_iface
    key = (moduleUnitId module_, moduleName module_)

loadDLL_ :: HscEnv -> Interp -> [FilePath] -> String -> IO ()
loadDLL_ hsc_env interp lib_paths lib = do
  let dflags = hsc_dflags hsc_env
      platform = targetPlatform dflags
      so_name = platformSOName platform lib
  mb_so_file  <- findFile lib_paths so_name
  case mb_so_file of
    Nothing -> emitError ("Library not found: " <+> text so_name)
    Just so_file -> do
      e <- loadDLLCompat interp so_file
      case e of
        Left err -> emitError (text err)
        Right _ -> pure ()
  where
    emitError msg =
      workerErrorIO hsc_env ("Loading DLL error:" <+> msg)

loadDLLCompat :: Interp -> FilePath -> IO (Either String ())

#if defined(GBC)

loadDLLCompat interp so_file = fmap (const ()) <$> loadDLLs interp [so_file]

#else

loadDLLCompat interp so_file = fmap (const ()) <$> loadDLL interp so_file

#endif

ensureLibraries :: MVar WorkerState -> HscEnv -> Interp -> [UnitId] -> IO ()
ensureLibraries stateVar hsc_env interp unitIds =
  for_ unitIds \unit_id ->
    modifyMakeState stateVar \make -> do
      let m = make.extraLib.requested
      case M.lookup unit_id m of
        Nothing -> pure (make, ())
        Just (lib_paths, libs) -> do
          let load loaded lib
                | lib `S.member` loaded = pure loaded
                | otherwise = loadDLL_ hsc_env interp lib_paths lib >> pure (S.insert lib loaded)
          loaded' <- foldM load make.extraLib.loaded libs
          pure (make {extraLib = LibLoadState m loaded'}, ())

hmiUnitId :: HomeModInfo -> UnitId
hmiUnitId HomeModInfo {hm_iface} = moduleUnitId (mi_module hm_iface)

-- | Wrap the native 'selectLinkDeps' to update the BCO tracking metadata.
linkablesSelect ::
  MVar WorkerState ->
  LinkDepsOpts ->
  Interp ->
  SrcSpan ->
  ResolvedLinkDeps ->
  IO LinkDeps
linkablesSelect stateVar opts interp srcSpan resolved = do
  deps <- selectLinkDeps opts interp srcSpan resolved
  modifyMVar_ stateVar \ state ->
    pure state {make = touchBcoCache (linkDepsBcoUsage deps) state.make}
  pure deps

#if defined(GBC)

type ResolvedLinkDeps = ([GHC.Linker.Types.LinkableUsage], [Module], [UnitId], UniqDSet UnitId)

-- | Modules whose bytecode was used by a link, with BCO counts for those that were newly loaded.
-- Usages of already loaded linkables don't carry the bytecode, so their size is unknown.
linkDepsBcoUsage :: LinkDeps -> [(Module, Maybe Int)]
linkDepsBcoUsage deps =
  [
    (m, M.lookup m needed)
    | usage <- deps.ldAllLinkables
    , any isByteCode usage.linkableParts
    , let m = usage.linkableModule
  ]
  where
    needed = M.fromList (mapMaybe linkableBcoUsage deps.ldNeededLinkables)

    isByteCode = \case
      ByteCodeLinkablePartUsage {} -> True
      FileLinkablePartUsage {} -> False

-- | If the home module is missing bytecode (because it was restored from cache), load it from the interface into the
-- HPT, whose entries are stored in 'IORef's, so that 'selectLinkDeps' will find it when looking up the module.
-- If the interface does not contain Core bindings, this will not have an effect.
addLazyByteCode ::
  Logger ->
  MVar WorkerState ->
  HscEnv ->
  HomeModInfo ->
  IO ()
addLazyByteCode logger stateVar hsc_env = \case
  hmi@HomeModInfo {hm_linkable = HomeModLinkable {homeMod_bytecode = Nothing}} ->
    void (loadImportedByteCode logger stateVar hsc_env hmi)
  _ -> pure ()

-- | Wrap the native 'resolveLinkDeps' to lazily load bytecode for all home modules that lack it.
linkablesResolve ::
  Logger ->
  MVar WorkerState ->
  HscEnv ->
  LinkDepsOpts ->
  LoaderState ->
  SrcSpan ->
  [Module] ->
  IO ResolvedLinkDeps
linkablesResolve logger stateVar hsc_env opts pls srcSpan mods = do
  resolved@(_, needed, _, _) <- resolveLinkDeps opts pls srcSpan mods
  homeMods <- catMaybes <$> traverse (`lookupHugByModule` hsc_env.hsc_unit_env.ue_home_unit_graph) needed
  when (ldUseByteCode opts) do
    traverse_ (addLazyByteCode logger stateVar hsc_env) homeMods
  ensureLibraries stateVar hsc_env (hscInterp hsc_env) (hmiUnitId <$> homeMods)
  pure resolved

#else

type ResolvedLinkDeps = ([Linkable], [LinkModule], UniqDSet UnitId, [UnitId])

-- | Modules whose bytecode was used by a link, with their BCO counts.
linkDepsBcoUsage :: LinkDeps -> [(Module, Maybe Int)]
linkDepsBcoUsage deps =
  fmap Just <$> mapMaybe linkableBcoUsage deps.ldAllLinkables

-- | If the link target is a home module that's missing bytecode (because it was restored from cache), load it from the
-- interface into the HPT and store it in the returned value.
-- 'selectLinkDeps' will then choose the bytecode for linking.
-- If the interface does not contain Core bindings, this will not have an effect.
addLazyByteCode ::
  Logger ->
  MVar WorkerState ->
  HscEnv ->
  LinkModule ->
  IO LinkModule
addLazyByteCode logger stateVar hsc_env = \case
  LinkHomeModule hmi@HomeModInfo {hm_linkable = HomeModLinkable {homeMod_bytecode = Nothing}} -> do
    homeMod_bytecode <- loadImportedByteCode logger stateVar hsc_env hmi
    pure (LinkHomeModule hmi {hm_linkable = hmi.hm_linkable {homeMod_bytecode}})
  lm -> pure lm

-- | Wrap the native 'resolveLinkDeps' to lazily load bytecode for all home modules that lack it.
linkablesResolve ::
  Logger ->
  MVar WorkerState ->
  HscEnv ->
  LinkDepsOpts ->
  LoaderState ->
  SrcSpan ->
  [Module] ->
  IO ResolvedLinkDeps
linkablesResolve logger stateVar hsc_env opts pls srcSpan mods = do
  (loaded, needed, allUnits, neededUnits) <- resolveLinkDeps opts pls srcSpan mods
  neededWithLazy <-
    if ldUseByteCode opts
    then traverse (addLazyByteCode logger stateVar hsc_env) needed
    else pure needed
  ensureLibraries stateVar hsc_env (hscInterp hsc_env) [hmiUnitId hmi | LinkHomeModule hmi <- neededWithLazy]
  pure (loaded, neededWithLazy, allUnits, neededUnits)

#endif

newLinkables ::
  Logger ->
  MVar WorkerState ->
  HscEnv ->
  LoaderState ->
  IO Linkables
newLinkables logger stateVar hsc_env pls = do
  pure Linkables {
    linkablesResolve = linkablesResolve logger stateVar hsc_env (initLinkDepsOpts hsc_env) pls,
    linkablesSelect = linkablesSelect stateVar (initLinkDepsOpts hsc_env) (hscInterp hsc_env)
  }

#endif

installLinkables :: Logger -> MVar WorkerState -> HscEnv -> HscEnv

#if defined(LINKABLES)

installLinkables logger stateVar hsc_env =
  hsc_env {hsc_linkables = newLinkables logger stateVar}

#else

installLinkables _ _ = id

#endif
