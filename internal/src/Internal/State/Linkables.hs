{-# LANGUAGE CPP #-}

module Internal.State.Linkables where

import Control.Concurrent (MVar)
import GHC.Driver.Env.Types (HscEnv)
import Types.State (WorkerState)

#if defined(LINKABLES)

import Control.Concurrent (modifyMVar)
import Data.Foldable (traverse_)
import GHC (Module)
import GHC.Driver.Env (hscInterp)
import qualified GHC.Driver.Env.Types as GHC
import GHC.Driver.Env.Types (HscEnv (..), LinkDeps, Linkables (Linkables))
import GHC.Linker.Deps (LinkDepsOpts)
import GHC.Linker.Loader (initLinkDepsOpts)
import GHC.Linker.Types (Linkable, LoaderState)
import GHC.Runtime.Interpreter (Interp)
import GHC.Types.SrcLoc (SrcSpan)
import GHC.Unit (moduleUnitId)
import GHC.Unit.Home.Graph (HomeUnitEnv (..), unitEnv_lookup_maybe)
import GHC.Unit.Home.ModInfo (HomeModInfo (..), HomeModLinkable (..))
import GHC.Unit.Home.PackageTable (addHomeModInfoToHpt)
import GHC.Unit.Module.ModIface (mi_module)
import Internal.Cache.Hpt (loadCachedByteCode)
import Internal.Compat.GHC914 (setExtraDecls)
import Internal.Compat.LinkDeps (getLinkDeps)
import Types.State (WorkerState (..))
import Types.State.Make (MakeState (..))

lazyLoadByteCode ::
  MVar WorkerState ->
  HscEnv ->
  HomeModInfo ->
  IO (Maybe Linkable)
lazyLoadByteCode stateVar hsc_env hmi =
  modifyMVar stateVar \ state ->
    loadCachedByteCode hsc_env "" (hm_iface hmi) (hm_details hmi) >>= \case
      Just bytecode -> do
        let iface = hm_iface hmi
            new = hmi {hm_iface = setExtraDecls Nothing iface, hm_linkable = hmi.hm_linkable {homeMod_bytecode = Just bytecode}}
        traverse_ (insertIntoHpt new) (unitEnv_lookup_maybe (moduleUnitId (mi_module iface)) state.make.hug)
        pure (state, Just bytecode)
      Nothing -> pure (state, Nothing)
  where
    insertIntoHpt new hue =
      addHomeModInfoToHpt new (homeUnitEnv_hpt hue)

linkablesResolve ::
  MVar WorkerState ->
  HscEnv ->
  LinkDepsOpts ->
  Interp ->
  LoaderState ->
  SrcSpan ->
  [Module] ->
  IO LinkDeps
linkablesResolve stateVar hsc_env o i l s m =
  getLinkDeps o i l (lazyLoadByteCode stateVar hsc_env) s m

linkablesSelect :: LinkDeps -> IO LinkDeps
linkablesSelect deps = do
  pure deps

newLinkables :: MVar WorkerState -> HscEnv -> LoaderState -> IO Linkables
newLinkables stateVar hsc_env pls = do
  pure Linkables {
    linkablesResolve = linkablesResolve stateVar hsc_env (initLinkDepsOpts hsc_env) (hscInterp hsc_env) pls,
    linkablesSelect
  }

#endif

installLinkables :: MVar WorkerState -> HscEnv -> HscEnv

#if defined(LINKABLES)

installLinkables stateVar hsc_env =
  hsc_env {hsc_linkables = newLinkables stateVar}

#else

installLinkables _ = id

#endif
