{-# LANGUAGE CPP #-}

module Internal.Compat.Unload where

import Control.Concurrent (modifyMVar_)
import Control.Exception (mask_)
import Data.Maybe (fromMaybe)
import GHC (Name, isExternalName, nameModule)
import GHC.Driver.Env (HscEnv)
import GHC.Linker.Loader (Loader (..), LoaderState (..), initLoaderState)
import GHC.Linker.Types (LinkerEnv (..))
import GHC.Runtime.Interpreter (Interp (..))
import GHC.Types.Name.Env (filterNameEnv)
import GHC.Types.Unique.DSet (UniqDSet, elementOfUniqDSet, mkUniqDSet, uniqDSetToList)
import GHC.Unit.Module.Env (delModuleEnvList)
import GHC.Unit.Types (Module, isInteractiveModule)
import GHC.Utils.Panic (panic)

#if defined(GBC)

import Control.Monad (unless)
import Data.Containers.ListUtils (nubOrd)
import Data.Maybe (mapMaybe)
import GHC.Linker.Types (
  BytecodeState (..),
  LinkedBreaks (..),
  linkableUsageObjs,
  modifyHomePackageBytecodeState,
  )
import GHC.Runtime.Interpreter (purgeLookupSymbolCache)
import GHC.Unit.Module.Env (lookupModuleEnv)

#endif

uninitialised :: a
uninitialised = panic "Loader not initialised"

modifyLoaderState_ :: Interp -> (LoaderState -> IO LoaderState) -> IO ()
modifyLoaderState_ interp f =
  modifyMVar_ (loader_state (interpLoader interp)) (fmap pure . f . fromMaybe uninitialised)

dropModules :: Interp -> UniqDSet Module -> LoaderState -> IO LoaderState

#if defined(GBC)

dropModules interp victims pls = do
  unless (null victim_objs) do
    purgeLookupSymbolCache interp
  pure $! pls {
    bcos_loaded = delModuleEnvList (bcos_loaded pls) victim_list,
    objs_loaded = delModuleEnvList (objs_loaded pls) victim_list,
    bco_loader_state = modifyHomePackageBytecodeState (bco_loader_state pls) drop_bytecode_state
  }
  where
    victim_objs = nubOrd (concatMap linkableUsageObjs victim_usages)

    victim_usages =
      mapMaybe (lookupModuleEnv (bcos_loaded pls)) victim_list
      ++
      mapMaybe (lookupModuleEnv (objs_loaded pls)) victim_list

    victim_list = uniqDSetToList victims

    keep_name :: (Name, a) -> Bool
    keep_name (n, _) = not (isExternalName n)
                    || not (nameModule n `elementOfUniqDSet` victims)

    drop_bytecode_state bs =
      bs {
        bco_linker_env = (bco_linker_env bs) {
          closure_env = filterNameEnv keep_name (closure_env (bco_linker_env bs)),
          itbl_env = filterNameEnv keep_name (itbl_env (bco_linker_env bs)),
          addr_env = filterNameEnv keep_name (addr_env (bco_linker_env bs))
        },
        bco_linked_breaks = (bco_linked_breaks bs) {
          breakarray_env = delModuleEnvList (breakarray_env (bco_linked_breaks bs)) victim_list,
          ccs_env = delModuleEnvList (ccs_env (bco_linked_breaks bs)) victim_list
        }
      }


#else

dropModules _ victims pls = do
  pure $! pls {
    bcos_loaded = delModuleEnvList (bcos_loaded pls) victim_list,
    objs_loaded = delModuleEnvList (objs_loaded pls) victim_list,
    linker_env = (linker_env pls) {
      closure_env = filterNameEnv keep_name (closure_env (linker_env pls)),
      itbl_env = filterNameEnv keep_name (itbl_env (linker_env pls)),
      addr_env = filterNameEnv keep_name (addr_env (linker_env pls))
    }
  }
  where
    keep_name :: (Name, a) -> Bool
    keep_name (n, _) =
      not (isExternalName n) || not (nameModule n `elementOfUniqDSet` victims)

    victim_list = uniqDSetToList victims

#endif

unloadModules :: Interp -> HscEnv -> [Module] -> IO ()
unloadModules interp hsc_env mods =
  mask_ do
    initLoaderState interp hsc_env
    modifyLoaderState_ interp $ dropModules interp (mkUniqDSet (filter (not . isInteractiveModule) mods))
