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

uninitialised :: a
uninitialised = panic "Loader not initialised"

modifyLoaderState_ :: Interp -> (LoaderState -> IO LoaderState) -> IO ()
modifyLoaderState_ interp f =
  modifyMVar_ (loader_state (interpLoader interp)) (fmap pure . f . fromMaybe uninitialised)

dropModules :: UniqDSet Module -> LoaderState -> LoaderState
dropModules mods pls =
  pls {
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

    victims = mods

-- | Backported from GHC master and stripped down to what we can handle.
-- Upstream has several changes to the 'LoaderState' that we don't have yet.
--
-- Unlike @unload@ in the current GHC, 
unloadModules :: Interp -> HscEnv -> [Module] -> IO ()
unloadModules interp hsc_env mods =
  mask_ do
    initLoaderState interp hsc_env
    modifyLoaderState_ interp \ pls ->
      pure $! dropModules (mkUniqDSet (filter (not . isInteractiveModule) mods)) pls
