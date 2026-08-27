module Internal.Compat.Unload where

import Control.Concurrent (modifyMVar_)
import Control.Exception (mask_)
import Data.Maybe (fromMaybe)
import GHC (Name, isExternalName, nameModule)
import GHC.Driver.Env (HscEnv)
import GHC.Linker.Loader (Loader (..), LoaderState (..), initLoaderState)
import GHC.Linker.Types (LinkerEnv (..))
import GHC.Runtime.Interpreter (Interp (..), interpreterDynamic, unloadObj)
import GHC.Types.Name.Env (filterNameEnv)
import GHC.Types.Unique.DSet (UniqDSet, elementOfUniqDSet, mkUniqDSet, uniqDSetToList)
import GHC.Unit.Module.Env (delModuleEnvList)
import GHC.Unit.Types (Module, isInteractiveModule)
import GHC.Utils.Panic (panic)

uninitialised :: a
uninitialised = panic "Loader not initialised"

modifyLoaderState_ :: Interp -> (LoaderState -> IO LoaderState) -> IO ()
modifyLoaderState_ interp f =
  modifyMVar_ (loader_state (interpLoader interp))
    (fmap pure . f . fromMaybe uninitialised)

-- See Note [Unloading vs purging objects] in GHC.Runtime.Interpreter
dropLinkableObjs :: Interp -> [FilePath] -> IO ()
dropLinkableObjs interp objs
  | interpreterDynamic interp = return ()
  | otherwise
  = mapM_ (unloadObj interp) objs

-- | Remove the given modules and every loaded module that transitively
-- refers to them.
-- See Note [Automatically reloading stale linkables]
dropModules :: UniqDSet Module -> LoaderState -> IO LoaderState
dropModules mods pls = do
  let victims = mods
      victim_list = uniqDSetToList victims
      keep_name :: (Name, a) -> Bool
      keep_name (n, _) = not (isExternalName n)
                      || not (nameModule n `elementOfUniqDSet` victims)

  return $! pls
    { bcos_loaded = delModuleEnvList (bcos_loaded pls) victim_list
    , objs_loaded = delModuleEnvList (objs_loaded pls) victim_list
    , linker_env = (linker_env pls)
      { closure_env = filterNameEnv keep_name (closure_env (linker_env pls))
      , itbl_env    = filterNameEnv keep_name (itbl_env (linker_env pls))
      , addr_env    = filterNameEnv keep_name (addr_env (linker_env pls))
      }
    }

-- | Drop the given modules and every loaded module that transitively
-- refers to them. Reloading stale code does not need this, it happens
-- automatically. This is for API clients dropping modules they no
-- longer need. Interactive modules are ignored.
-- See Note [Automatically reloading stale linkables]
-- See Note [Unloading vs purging objects] in GHC.Runtime.Interpreter
unloadModules :: Interp -> HscEnv -> [Module] -> IO ()
unloadModules interp hsc_env mods =
  mask_ do
    initLoaderState interp hsc_env
    modifyLoaderState_ interp $ dropModules (mkUniqDSet (filter (not . isInteractiveModule) mods))
