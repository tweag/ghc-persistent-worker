{-# LANGUAGE CPP #-}

module Test.Bytecode where

import Control.Concurrent.MVar (modifyMVar_, readMVar)
import Control.Monad.IO.Class (liftIO)
import Data.Functor ((<&>))
import Data.List (isPrefixOf)
import Data.Maybe (mapMaybe)
import GHC (isExternalName, moduleNameFS)
import GHC.Data.FastString (FastString)
import GHC.Runtime.Interpreter.Types (Interp (..))
import GHC.Unit.Module.Env (moduleEnvToList)
import GHC.Unit.Types (GenModule (..), unitFS)
import GHC.Utils.Outputable (showPprUnsafe)
import Hedgehog (TestT, evalMaybe)
import Internal.Compat.Linkables (support_Linkables)
import Test.Data.Env (TestEnv (..))
import qualified Types.Args as Args
import Types.Args (Args (..))
import Types.Env (Env (..))
import Types.FeatureFlags (Feature (..))
import Types.Settings (Settings (..), setFeature)
import qualified Types.State as WorkerState
import Types.State (WorkerState (..))
import Types.State.Make (MakeState (..))

#if defined(GBC)
import GHC.Linker.Types (BytecodeLoaderState (..), BytecodeState (..), LinkerEnv (..), Loader (..), LoaderState (..))
import GHC.Types.Name (nameModule_maybe)
import GHC.Types.Name.Env (nonDetNameEnvElts)
import GHC.Unit.Types (interactiveUnit)
#else
import GHC.ByteCode.Types (bc_bcos, unlinkedBCOName)
import GHC.Data.FlatBag (elemsFlatBag)
import GHC.Linker.Types (Loader (..), LoaderState (..), linkableBCOs)
#endif

enableLazyByteCode :: TestEnv -> TestEnv
enableLazyByteCode testEnv =
  testEnv {
    baseArgs = testEnv.baseArgs {
      Args.settings = setFeature FeatureLazyByteCode support_Linkables testEnv.baseArgs.settings
    }
  }

enableByteCodeCacheLimit :: Int -> TestEnv -> TestEnv
enableByteCodeCacheLimit limit testEnv =
  testEnv {
    baseArgs = testEnv.baseArgs {
      Args.settings = testEnv.baseArgs.settings {lazyByteCodeCacheLimit = Just limit}
    }
  }

-- | Update the bytecode cache size limit on an already-running session's persistent 'WorkerState.settings'.
-- Unlike 'enableByteCodeCacheLimit' (which only affects a 'TestEnv' consulted at session creation, via
-- 'Test.Env.newSessionEnv'), this mutates the live 'MVar WorkerState', mirroring how
-- 'GhcWorker.Grpc' handles the @ToggleFeatureFlag@ RPC. This is required when the desired limit is only known once
-- the session is already running (e.g. derived from a module's tracked cache size).
setByteCodeCacheLimit :: Int -> Env -> IO ()
setByteCodeCacheLimit limit env =
  modifyMVar_ env.state \ (state :: WorkerState) ->
    pure state {WorkerState.settings = state.settings {lazyByteCodeCacheLimit = Just limit}}

envLoader :: Env -> IO (Maybe Loader)
envLoader env = do
  readMVar env.state <&> \ WorkerState {make = MakeState {interp = mb_interp}} ->
    mb_interp <&> \ Interp {interpLoader} -> interpLoader

loadedBcos :: Env -> TestT IO [(FastString, FastString, [String])]
loadedBcos env = do
  Loader lsVar <- evalMaybe =<< liftIO (envLoader env)
#if defined(GBC)
  LoaderState {bcos_loaded, bco_loader_state} <- evalMaybe =<< liftIO (readMVar lsVar)
  let closures = fst <$> nonDetNameEnvElts bco_loader_state.homePackage_loaded.bco_linker_env.closure_env
  -- Splice expressions are loaded as modules in the interactive unit, which aren't relevant here.
  pure [modBcos m (bcoNames m closures) | (m, _) <- moduleEnvToList bcos_loaded, m.moduleUnit /= interactiveUnit]
#else
  LoaderState {bcos_loaded} <- evalMaybe =<< liftIO (readMVar lsVar)
  pure [modBcos m (bcoNames linkable) | (m, linkable) <- moduleEnvToList bcos_loaded]
#endif
  where
    modBcos m ns =
      (unitFS m.moduleUnit, moduleNameFS m.moduleName, mapMaybe interestingName ns)

    interestingName name
      | isExternalName name
      , not (isPrefixOf "$" (showPprUnsafe name))
      = Just (showPprUnsafe name)
      | otherwise
      = Nothing

#if defined(GBC)
    -- The loader state only retains usage info for loaded linkables, so the names of the top-level BCOs are
    -- recovered from the closures the linker registered for the module.
    bcoNames m closures =
      [n | n <- closures, nameModule_maybe n == Just m]
#else
    bcoNames lnk =
      [unlinkedBCOName bco | cbc <- linkableBCOs lnk, bco <- elemsFlatBag (bc_bcos cbc)]
#endif
