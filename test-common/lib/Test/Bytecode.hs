module Test.Bytecode where

import Control.Concurrent.MVar (readMVar)
import Control.Monad.IO.Class (liftIO)
import Data.List (isPrefixOf)
import Data.Maybe (mapMaybe)
import GHC (isExternalName, moduleNameFS)
import GHC.ByteCode.Types (bc_bcos, unlinkedBCOName)
import GHC.Data.FastString (FastString)
import GHC.Data.FlatBag (elemsFlatBag)
import GHC.Linker.Types (Loader (..), LoaderState (..), linkableBCOs)
import GHC.Runtime.Interpreter.Types (Interp (..))
import GHC.Unit.Module.Env (moduleEnvToList)
import GHC.Unit.Types (GenModule (..), unitFS)
import GHC.Utils.Outputable (showPprUnsafe)
import Hedgehog (TestT, evalMaybe)
import Test.Data.Env (TestEnv (..))
import Types.Args (Args (..))
import Types.Env (Env (..))
import Types.FeatureFlags (FeatureFlags (..))
import Types.State (WorkerState (..))
import Types.State.Make (MakeState (..))

enableLazyByteCode :: TestEnv -> TestEnv
enableLazyByteCode testEnv =
  testEnv {
    baseArgs = testEnv.baseArgs {
      features = testEnv.baseArgs.features {lazyByteCode = True}
    }
  }

loadedBcos :: Env -> TestT IO [(FastString, FastString, [String])]
loadedBcos env = do
  WorkerState {make = MakeState {interp = mb_interp}} <- liftIO $ readMVar env.state
  Interp {interpLoader = Loader lsVar} <- evalMaybe mb_interp
  LoaderState {bcos_loaded} <- evalMaybe =<< liftIO (readMVar lsVar)
  pure [modBcos m (bcoNames linkable) | (m, linkable) <- moduleEnvToList bcos_loaded]
  where
    modBcos m ns =
      (unitFS m.moduleUnit, moduleNameFS m.moduleName, mapMaybe interestingName ns)

    interestingName name
      | isExternalName name
      , not (isPrefixOf "$" (showPprUnsafe name))
      = Just (showPprUnsafe name)
      | otherwise
      = Nothing

    bcoNames lnk =
      [unlinkedBCOName bco | cbc <- linkableBCOs lnk, bco <- elemsFlatBag (bc_bcos cbc)]
