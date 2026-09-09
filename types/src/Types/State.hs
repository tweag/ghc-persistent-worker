module Types.State where

import Control.Concurrent.MVar (MVar)
import Data.Map.Strict (Map)
import Data.Set (Set)
import GHC (HscEnv, ModuleName)
import GHC.Unit.Home.ModInfo (HomeModInfo)
import GHC.Unit.Types (UnitId)
import Types.Grpc (CommandEnv, RequestArgs)
import Types.State.Make (MakeState (..))
import Types.Target (TargetSpec)
import System.OsPath (OsPath)

data BinPath =
  BinPath {
    initial :: Maybe OsPath,
    extra :: Set OsPath
  }
  deriving stock (Eq, Show)

data Options =
  Options {
    extraGhcOptions :: String
  }

defaultOptions :: Options
defaultOptions =
  Options {
    extraGhcOptions = ""
  }

data WorkerState =
  WorkerState {
    path :: BinPath,
    baseSession :: Maybe HscEnv,
    options :: Options,
    make :: MakeState,
    targetArgs :: Map TargetSpec (CommandEnv, RequestArgs),
    loadingModInfos :: MVar (Map (UnitId, ModuleName) (IO (Maybe HomeModInfo)))
  }
