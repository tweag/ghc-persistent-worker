{-# LANGUAGE DeriveAnyClass #-}
module Types.State where

import Data.Aeson (FromJSON, ToJSON)
import Data.Map.Strict (Map)
import Data.Set (Set)
import GHC (HscEnv)
import GHC.Generics (Generic)
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
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

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
    targetArgs :: Map TargetSpec (CommandEnv, RequestArgs)
  }
