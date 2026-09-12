{-# LANGUAGE DeriveAnyClass #-}

module Types.State where

import Data.Set (Set)
import GHC (HscEnv)
import System.OsPath (OsPath)
import Types.State.Make (MakeState (..))

data BinPath =
  BinPath {
    initial :: Maybe OsPath,
    extra :: Set OsPath
  }
  deriving stock (Eq, Show)

data WorkerState =
  WorkerState {
    path :: BinPath,
    baseSession :: Maybe HscEnv,
    make :: MakeState
  }
