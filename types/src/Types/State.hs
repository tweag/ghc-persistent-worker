{-# LANGUAGE DeriveAnyClass #-}

module Types.State where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import GHC (HscEnv)
import System.OsPath (OsPath)
import Types.Api (ExecutorId)
import Types.Settings (Settings)
import Types.State.Executor (ExecutorHandle)
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
    make :: MakeState,

    -- | Runtime feature flags, initialized from the worker's CLI options/build config and mutable at runtime via
    -- the @ToggleFeatureFlag@ API request.
    settings :: Settings,

    -- | Persistent execute-task subprocesses, keyed by the client-specified 'ExecutorId'. Populated lazily by
    -- 'GhcServer.Build.Executor.ensureExecutor' the first time a given id is dispatched to, and reused for every
    -- subsequent execute task naming the same id, until explicitly terminated (@TerminateExecutor@ API request).
    executors :: Map ExecutorId ExecutorHandle
  }

-- | The empty executor map, for constructing a fresh 'WorkerState'.
emptyExecutors :: Map ExecutorId ExecutorHandle
emptyExecutors = Map.empty
