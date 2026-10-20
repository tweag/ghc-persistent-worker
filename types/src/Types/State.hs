module Types.State where

import Data.Map.Strict (Map)
import Data.Set (Set)
import GHC (HscEnv)
import System.OsPath (OsPath)
import Types.Api (ExecutorId)
import Types.Grpc (CommandEnv, RequestArgs)
import Types.Settings (Settings)
import Types.State.Executor (ExecutorHandle)
import Types.State.Make (MakeState (..))
import Types.Target (TargetSpec)

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
    -- the @ToggleFeature@ API request.
    settings :: Settings,
    targetArgs :: Map TargetSpec (CommandEnv, RequestArgs),

    -- | Executor processes evaluate expressions as bytecode.
    executors :: Map ExecutorId ExecutorHandle
  }
