module Test.Data.Scheduler where

import Data.Map.Strict (Map)
import Data.Set (Set)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import Test.Data.Env (MaxJobs)

data RequestFailure =
  -- | Diagnostic codes that were not in the expected set for this module.
  UnexpectedDiagnostics (Set Natural)
  |
  -- | An exception escaped the GHC diagnostics layer.
  RequestFatal String
  |
  -- | Compilation failed, but all emitted diagnostic codes matched the 'ModuleKey' error variant.
  ExpectedFailure
  deriving stock (Eq, Show)

data RequestResult =
  RequestSuccess
  |
  RequestFailure RequestFailure
  deriving stock (Eq, Show)

data SchedulerEnv key task =
  SchedulerEnv {
    maxJobs :: MaxJobs,
    dispatch :: task -> IO RequestResult
  }
  deriving stock (Generic)

-- | An build task used in a schedule, used to track dependency availability.
data Task key a =
  Task {
    key :: key,
    deps :: Set key,
    value :: a
  }
  deriving stock (Eq, Show)

newtype Schedule key task =
  Schedule { tasks :: [Task key task] }
  deriving stock (Eq, Show)
  deriving newtype (Semigroup, Monoid)

data SchedulerState key task =
  SchedulerState {
    schedule :: Schedule key task,
    completed :: Set key,
    failures :: Map key RequestFailure
  }
  deriving stock (Generic)

unexpectedFailure :: RequestFailure -> Bool
unexpectedFailure = \case
  UnexpectedDiagnostics _ -> True
  RequestFatal _ -> True
  ExpectedFailure -> False
