module Test.Data.Schedule where

import Data.Coerce (coerce)
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

newtype Dispatch task =
  Dispatch { run :: task -> IO RequestResult }

runDispatch :: Dispatch task -> task -> IO RequestResult
runDispatch = coerce

data ScheduleEnv key task =
  ScheduleEnv {
    maxJobs :: MaxJobs,
    dispatch :: Dispatch task
  }
  deriving stock (Generic)

-- | A node of a pre-sorted 'Schedule', used to track dependency availability.
data ScheduleNode key a =
  ScheduleNode {
    key :: key,
    deps :: Set key,
    value :: a
  }
  deriving stock (Eq, Show)

newtype Schedule key task =
  Schedule { tasks :: [ScheduleNode key task] }
  deriving stock (Eq, Show)
  deriving newtype (Semigroup, Monoid)

-- | Outcome of running a 'Schedule'.
data ScheduleProgress key task =
  ScheduleProgress {
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
