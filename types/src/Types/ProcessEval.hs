-- | Result types of evaluations in subprocesses: the one-shot execute-task children of the @ghc-server@ and the
-- persistent executors (see 'Types.State.Executor.ExecutorHandle'), which are also used by the worker's eval mode.
module Types.ProcessEval where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Types.Api (ProcessStats)

-- | The outcome of the child's attempt to run one module's @main@, covering both the setup steps that precede
-- evaluation and the evaluation itself.
data EvalOutcome =
  -- | @main@ ran to completion, optionally carrying the value the target function exfiltrated (see
  -- 'Internal.Evaluate.executeMain').
  EvalSuccess (Maybe Text)
  |
  -- | The module has no @main@ binding; the scheduler treats this as a skip rather than a failure.
  EvalNoMain
  |
  -- | Evaluation was attempted and failed (session setup, a precondition, or @main@ itself).
  EvalExecuteFailed Text
  |
  -- | The JSON config passed in argv could not be decoded.
  EvalConfigInvalid Text
  |
  -- | The unit's cache file exists but could not be read.
  EvalCacheUnreadable Text
  |
  -- | The unit has no cache on disk, so its module map cannot be restored.
  EvalCacheMissing
  |
  -- | A persistent executor could not decode or link the bytecode of a 'Types.ExecutorPlan.PlanRequest'.
  -- Its loaded state is unknown afterwards, so the parent forgets what it recorded as loaded.
  EvalLoadFailed Text
  |
  -- | The executor's plan session was started with GHC options different from the request's. Nothing was linked,
  -- but the executor can't serve the request, so the parent respawns it.
  EvalOptionsMismatch Text
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | The child's sole output, written to stdout as JSON right before it exits.
data ProcessEvalResult =
  ProcessEvalResult {
    -- | What the evaluated module wrote to stdout, captured with 'System.IO.Silently.hCapture'.
    evalStdout :: Text,
    -- | What the evaluated module (and GHC) wrote to stderr, captured with 'System.IO.Silently.hCapture'.
    evalStderr :: Text,
    -- | The messages the child's in-memory logger accumulated.
    logMessages :: [Text],
    outcome :: EvalOutcome,
    -- | RTS memory-usage stats collected via 'GHC.Stats.getRTSStats' right before the child exits.
    stats :: ProcessStats
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | The parent's view of a finished child process: the decoded result (or a description of why the child's
-- output could not be interpreted) plus whatever the child wrote to stderr, which is expected to be empty.
data ProcessEvalOutput =
  ProcessEvalOutput {
    result :: Either Text ProcessEvalResult,
    processStderr :: Text
  }
  deriving stock (Show)
