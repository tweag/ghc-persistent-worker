-- | Types for the stdio protocol between the parent @ghc-server@ and a self-relaunched execute-task child
-- process (see 'GhcServer.Build.Process').
--
-- The child writes nothing to its stdio except a single JSON-encoded 'ProcessEvalResult' at the very end;
-- everything the evaluated module printed and everything the child's own logger recorded is carried as fields
-- of that value, so the parent can parse the child's entire stdout as one JSON document and treat any stderr
-- output at all as unexpected.
module GhcServer.Data.ProcessEval where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import GhcServer.Data.Unit (Unit)
import System.OsPath.Extra (OsPath)
import Types.Api (ModuleName, ProcessStats)

data ProcessEvalOptions =
  ProcessEvalOptions {
    configFile :: Maybe OsPath
  }
  deriving stock (Eq, Show)

-- | Everything the child process needs to run the target module's @main@, carried over from the parent's
-- already-resolved 'GhcServer.Data.Unit.Project' rather than rediscovered from disk (the child has no way to
-- know which discovery mechanism -- @unit.json@ or Cabal -- the parent used). Serialized to JSON and passed as
-- a single argv element (kept on the command line rather than sent over a pipe\/socket, since the child's
-- entire lifetime is this one task).
data ProcessEvalConfig =
  ProcessEvalConfig {
    projectRoot :: OsPath,
    unit :: Unit,
    moduleName :: ModuleName,
    -- | Path to a @\/dev\/shm@ file written by 'GhcServer.Build.SharedBytecode.exportSharedBytecode', containing
    -- the parent's already-compiled bytecode for this unit's modules. 'Nothing' if the parent had no bytecode
    -- to share, in which case the child falls back to its usual cache-only reconstruction.
    sharedBytecodePath :: Maybe FilePath
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

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
    -- | RTS memory-usage stats collected via 'GHC.Stats.getRTSStats' right before the child exits, forwarded to
    -- the UI as part of 'Types.Api.Event's @CompileEnd@ ('GhcServer.Build.Process.executeModuleTaskProcess').
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
