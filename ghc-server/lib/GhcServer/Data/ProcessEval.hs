-- | Types for the stdio protocol between the parent @ghc-server@ and a self-relaunched execute-task child
-- process (see 'GhcServer.Build.Process').
--
-- The child writes nothing to its stdio except a single JSON-encoded 'Types.Executor.Protocol.CallResult' at the
-- very end;
-- everything the evaluated module printed and everything the child's own logger recorded is carried as fields
-- of that value, so the parent can parse the child's entire stdout as one JSON document and treat any stderr
-- output at all as unexpected.
module GhcServer.Data.ProcessEval where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import GhcServer.Data.Unit (Unit)
import System.OsPath.Extra (OsPath)
import Types.Api (ModuleName)

data ProcessEvalOptions =
  ProcessEvalOptions {
    configFile :: Maybe OsPath
  }
  deriving stock (Eq, Show)

-- | Everything the child process needs to run the target module's @main@, carried over from the parent's
-- already-resolved 'GhcServer.Data.Unit.Project' rather than rediscovered from disk (the child has no way to
-- know which discovery mechanism -- @unit.json@ or Cabal -- the parent used). Serialized to JSON and sent to the
-- child on stdin (one-shot child) or as the payload of a 'Types.Executor.Protocol.CallModule' (executor).
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
