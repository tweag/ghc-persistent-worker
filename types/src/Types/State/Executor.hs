-- | The persistent-executor subprocess handle stored in 'Types.State.WorkerState.executors', keyed by
-- 'Types.Api.ExecutorId'. See @kb-process-isolation@ for the rationale behind running execute tasks out of
-- process, and 'GhcServer.Build.Executor' for the parent-side spawn\/dispatch\/terminate logic.
module Types.State.Executor where

import Control.Concurrent.QSemN (QSemN, newQSemN, signalQSemN, waitQSemN)
import Control.Exception (bracket_)
import Data.Aeson (FromJSON, ToJSON)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Time (UTCTime)
import GHC.Fingerprint (Fingerprint)
import GHC.Generics (Generic)
import GHC.Unit.Types (UnitId)
import Language.Haskell.Syntax.Module.Name (ModuleName)
import System.IO (Handle)
import System.OsPath (OsPath)
import System.Process (ProcessHandle)
import Types.Api (ProcessStats)

-- | Identifies a home module across units in executor bytecode plans.
type HomeModuleId = (UnitId, ModuleName)

-- | A home module whose bytecode an executor has linked, as recorded by the parent.
-- The direct dependencies are kept so that replacing a module can invalidate its loaded dependents, which still
-- refer to the replaced module's old closures.
data LoadedModule =
  LoadedModule {
    -- | Content hash of the bytecode payload that was sent.
    hash :: Fingerprint,
    deps :: [HomeModuleId]
  }
  deriving stock (Eq)

-- | The parent's record of the bytecode loaded by one executor.
-- It is part of 'ExecutorHandle', so a respawned executor starts with an empty set.
data ExecutorLoaded =
  ExecutorLoaded {
    modules :: Map HomeModuleId LoadedModule,
    -- | Number of plans sent so far. The child uses it to derive distinct linkable times, since GHC's loader only
    -- relinks a module if its linkable time differs from the loaded one.
    generation :: Int
  }
  deriving stock (Eq)

emptyExecutorLoaded :: ExecutorLoaded
emptyExecutorLoaded = ExecutorLoaded {modules = Map.empty, generation = 0}

-- | Identifies the contents of a file on disk without reading it.
data FileStamp =
  FileStamp {
    modified :: UTCTime,
    size :: Integer
  }
  deriving stock (Eq, Show)

-- | A byte range in a shared payload segment, which contains the serialized bytecode of one module in the format of
-- a @.gbc@ file.
data SegmentSlice =
  SegmentSlice {
    segment :: FilePath,
    offset :: Int,
    size :: Int
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Parent-side state of an append-only payload segment on a @tmpfs@, which executors map into memory.
-- Since the parent only ever appends, the ranges of existing slices are immutable.
data PayloadArena =
  PayloadArena {
    segment :: FilePath,
    -- | Opened for appending.
    handle :: Handle,
    -- | Current size of the segment.
    end :: Int,
    -- | Payloads in the segment, keyed by content hash.
    slices :: Map Fingerprint SegmentSlice
  }
  deriving stock (Eq, Show)

-- | Parent-side cache of bytecode payloads shared by all executors.
data PayloadCache =
  PayloadCache {
    -- | Content hashes of @.gbc@ files, valid as long as the file's stamp is unchanged.
    fileHashes :: Map FilePath (FileStamp, Fingerprint),
    -- | Payload files written by the parent for modules whose bytecode has no @.gbc@ file, keyed by content hash.
    written :: Map Fingerprint FilePath,
    -- | The shared payload segment, created on first use.
    arena :: Maybe PayloadArena
  }
  deriving stock (Eq, Show)

emptyPayloadCache :: PayloadCache
emptyPayloadCache = PayloadCache {fileHashes = Map.empty, written = Map.empty, arena = Nothing}

-- | The number of calls that can hold an executor's 'ExecutorHandle.calls' at the same time.
maxConcurrentCalls :: Int
maxConcurrentCalls = 64

newCallLock :: IO QSemN
newCallLock = newQSemN maxConcurrentCalls

-- | Run a call to the executor, concurrently with other calls, but not while 'withoutCalls' runs.
withCall :: ExecutorHandle -> IO a -> IO a
withCall handle =
  bracket_ (waitQSemN handle.calls 1) (signalQSemN handle.calls 1)

-- | Run an action while no call is in flight and none is started, e.g. to replace the process.
withoutCalls :: ExecutorHandle -> IO a -> IO a
withoutCalls handle =
  bracket_ (waitQSemN handle.calls maxConcurrentCalls) (signalQSemN handle.calls maxConcurrentCalls)

-- | A running persistent executor child process: its OS process handle (for termination), the Unix socket
-- path on which it serves the @Executor@ gRPC service, and the write end of its stdin pipe. The child exits when
-- its stdin reaches EOF, so the handle must be kept alive (and is closed on termination).
data ExecutorHandle =
  ExecutorHandle {
    process :: ProcessHandle,
    socket :: OsPath,
    stdin :: Handle,
    loaded :: ExecutorLoaded,
    -- | RTS stats the executor reported with its most recent result.
    stats :: Maybe ProcessStats,
    -- | Lets calls run concurrently while keeping a respawn from stopping the process during a call, see
    -- 'withCall' and 'withoutCalls'.
    -- Shared by all handles registered for the same 'Types.Api.ExecutorId', including respawned ones.
    calls :: QSemN,
    -- | Counts the processes spawned for the same 'Types.Api.ExecutorId' before this one, so that a respawn can tell
    -- whether the process that failed has already been replaced.
    incarnation :: Int
  }
