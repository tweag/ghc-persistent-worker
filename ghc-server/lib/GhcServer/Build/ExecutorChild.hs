-- | The child side of a persistent executor subprocess (see 'GhcServer.Build.Executor' for the parent side).
--
-- Unlike the one-shot child in 'GhcServer.Build.ProcessChild', a persistent executor keeps a single
-- 'Types.State.WorkerState' for its whole lifetime and serves the @Executor@ gRPC service on a Unix socket, so
-- that the GHC session state (loaded interfaces, bytecode, interpreter) built up by one execute task is reused by
-- the next one dispatched to the same 'Types.Api.ExecutorId'.
--
-- Calls are serialized, since each evaluation captures the process-global stdout\/stderr handles.
--
-- The executor terminates when its stdin reaches EOF: the parent keeps the write end of a pipe open for as long as
-- it considers the executor alive, so the child also exits when the parent dies without terminating it.
module GhcServer.Build.ExecutorChild where

import BuckWorkerProto ()
import Common.Grpc (runGrpcServer)
import Control.Concurrent.Async (race_)
import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import qualified Data.Aeson as Aeson
import Data.ByteString (toStrict)
import Network.GRPC.Common.Protobuf (Proto, defMessage, (&), (.~))
import Network.GRPC.Server.Protobuf (ProtobufMethodsOf)
import Network.GRPC.Server.StreamType (Methods, mkNonStreaming, simpleMethods)
import Proto.Executor (Encoded, Executor)
import qualified Proto.Executor_Fields as Fields
import qualified System.IO as IO
import System.OsPath.Extra (OsPath)
import GhcServer.Build.ProcessChild (configInvalidResult, evalResult)
import Internal.State (newState)
import Types.Settings (defaultSettings)
import Types.State (WorkerState)

-- | Handle one @Execute@ call: decode the 'GhcServer.Data.ProcessEval.ProcessEvalConfig', evaluate it against the
-- executor's persistent state while holding the lock, and encode the 'GhcServer.Data.ProcessEval.ProcessEvalResult'.
executeCall :: MVar () -> MVar WorkerState -> Proto Encoded -> IO (Proto Encoded)
executeCall lock stateVar request = do
  result <- withMVar lock \ () ->
    either configInvalidResult (evalResult stateVar) (Aeson.eitherDecodeStrict' request.payload)
  pure (defMessage & Fields.payload .~ toStrict (Aeson.encode result))

-- | Block until stdin is closed by the parent.
awaitStdinEof :: IO ()
awaitStdinEof =
  IO.hIsEOF IO.stdin >>= \case
    True -> pure ()
    False -> IO.hGetLine IO.stdin >> awaitStdinEof

-- | Entry point of the @executor@ CLI mode: serve the @Executor@ service on the given socket until stdin closes.
runExecutor :: OsPath -> IO ()
runExecutor socket = do
  stateVar <- newState defaultSettings
  lock <- newMVar ()
  race_ awaitStdinEof (runGrpcServer socket (executorMethods lock stateVar))

executorMethods :: MVar () -> MVar WorkerState -> Methods IO (ProtobufMethodsOf Executor)
executorMethods lock stateVar =
  simpleMethods (mkNonStreaming (executeCall lock stateVar))
