module Grpc where

import BuckWorkerProto (Instrument)
import Control.Concurrent (forkIO)
import Control.Monad (void)
import Data.Text qualified as Text
import Network.GRPC.Client (rpc, Connection)
import System.IO (hPutStrLn, stderr)
import Network.GRPC.Client.StreamType.IO (nonStreaming)
import Network.GRPC.Common.Protobuf (Proto, Protobuf, defMessage, (&), (.~))
import Proto.Instrument qualified as Instr
import Proto.Instrument_Fields qualified as Fields
import Types.State (Options (..))
import Types.Target (TargetSpec, renderTargetSpec)

sendOptions :: Connection -> Options -> IO ()
sendOptions conn options =
  void $ forkIO $ void $
    nonStreaming conn (rpc @(Protobuf Instrument "setOptions")) $
      mkOptions options

mkOptions :: Options -> Proto Instr.Options
mkOptions Options{..} =
  defMessage
    & Fields.extraGhcOptions
    .~ Text.pack extraGhcOptions

-- | Send a raw target-text 'RebuildRequest' to the server. Shared by 'triggerRebuild' (worker flow, target text is
-- 'renderTargetSpec' output) and the instrument UI's 'b'\/'m' keys (ghc-server flow, target text is
-- @unitName:moduleName@ for 'b' or @unitName:metadata@ for 'm', computed by 'UI.TaskTree.selectedCompileTargets'\/
-- 'UI.TaskTree.selectedMetadataTarget').
triggerRebuildText :: Connection -> Text.Text -> IO ()
triggerRebuildText conn targetText =
  void $ forkIO $ void $
    nonStreaming conn (rpc @(Protobuf Instrument "triggerRebuild")) $
      defMessage
        & Fields.target
        .~ targetText

triggerRebuild :: Connection -> TargetSpec -> IO ()
triggerRebuild conn target =
  triggerRebuildText conn (Text.pack (renderTargetSpec target))

-- | Send a bare unit-name target to the server's @triggerExecute@ RPC, requesting execution of @main@ for
-- every module in that unit (in parallel, skipping modules without @main@). Fire-and-forget, mirroring
-- 'triggerRebuildText'.
triggerExecuteText :: Connection -> Text.Text -> IO ()
triggerExecuteText conn targetText = do
  hPutStrLn stderr ("triggerExecuteText: sending target=" ++ Text.unpack targetText)
  void $ forkIO $ void $
    nonStreaming conn (rpc @(Protobuf Instrument "triggerExecute")) $
      defMessage
        & Fields.target
        .~ targetText

-- | Request a snapshot of the lazily-loaded bytecode cache state.
getBytecodeState :: Connection -> IO (Proto Instr.BytecodeState)
getBytecodeState conn =
  nonStreaming conn (rpc @(Protobuf Instrument "getBytecodeState")) defMessage

-- | Request eviction of a module (or, if empty, an entire unit) from the bytecode cache.
evictBytecode :: Connection -> Text.Text -> Text.Text -> IO ()
evictBytecode conn unitId modName =
  void $ forkIO $ void $
    nonStreaming conn (rpc @(Protobuf Instrument "evictBytecode")) $
      defMessage
        & Fields.unitId
        .~ unitId
        & Fields.moduleName
        .~ modName
