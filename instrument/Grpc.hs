module Grpc where

import BuckWorkerProto (Instrument)
import Control.Concurrent (forkIO)
import Control.Monad (void)
import Data.Text qualified as Text
import Network.GRPC.Client (Connection, rpc)
import Network.GRPC.Client.StreamType.IO (nonStreaming)
import Network.GRPC.Common.Protobuf (Proto, Protobuf, defMessage, (&), (.~))
import Proto.Instrument qualified as Instr
import Proto.Instrument_Fields qualified as Fields
import Types.State (Options (..))

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

-- | Send a raw target-text 'RebuildRequest' to the server, used by the instrument UI's 'b'\/'m'\/'r' keys
-- (ghc-server flow, target text is @unitName:moduleName@ for 'b', @unitName:metadata@ for 'm', computed by
-- 'UI.TaskTree.selectedCompileTargets'\/'UI.TaskTree.selectedMetadataTargets'). @rebuild@ is forwarded verbatim
-- to the server's @rebuild@ request field: 'False' for an ordinary incremental recompile ('b'\/'m'), 'True' for
-- a forced full rebuild ('r') that discards the server's stored source-digest record for the requested scope.
triggerRebuildText :: Connection -> Text.Text -> Bool -> IO ()
triggerRebuildText conn targetText rebuild =
  void $ forkIO $ void $
    nonStreaming conn (rpc @(Protobuf Instrument "triggerRebuild")) $
      defMessage
        & Fields.target
        .~ targetText
        & Fields.rebuild
        .~ rebuild

-- | Send a raw target-text 'RebuildRequest' to the server's @triggerExecute@ RPC, requesting execution of
-- @main@ for a single module, a whole unit, or (via the sentinel target @"*"@) the entire project -- see
-- 'GhcServer.Grpc.triggerExecute' and 'UI.TaskTree.selectedExecuteTarget'. Fire-and-forget, mirroring
-- 'triggerRebuildText'.
triggerExecuteText :: Connection -> Text.Text -> IO ()
triggerExecuteText conn targetText =
  void $ forkIO $ void $
    nonStreaming conn (rpc @(Protobuf Instrument "triggerExecute")) $
      defMessage
        & Fields.target
        .~ targetText

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
