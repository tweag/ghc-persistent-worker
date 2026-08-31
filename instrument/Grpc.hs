module Grpc where

import BuckWorkerProto (Instrument)
import Control.Concurrent (forkIO)
import Control.Monad (void)
import Data.Aeson (encode)
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as Text
import Network.GRPC.Client (Connection, rpc)
import Network.GRPC.Client.StreamType.IO (nonStreaming)
import Network.GRPC.Common.Protobuf (Protobuf, defMessage, (&), (.~))
import Proto.Instrument_Fields qualified as Fields
import Types.Instrument (Command (..), EvictRequest (..), TaskKind (..), TaskTrigger (..))
import Types.State (Options)

-- | Send a 'Command' to the server via the unified @Send@ RPC, JSON-encoded into the 'Instr.Command' message's
-- @payload@ field. Fire-and-forget: does not wait for or decode the 'Instr.CommandResponse'.
sendCommand :: Connection -> Command -> IO ()
sendCommand conn command =
  void $ forkIO $ void $
    nonStreaming conn (rpc @(Protobuf Instrument "send")) $
      defMessage
        & Fields.payload
        .~ LBS.toStrict (encode command)

sendOptions :: Connection -> Options -> IO ()
sendOptions conn = sendCommand conn . SetOptions

-- | Send a raw target-text rebuild task to the server, used by the instrument UI's 'b'\/'m'\/'r' keys
-- (ghc-server flow, target text is @unitName:moduleName@ for 'b', @unitName:metadata@ for 'm', computed by
-- 'UI.TaskTree.selectedCompileTargets'\/'UI.TaskTree.selectedMetadataTargets'). @rebuild@ is forwarded verbatim
-- to the server's @rebuild@ request field: 'False' for an ordinary incremental recompile ('b'\/'m'), 'True' for
-- a forced full rebuild ('r') that discards the server's stored source-digest record for the requested scope.
triggerRebuildText :: Connection -> Text.Text -> Bool -> IO ()
triggerRebuildText conn targetText rebuild =
  sendCommand conn (TriggerTask (TaskTrigger (Text.unpack targetText) Rebuild rebuild))

-- | Send a raw target-text execute task to the server, requesting execution of @main@ for a single module, a
-- whole unit, or (via the sentinel target @"*"@) the entire project -- see 'GhcServer.Grpc.triggerTask' and
-- 'UI.TaskTree.selectedExecuteTarget'. Fire-and-forget, mirroring 'triggerRebuildText'.
triggerExecuteText :: Connection -> Text.Text -> IO ()
triggerExecuteText conn targetText =
  sendCommand conn (TriggerTask (TaskTrigger (Text.unpack targetText) Execute False))

-- | Request eviction of a module (or, if empty, an entire unit) from the bytecode cache.
evictBytecode :: Connection -> Text.Text -> Text.Text -> IO ()
evictBytecode conn unitId modName =
  sendCommand conn (EvictBytecode (EvictRequest (Text.unpack unitId) (Text.unpack modName)))
