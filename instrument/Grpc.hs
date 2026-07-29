module Grpc where

import BuckWorkerProto (Instrument)
import Control.Concurrent (forkIO)
import Control.Monad (void)
import Data.Text qualified as Text
import Network.GRPC.Client (rpc, Connection)
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
-- 'renderTargetSpec' output) and 'triggerBuild' (ghc-server flow, target text is @unitName:metadata@).
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

-- | Trigger a metadata build for the given @ghc-server@ unit, equivalent to running
-- @ghc-client $project unitName:metadata@. Uses 'GhcServer.Handler.parseTarget'\'s target-text syntax, which
-- 'GhcServer.Grpc.triggerRebuild' parses on the server side.
triggerBuild :: Connection -> Text.Text -> IO ()
triggerBuild conn unitName =
  triggerRebuildText conn (unitName <> ":metadata")

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
