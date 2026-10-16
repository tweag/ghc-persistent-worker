module BuckWorkerProto.Data.Protocol where

-- | The gRPC service providing events and API requests.
--
-- Kept apart from the client functions in "BuckWorkerProto.Protocol", since importing @proto-lens@ modules brings
-- @IsLabel@ instances into scope that clash with @generic-lens@.
data Protocol =
  ProtocolGhcServer
  |
  ProtocolInstrument
  deriving stock (Eq, Show)
