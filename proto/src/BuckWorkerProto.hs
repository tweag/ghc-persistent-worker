{-# OPTIONS_GHC -Wno-orphans #-}

module BuckWorkerProto (module Proto.Worker) where

import Network.GRPC.Common (
  NoMetadata,
  RequestMetadata,
  ResponseInitialMetadata,
  ResponseTrailingMetadata,
 )
import Network.GRPC.Common.Protobuf (Protobuf)
import Proto.Executor (Executor)
import Proto.Worker

type instance RequestMetadata (Protobuf Worker _) = NoMetadata
type instance ResponseInitialMetadata (Protobuf Worker _) = NoMetadata
type instance ResponseTrailingMetadata (Protobuf Worker _) = NoMetadata

-- TODO: Needs a reference to documentation that explains RequestMetadata.
-- The grapesy package where it is defined provides no clue.
type instance RequestMetadata (Protobuf Executor _) = NoMetadata
type instance ResponseInitialMetadata (Protobuf Executor _) = NoMetadata
type instance ResponseTrailingMetadata (Protobuf Executor _) = NoMetadata
