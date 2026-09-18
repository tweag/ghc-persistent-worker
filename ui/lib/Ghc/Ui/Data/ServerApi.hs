module Ghc.Ui.Data.ServerApi where

import Types.Api (Target, TaskKind)
import Types.FeatureFlags (Feature)

-- | Abstraction of gRPC endpoints.
-- This is crucial to keep out of the event handler logic, because the implementation brings instances of @IsLabel@ from
-- @proto-lens@ into scope, which clash with @generic-lens@.
data ServerApi =
  ServerApi {
    triggerTask :: Target -> TaskKind -> IO (),
    evictBytecode :: Target -> IO (),
    clean :: Target -> IO (),
    toggleFeature :: Feature -> IO ()
  }
