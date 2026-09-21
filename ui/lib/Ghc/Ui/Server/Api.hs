module Ghc.Ui.Server.Api where

import BuckWorkerProto ()
import Control.Concurrent.Async.Lifted (async, forConcurrently_)
import Control.Monad (void)
import Control.Monad.Catch (catch)
import Control.Monad.Reader (MonadReader (..), ReaderT, liftIO, runReaderT)
import Data.Aeson (FromJSON, ToJSON, eitherDecodeStrict', encode)
import Data.ByteString (toStrict)
import Data.Text qualified as Text
import Data.Text (Text)
import GHC.Generics (Generic)
import Ghc.Ui.Data.ServerApi (ServerApi (..))
import Ghc.Ui.Data.ServerHandlers (ApiConfig (..))
import Ghc.Ui.Server.Monad (ServerEnv, ServerM, logError)
import Internal.Error (nonAsync)
import Network.GRPC.Client (rpc)
import Network.GRPC.Client.StreamType.IO (nonStreaming)
import Network.GRPC.Common.Protobuf (Protobuf, defMessage, (&), (.~))
import Proto.GhcServer (GhcServer)
import Proto.GhcServer_Fields qualified as Fields
import Types.Api (
  ApiRequest (..),
  ApiResponse (..),
  SomeApiRequest (..),
  Target,
  TaskKind (..),
  TaskTrigger (..),
  renderTarget,
  )
import Types.FeatureFlags (Feature)

data RequestEnv =
  RequestEnv {
    config :: ApiConfig,
    server :: ServerEnv
  }
  deriving stock (Generic)

type RequestM a = ReaderT RequestEnv IO a

liftServer :: ServerM a -> RequestM a
liftServer ma = do
  RequestEnv {server} <- ask
  liftIO (runReaderT ma server)

-- | A request can be made synchronously or asynchronously, though it only differs in whether we block.
--
-- HTTP2 uses custom exceptions types, so we have to catch 'SomeException' with 'nonAsync'.
makeRequest ::
  ToJSON a =>
  FromJSON a =>
  ApiRequest a ->
  (ApiResponse a -> ServerM ()) ->
  RequestM ()
makeRequest command handleResponse = do
  RequestEnv {config = ApiConfig {sync}} <- ask
  (if sync then id else void . async) do
    catch send $ nonAsync \ err ->
      liftServer $ logError ("API request failed: " <> err)
  where
    send = do
      RequestEnv {config = ApiConfig {connections}} <- ask
      liftServer $ forConcurrently_ connections \ connection -> do
        output <- liftIO (nonStreaming connection (rpc @(Protobuf GhcServer "api")) message)
        case eitherDecodeStrict' output.payload of
          Left err -> logError ("Failed to decode grpc response: " <> Text.pack err)
          Right (payload) -> handleResponse payload

    message =
      defMessage
      & Fields.payload
      .~ toStrict (encode (SomeApiRequest command))

requestLogError ::
  ToJSON a =>
  FromJSON a =>
  ApiRequest a ->
  Text ->
  RequestM ()
requestLogError request desc =
  makeRequest request \case
    ApiSuccess _ -> pure ()
    ApiFailure err -> logError (desc <> " failed: " <> err)

triggerTask :: Target -> TaskKind -> RequestM ()
triggerTask target task =
  requestLogError (TriggerTask TaskTrigger {..}) ("Build for " <> renderTarget target)

-- | Request eviction of the modules covered by the given 'Target' from the loader state.
evictBytecode :: Target -> RequestM ()
evictBytecode target =
  requestLogError (EvictBytecode target) ("Evicting bytecode for " <> renderTarget target)

clean :: Target -> RequestM ()
clean target =
  makeRequest (Clean target) (logError . errorMessage)
  where
    errorMessage = \case
      ApiFailure err -> "Cleaning " <> rendered <> " failed: " <> err
      ApiSuccess _ -> "Cleaned " <> rendered

    rendered = renderTarget target

-- | Request that a single feature flag be toggled on the connected server(s).
toggleFeature :: Feature -> RequestM ()
toggleFeature feature =
  requestLogError (ToggleFeature feature) ("Toggling feature " <> Text.pack (show feature))

serverApi :: ServerEnv -> ApiConfig -> ServerApi
serverApi server config =
  ServerApi {
    triggerTask = \ t k -> run (triggerTask t k),
    evictBytecode = run . evictBytecode,
    clean = run . clean,
    toggleFeature = run . toggleFeature
  }
  where
    run = flip runReaderT RequestEnv {..}
