{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Ghc.Ui.Monad where

import Brick (EventM)
import Control.Lens (use)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (ReaderT, ask)
import Control.Monad.State (MonadState)
import Data.Kind (Constraint, Type)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (maybeToList)
import GHC.Records (HasField (..))
import GHC.TypeLits (Symbol)
import Ghc.Ui.Data.Main (MainState, currentSession)
import Ghc.Ui.Data.Name (Name)
import Ghc.Ui.Data.ServerApi (ServerApi)
import Ghc.Ui.Data.ServerHandlers (ApiConfig (..), ServerHandlers (..))
import Ghc.Ui.Data.Session (Worker (..))
import Ghc.Ui.Data.WorkerId (WorkerId)

type UiM s a = ReaderT ServerHandlers (EventM Name s) a

type MainM a = UiM MainState a

type MonadUi m = (MonadIO m, MonadState MainState m)

selectWorkers :: Maybe WorkerId -> Map WorkerId Worker -> [Worker]
selectWorkers = \case
  Just target -> maybeToList . Map.lookup target
  Nothing -> Map.elems

apiFor ::
  Maybe WorkerId ->
  Bool ->
  MainM ServerApi
apiFor workerSpec sync = do
  ServerHandlers {api} <- ask
  workers <- selectWorkers workerSpec <$> use (currentSession . #workers)
  pure $ api ApiConfig {connections = [w.connection | w <- workers], sync}

withApi :: (ServerApi -> IO a) -> MainM a
withApi f =
  liftIO . f =<< apiFor Nothing False

withApiSync :: (ServerApi -> IO a) -> MainM a
withApiSync f =
  liftIO . f =<< apiFor Nothing True

-- | This is just a fun little syntax hack that allows writing @server.start@ instead of:
-- > f = do
-- >   server <- ask
-- >   liftIO server.start
data ServerProxy = ServerProxy

type CallProxy :: Symbol -> Type -> Type -> Constraint
class CallProxy name a b | a -> b where
  callProxy :: (ServerHandlers -> a) -> b

instance CallProxy name (IO ()) (MainM ()) where
  callProxy field = do
    handlers <- ask
    liftIO (field handlers)

instance CallProxy name (a -> IO ()) (a -> MainM ()) where
  callProxy field a = do
    handlers <- ask
    liftIO (field handlers a)

instance (
    HasField name ServerHandlers a,
    CallProxy name a b
  ) => HasField name ServerProxy b where
    getField ServerProxy =
      callProxy @name @a @b (getField @name)

server :: ServerProxy
server = ServerProxy
