{-# LANGUAGE NoFieldSelectors #-}

module Types.Api where

import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.=))
import Data.Binary (Binary)
import Data.Fixed (Pico)
import Data.Map (Map)
import Data.String (IsString)
import qualified Data.Text as Text
import Data.Text (Text, unpack)
import qualified GHC
import GHC.Generics (Generic)
import GHC.Unit (Module, UnitId, mkModuleName, moduleName, moduleNameString, moduleUnitId, stringToUnitId, unitIdString)
import Types.FeatureFlags (Feature)
import qualified Types.Target as Worker
import Types.Target (ModuleTarget (..), TargetSpec, UnitTarget (..))

newtype UnitName =
  UnitName { text :: Text }
  deriving stock (Eq, Show)
  deriving newtype (IsString, Ord, Binary, FromJSON, ToJSON)

fromUnitId :: UnitId -> UnitName
fromUnitId name =
  UnitName (Text.pack (unitIdString name))

toUnitId :: UnitName -> UnitId
toUnitId name =
  stringToUnitId (Text.unpack name.text)

newtype ModuleName =
  ModuleName { text :: Text }
  deriving stock (Eq, Show)
  deriving newtype (IsString, Ord, Binary, FromJSON, ToJSON)

-- | Identifier for a persistent execute-task subprocess (see 'Types.State.WorkerState.executors' and
-- 'GhcServer.Build.Executor'), specified by the client/UI when triggering an execute task. The @ghc-ui@ default
-- is the target unit's name (see 'Ghc.Ui.Event.Main'), but any text is accepted, allowing multiple executors per
-- unit or one executor shared across units.
newtype ExecutorId =
  ExecutorId { text :: Text }
  deriving stock (Eq, Ord, Show)
  deriving newtype (IsString, Binary, FromJSON, ToJSON)

fromGhcModuleName :: GHC.ModuleName -> ModuleName
fromGhcModuleName name =
  ModuleName (Text.pack (moduleNameString name))

toGhcModuleName :: ModuleName -> GHC.ModuleName
toGhcModuleName name =
  mkModuleName (Text.unpack name.text)

data HomeModule =
  HomeModule {
    unit :: UnitName,
    name :: ModuleName
  }
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass (Binary, FromJSON, ToJSON)

homeModuleFromGhc :: Module -> HomeModule
homeModuleFromGhc module_ =
  HomeModule {
    unit = fromUnitId (moduleUnitId module_),
    name = fromGhcModuleName (moduleName module_)
  }

data UnitSummary =
  UnitSummary {
    name :: UnitName,
    modules :: [ModuleName]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)

-- | Metadata for tracking the loaded bytecode of a module.
data TrackedBytecode =
  TrackedBytecode {
    key :: HomeModule,
    size :: Int,
    lastAccess :: Int,
    resident :: Bool,
    pendingEviction :: Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary, FromJSON, ToJSON)

-- | Specification for selecting build targets by names, especially from the UI.
data Target =
  TargetProject
  |
  TargetUnit { name :: UnitName }
  |
  TargetModule { key :: HomeModule }
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass (Binary, FromJSON, ToJSON)

homeModuleMatchTarget :: HomeModule -> Target -> Bool
homeModuleMatchTarget candidate = \case
  TargetProject -> True
  TargetUnit {name} -> candidate.unit == name
  TargetModule {key} -> candidate == key

renderTarget :: Target -> Text
renderTarget = \case
  TargetProject -> "the project"
  TargetUnit {name = UnitName name} -> name
  TargetModule {key = HomeModule {unit = UnitName unit, name = ModuleName name}} -> unit <> ":" <> name

-- | Whether the second target is included in the first.
targetContains :: Target -> Target -> Bool
targetContains = \cases
  TargetProject _ -> True
  TargetUnit {name} TargetUnit {name = candidate} -> name == candidate
  TargetUnit {name} TargetModule {key = candidate} -> name == candidate.unit
  reference candidate -> reference == candidate

targetFromWorkerSpec :: TargetSpec -> Maybe Target
targetFromWorkerSpec = \case
  Worker.TargetModule ModuleTarget {module_} -> Just TargetModule {key = homeModuleFromGhc module_}
  Worker.TargetModuleInterp ModuleTarget {module_} -> Just TargetModule {key = homeModuleFromGhc module_}
  Worker.TargetUnit UnitTarget {unit} -> Just TargetUnit {name = UnitName (Text.pack (unitIdString unit))}
  _ -> Nothing

data TaskKind =
  Metadata
  |
  Build { rebuild :: Bool }
  |
  Execute
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary, FromJSON, ToJSON)

data TaskTrigger =
  TaskTrigger {
    target :: Target,
    task :: TaskKind
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary, FromJSON, ToJSON)

data ApiRequest a where
  TriggerTask :: { trigger :: TaskTrigger } -> ApiRequest ()
  EvictBytecode :: { target :: Target } -> ApiRequest ()
  Clean :: { target :: Target } -> ApiRequest ()
  -- | Toggle a single 'Feature', sent by the @ghc-ui@ feature-flags panel when a checkbox is toggled.
  ToggleFeature :: { feature :: Feature } -> ApiRequest ()

deriving stock instance Eq (ApiRequest a)
deriving stock instance Show (ApiRequest a)

data SomeApiRequest where
  SomeApiRequest :: ToJSON a => ApiRequest a -> SomeApiRequest

instance ToJSON SomeApiRequest where
  toJSON (SomeApiRequest req) = case req of
    TriggerTask {trigger} ->
      tagged "TriggerTask" ["trigger" .= toJSON trigger]
    EvictBytecode {target} ->
      tagged "EvictBytecode" ["target" .= toJSON target]
    Clean {target} ->
      tagged "Clean" ["target" .= toJSON target]
    ToggleFeature {feature} ->
      tagged "ToggleFeatureFlag" ["feature" .= toJSON feature]
    where
      tagged (tag :: Text) fields = object $ ("tag" .= tag) : fields

instance FromJSON SomeApiRequest where
  parseJSON =
    withObject "ApiRequest" \ o ->
      o .: "tag" >>= \case
        ("TriggerTask" :: Text) -> do
          trigger <- o .: "trigger"
          pure (SomeApiRequest TriggerTask {trigger})
        "EvictByteCode" -> do
          target <- o .: "target"
          pure (SomeApiRequest EvictBytecode {target})
        "Clean" -> do
          target <- o .: "target"
          pure (SomeApiRequest Clean {target})
        "ToggleFeatureFlag" -> do
          feature <- o .: "feature"
          pure (SomeApiRequest ToggleFeature {feature})
        tag ->
          fail (unpack ("Invalid tag: " <> tag))

data ApiResponse a =
  ApiSuccess { payload :: a }
  |
  ApiFailure { message :: Text }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

newtype RequestId =
  RequestId { number :: Int }
  deriving stock (Eq, Show)
  deriving newtype (Num, Real, Enum, Integral, Ord, FromJSON, ToJSON, Binary)

-- | Events emitted during task execution, to be consumed by clients like the UI.
data Event =
  CompileStart {
    target :: Target,
    debuggable :: Bool,
    requestId :: RequestId
  }
  |
  CompileEnd {
    target :: Target,
    exitCode :: Int,
    stderr :: String,
    requestId :: RequestId
  }
  |
  Stats {
    memory :: Map Text Int,
    cpuNs :: Int,
    gcCpuNs :: Int
  }
  |
  PhaseStart {
    target :: Target,
    phase :: String,
    requestId :: RequestId
  }
  |
  PhaseEnd {
    target :: Target,
    durationMs :: Word,
    requestId :: RequestId
  }
  |
  -- | The project structure at the point when a client connects.
  ProjectStructure { units :: [UnitSummary] }
  |
  -- | Sent when bytecode in the loader state was accessed.
  BytecodeSnapshot { entries :: [TrackedBytecode] }
  |
  LogMessage {
    category :: String,
    level :: String,
    message :: String,
    time :: Pico
  }
  |
  -- | Indicates that all scheduled tasks have concluded.
  RequestCompleted { statusMessage :: Text }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)

data Events =
  Events {
    publish :: (RequestId -> Event) -> IO ()
  }

eventsNoop :: Events
eventsNoop =
  Events {
    publish = \ _ -> pure ()
  }

eventsForRequest :: RequestId -> (Event -> IO ()) -> Events
eventsForRequest requestId publish =
  Events {
    publish = \ event -> publish (event requestId)
  }
