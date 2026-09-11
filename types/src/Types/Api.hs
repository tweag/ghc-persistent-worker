{-# LANGUAGE NoFieldSelectors #-}

module Types.Api where

import Data.Aeson (FromJSON, ToJSON)
import Data.Binary (Binary)
import Data.Map (Map)
import Data.String (IsString)
import qualified Data.Text as Text
import Data.Text (Text)
import qualified GHC
import GHC.Generics (Generic)
import GHC.Unit (Module, mkModuleName, moduleName, moduleNameString, moduleUnitId, unitIdString)

newtype UnitName =
  UnitName { text :: Text }
  deriving stock (Eq, Show)
  deriving newtype (IsString, Ord, Binary, FromJSON, ToJSON)

newtype ModuleName =
  ModuleName { text :: Text }
  deriving stock (Eq, Show)
  deriving newtype (IsString, Ord, Binary, FromJSON, ToJSON)

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
homeModuleFromGhc m =
  HomeModule {
    unit = UnitName (Text.pack (unitIdString (moduleUnitId m))),
    name = fromGhcModuleName (moduleName m)
  }

data UnitSummary =
  UnitSummary {
    name :: UnitName,
    modules :: [ModuleName]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)

data Event
  = CompileStart { target :: String, canDebug :: Bool }
  | CompileEnd { target :: String, exitCode :: Int, stderr :: String }
  | Stats { memory :: Map String Int, cpuNs :: Int, gcCpuNs :: Int }
  | Halt
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Binary)
