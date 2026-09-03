module Ghc.Ui.Data.Project where

import Brick.Widgets.List (GenericList, list)
import Data.Map.Strict (Map)
import Data.Sequence (Seq)
import Data.Set qualified as Set
import Data.Set (Set)
import GHC.Generics (Generic)
import Ghc.Ui.Data.Name (Name (..))
import Types.Api (HomeModule, ModuleName, Target, TrackedBytecode, UnitName)

data ProjectUnit =
  ProjectUnit {
    unit :: UnitName,
    modules :: [ModuleName]
  }
  deriving stock (Eq, Show)

data Row =
  Root
  |
  Header { unit :: UnitName, expanded :: Bool, isLast :: Bool }
  |
  ModuleRow { key :: HomeModule, isLast :: Bool, isLastUnit :: Bool }
  deriving stock (Eq, Show)

data ProjectState =
  ProjectState {
    rows :: GenericList Name Seq Row,
    units :: [ProjectUnit],
    expandedUnits :: Set UnitName,
    built :: Set Target,
    failed :: Set Target,
    bco :: Map HomeModule TrackedBytecode
  }
  deriving stock (Generic)

initialState :: ProjectState
initialState =
  ProjectState {
    rows = list Project [] 1,
    units = [],
    expandedUnits = Set.empty,
    built = Set.empty,
    failed = Set.empty,
    bco = []
  }
