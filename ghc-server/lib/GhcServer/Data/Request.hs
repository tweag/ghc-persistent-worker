-- | Request types for the standalone GHC server build schedule.
module GhcServer.Data.Request where

import GhcServer.Data.Unit (ClientModule, UnitName)

-- | What to build for a unit: metadata, individual modules, or everything.
data UnitRequest =
  -- | Run the metadata step only.
  UnitMetadata
  |
  -- | Compile all modules (skip metadata).
  UnitModulesOnly
  |
  -- | Compile specific modules (skip metadata).
  UnitModules [ClientModule]
  |
  -- | Run metadata and compile all modules.
  UnitAll
  |
  -- | Run metadata, compile, and execute @main@ for all modules.
  UnitExecute
  |
  -- | Run metadata, compile, and execute @main@ for specific modules.
  UnitExecuteModules [ClientModule]
  deriving stock (Show, Eq)

-- | The sequence of build steps requested by the user.
data ScheduleRequest =
  ScheduleRequest {
    steps :: [(UnitName, UnitRequest)],
    -- | Force recompilation of modules even when cached artifacts exist.
    recompile :: Bool,
    -- | Recompute metadata and recompile even when cached.
    rebuild :: Bool
  }
  deriving stock (Show, Eq)

-- | A unit target as computed by 'effectiveRequests'.
--
-- Separates explicit user requests from implicit transitive dependencies.
-- Implicit deps exist solely for ordering and always use 'UnitAll' scope;
-- their request type is fixed by construction rather than computed.
data EffectiveUnit =
  -- | A unit explicitly requested by the user with a specific scope.
  Explicit UnitName UnitRequest
  |
  -- | A transitive dependency added implicitly, always built with 'UnitAll' scope.
  ImplicitDep UnitName
  deriving stock (Show, Eq)

-- | Extract the unit name from an effective unit.
effectiveUnitName :: EffectiveUnit -> UnitName
effectiveUnitName = \case
  Explicit name _ -> name
  ImplicitDep name -> name

-- | Whether a 'UnitRequest' triggers compilation for a unit.
isCompileRequest :: UnitRequest -> Bool
isCompileRequest = \case
  UnitMetadata -> False
  _ -> True
