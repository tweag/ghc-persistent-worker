-- | Request types for the standalone GHC server build schedule.
module GhcServer.Data.Request where

import GhcServer.Data.Unit (ClientModule, Unit (..))
import Types.Api (ExecutorId, UnitName)

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
    rebuild :: Bool,
    -- | Run this request's @execute@ tasks via a persistent gRPC-addressable executor subprocess (see
    -- 'GhcServer.Build.Executor') instead of in-process. 'Nothing' runs in-process.
    executor :: Maybe ExecutorId
  }
  deriving stock (Show, Eq)

-- | Why a unit is part of a build, and at which scope.
--
-- Separates explicit user requests from implicit transitive dependencies.
-- Implicit deps exist solely for ordering and always use 'UnitAll' scope;
-- their request type is fixed by construction rather than computed.
data UnitScope =
  -- | A unit explicitly requested by the user with a specific scope.
  Explicit UnitRequest
  |
  -- | A transitive dependency added implicitly, always built with 'UnitAll' scope.
  ImplicitDep
  deriving stock (Show, Eq)

-- | A unit target as computed by 'GhcServer.Build.Classify.effectiveRequests', paired with the
-- 'Unit' the request resolved to.
--
-- Carrying the 'Unit' rather than its name means the project map is consulted exactly once, at
-- request expansion time, instead of at every downstream step that needs the unit's sources,
-- modules or cache paths.
data EffectiveUnit =
  EffectiveUnit {
    unit :: Unit,
    scope :: UnitScope
  }

-- | The result of expanding a 'ScheduleRequest' against a 'GhcServer.Data.Unit.Project'.
data EffectiveUnits =
  EffectiveUnits {
    -- | Requested and implied units that exist in the project.
    resolved :: [EffectiveUnit],
    -- | Requested unit names with no counterpart in the project.  Retained so that a metadata
    -- task is still created for them and fails with a diagnostic, rather than the request
    -- silently expanding to no work at all.
    unknown :: [UnitName]
  }

-- | Extract the unit name from an effective unit.
effectiveUnitName :: EffectiveUnit -> UnitName
effectiveUnitName eu = eu.unit.name

-- | Whether a 'UnitRequest' triggers compilation for a unit.
isCompileRequest :: UnitRequest -> Bool
isCompileRequest = \case
  UnitMetadata -> False
  _ -> True
