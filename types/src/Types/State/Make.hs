{-# LANGUAGE CPP #-}

module Types.State.Make where

import Control.Concurrent.MVar (MVar)
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import GHC (ModuleGraph, ModuleName)
import GHC.Linker.Types (Linkable)
import GHC.Runtime.Interpreter (Interp)
import GHC.Unit.Env (HomeUnitGraph)
import GHC.Unit.Module.Graph (ModuleGraphNode, NodeKey)
import GHC.Unit.Types (Module, UnitId)

#if defined(UNIT_INDEX)

import GHC.Unit.State (UnitIndex)

#else

data UnitIndex = UnitIndex

#endif

type LibName = String

-- | Currently requested and loaded dynamic libraries
--   which are being loaded via direct loadDLL calls.
--   Loaded libraries are tracked and loading is done only once.
data LibLoadState =
  LibLoadState {
    requested :: M.Map UnitId ([FilePath], [LibName]),
    loaded :: S.Set LibName
  }

emptyLibLoadState :: LibLoadState
emptyLibLoadState = LibLoadState
  { requested = M.empty,
    loaded = S.empty
  }

-- | Loader state tracking metadata for a single linkable.
data BcoCacheEntry =
  BcoCacheEntry {
    linkable :: Linkable,
    -- | A measure for the age of this entry, representing the value of 'bcoAccessCounter' at the time it was last used.
    lastAccess :: Int,
    -- | The number of BCOs contained in the linkable, used as a proxy for size.
    size :: Int
  }

-- | Data extracted from 'HscEnv' for the purpose of persisting it across sessions.
--
-- While many parts of the session are either contained in mutable variables or trivially reinitialized, some components
-- must be handled explicitly: The module graph and home unit graph are pure fields that need to be shared, and the
-- interpreter state for TH execution is only initialized when the flags are parsed.
data MakeState =
  MakeState {
    -- | The module graph for a specific unit is computed in its metadata step, after which it's extracted and merged
    -- into the existing graph.
    moduleGraph :: ModuleGraph,

    -- | moduleGraph nodes indexed by NodeKey.
    moduleGraphNodes :: M.Map NodeKey ModuleGraphNode,

    -- | The unit environment for a specific unit is inserted into the shared home unit graph at the beginning of the
    -- metadata step, constructed from the dependency specifications provided by Buck.
    -- After compilation of a module, its 'HomeUnitInfo' is inserted into the home package table contained in its unit's
    -- unit environment.
    hug :: HomeUnitGraph,

    -- | While the interpreter state contains a mutable variable that would be shared across sessions, it isn't
    -- initialized properly until the first module compilation's flags have been parsed, so we store it in the shared
    -- state for consistency.
    interp :: Maybe Interp,

    unitIndex :: UnitIndex,

    bcoLoadState :: M.Map ModuleName (MVar ()),

    -- | Unit-level extra native library dependencies are loaded by checking in LibLoadState explicitly.
    extraLib :: LibLoadState,

    -- | Track loader state entries for unloading.
    bcoCache :: M.Map Module BcoCacheEntry,

    -- | Monotonic counter bumped every time bytecode dependencies are loaded, used as the age of loader state entries.
    bcoAccessCounter :: Int
  }
