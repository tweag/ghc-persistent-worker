{-# LANGUAGE CPP #-}

module Types.State.Make where

import Control.Concurrent.MVar (MVar)
import GHC (ModuleGraph, ModuleName)
import GHC.Linker.Types (Linkable)
import GHC.Runtime.Interpreter (Interp)
import GHC.Unit.Env (HomeUnitGraph)
import GHC.Unit.Types (Module)
-- import Data.IORef (IORef)
import Data.Map.Strict qualified as M

#if defined(UNIT_INDEX)

import GHC.Unit.State (UnitIndex)

#else

data UnitIndex = UnitIndex

#endif

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

    -- | Tracks lazily-loaded bytecode (see the @lazyByteCode@ feature flag) for LRU-based unloading: for each home
    -- module currently holding reconstructed bytecode, the linkable itself (needed to tell GHC's 'GHC.Linker.Loader.unload'
    -- which linkables to retain), a monotonic last-access counter, and an approximate size.
    bcoCache :: M.Map Module BcoCacheEntry,

    -- | Monotonic counter bumped every time a Template Haskell splice resolves its dependencies; used as the "time"
    -- source for 'bcoCache' entries' 'lastAccess'.
    bcoAccessCounter :: Int
  }

-- | Cache-tracking metadata for a single lazily-loaded bytecode linkable. See 'MakeState.bcoCache'.
data BcoCacheEntry =
  BcoCacheEntry {
    linkable :: Linkable,
    lastAccess :: Int,
    -- | Approximate size, in number of BCOs (unlinked bindings) contained in the linkable. A coarse proxy for memory
    -- footprint, used only to compare relative weight of cache entries for eviction purposes.
    size :: Int
  }
