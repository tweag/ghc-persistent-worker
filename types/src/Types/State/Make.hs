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
import Data.Set (Set)

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
    bcoAccessCounter :: Int,

    -- | Modules requested for eviction from 'bcoCache' via the instrumentation UI (see 'Internal.Cache.Bytecode.evictSpecific').
    -- Applied and cleared the next time a compile job's session is stored (in 'Internal.State.withState'), since
    -- eviction requires a live 'HscEnv'/'Interp' that isn't available outside of a running session.
    pendingEvictions :: Set Module,

    -- | Every module that has ever had an entry in 'bcoCache', including ones since evicted from it. Unlike
    -- 'bcoCache', entries here are never removed, only updated (by 'Internal.Cache.Bytecode.touchBcoCache') when a
    -- module's bytecode is (re-)loaded. Whether a historic entry is currently resident is derived by checking
    -- membership in 'bcoCache', not stored here; this map only preserves the last known size\/access metadata so the
    -- instrumentation UI's bytecode browser can still display evicted modules after they've dropped out of
    -- 'bcoCache'.
    bcoHistory :: M.Map Module BcoHistoryEntry
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

-- | Historic cache-tracking metadata for a module that has (or had) an entry in 'MakeState.bcoCache'. See
-- 'MakeState.bcoHistory'. Unlike 'BcoCacheEntry', this carries no 'Linkable' (it isn't valid once the module has
-- been evicted), only the last known size\/access-time metadata.
data BcoHistoryEntry =
  BcoHistoryEntry {
    lastAccess :: Int,
    size :: Int
  }
