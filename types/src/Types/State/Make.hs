{-# LANGUAGE CPP #-}

module Types.State.Make where

import Control.Concurrent.MVar (MVar)
import Data.IntMap qualified as IM
import Data.IntSet qualified as IS
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import GHC (HscEnv, ModuleGraph, ModuleName, emptyMG)
import GHC.Data.Graph.Directed (Node)
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

-- | Maps among unique key, graph idx and graph node content.
-- This is actual content of graph and integerization of nodes
-- for efficient query.
-- In our case, node = ModuleGraphNode
-- Note that Node Int inode = SummaryNode
data KeyIndexNodeMap node = KIN
  { keyIdxMap :: M.Map NodeKey Int,
    idxNodeMap :: IM.IntMap node,
    -- | key to (idx, graph node) pair.
    keyINodeMap :: M.Map NodeKey (Node Int node),
    idxKeyMap :: IM.IntMap NodeKey,
    reachabilityMap :: IM.IntMap IS.IntSet
  }

emptyKINMap :: KeyIndexNodeMap ModuleGraphNode
emptyKINMap = KIN
  { keyIdxMap = M.empty,
    idxNodeMap = IM.empty,
    keyINodeMap = M.empty,
    idxKeyMap = IM.empty,
    reachabilityMap = IM.empty
  }

data EModuleGraph = EModuleGraph
  { moduleGraph :: ModuleGraph,
    keyIndexNodeMap :: KeyIndexNodeMap ModuleGraphNode
  }

emptyEModuleGraph :: EModuleGraph
emptyEModuleGraph = EModuleGraph
  { moduleGraph = emptyMG,
    keyIndexNodeMap = emptyKINMap
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
    moduleGraphState :: EModuleGraph,

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
    bcoAccessCounter :: Int,

    -- | Modules requested for eviction from 'bcoCache' via the instrumentation UI (see 'Internal.Cache.Bytecode.evictSpecific').
    -- Applied and cleared the next time a compile job's session is stored (in 'Internal.State.withState'), since
    -- eviction requires a live 'HscEnv'/'Interp' that isn't available outside of a running session.
    pendingEvictions :: S.Set Module,

    -- | Every module that has ever had an entry in 'bcoCache', including ones since evicted from it. Unlike
    -- 'bcoCache', entries here are never removed, only updated (by 'Internal.Cache.Bytecode.touchBcoCache') when a
    -- module's bytecode is (re-)loaded. Whether a historic entry is currently resident is derived by checking
    -- membership in 'bcoCache', not stored here; this map only preserves the last known size\/access metadata so the
    -- instrumentation UI's bytecode browser can still display evicted modules after they've dropped out of
    -- 'bcoCache'.
    --
    -- TODO can't we just merge this into bcoCache and make the linkable Maybe
    bcoHistory :: M.Map Module BcoHistoryEntry,

    -- | Bytecode made available from an external source (currently: 'GhcServer.Build.SharedBytecode', shared-memory
    -- transfer from a parent process into an execute subprocess) that hasn't yet been consulted for any module.
    -- Consulted by 'Internal.State.Linkables.addLazyByteCode' before falling back to reconstructing bytecode from
    -- Core ('Internal.State.Linkables.lazyLoadByteCode'); a hit rehydrates the entry (the closure captures whatever
    -- source-specific representation the caller built it from, e.g. a mirrored, 'Name'-free linkable) and removes it
    -- from this map, since after insertion into the HPT the module's 'HomeModLinkable.homeMod_bytecode' is no longer
    -- 'Nothing' and will never be looked up here again. Kept as a plain 'GHC.Linker.Types.Linkable'-producing
    -- function (rather than an importer-specific type) so this module doesn't need to depend on whichever package
    -- defines the external source.
    bytecodeImport :: M.Map (UnitId, ModuleName) (HscEnv -> Module -> IO Linkable)
  }

-- | Historic cache-tracking metadata for a module that has (or had) an entry in 'MakeState.bcoCache'. See
-- 'MakeState.bcoHistory'. Unlike 'BcoCacheEntry', this carries no 'Linkable' (it isn't valid once the module has
-- been evicted), only the last known size\/access-time metadata.
data BcoHistoryEntry =
  BcoHistoryEntry {
    lastAccess :: Int,
    size :: Int
  }
