-- | LRU tracking and unloading of lazily-loaded bytecode (the @lazyByteCode@ feature, see 'Types.FeatureFlags').
--
-- Loading bytecode from a module's interface's simplified core (see 'Internal.Cache.Hpt.loadCachedByteCode') is
-- deferred until a Template Haskell splice actually needs it (see 'Internal.State.Linkables'). Once loaded, the
-- 'Linkable' stays in the 'HomeModInfo' (and therefore in the HPT) indefinitely unless reclaimed. This module
-- provides that reclamation: every time a splice resolves its dependencies, the touched linkables' cache entries are
-- refreshed ('touchBcoCache'); at the end of a compile job, if the tracked total size exceeds a configured limit, the
-- least-recently-used entries are unloaded ('evictBcoCache').
module Internal.Cache.Bytecode where

import Control.Monad (unless)
import Data.Foldable (traverse_)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Maybe (isNothing)
import GHC.ByteCode.Types (bc_bcos)
import GHC.Data.FlatBag (sizeFlatBag)
import GHC.Driver.Env (HscEnv, hscInterp)
import GHC.Linker.Loader (unload)
import GHC.Linker.Types (Linkable, linkableBCOs, linkableModule)
import GHC.Unit.Home.Graph (HomeUnitEnv (..), UnitEnvGraph (..), unitEnv_lookup_maybe)
import GHC.Unit.Home.ModInfo (HomeModInfo (..), HomeModLinkable (..))
import GHC.Unit.Home.PackageTable (addHomeModInfoToHpt, lookupHpt)
import GHC.Unit.Types (Module, moduleName, moduleUnitId)
import Types.State.Make (BcoCacheEntry (..), MakeState (..))

-- | Approximate the memory footprint of a linkable's bytecode by the number of BCOs (unlinked bindings) it contains.
-- This is a coarse proxy, not an attempt at measuring actual heap usage, but it is sufficient to compare the relative
-- weight of cache entries for LRU eviction, and it corresponds directly to the number of top-level bindings compiled
-- for a module (a size knob controllable in tests via 'Test.Data.Project.ModuleSource.bindings').
linkableBcoCount :: Linkable -> Int
linkableBcoCount lnk = sum [fromIntegral (sizeFlatBag (bc_bcos cbc)) | cbc <- linkableBCOs lnk]

-- | Record or refresh cache-tracking metadata for every linkable that carries bytecode, bumping the shared access
-- counter once. Called after 'Internal.Compat.LinkDeps.getLinkDeps' has resolved the full dependency set for a
-- Template Haskell splice. Linkables without any BCOs (object-only linkables, which can no longer occur for external
-- package dependencies since package-DB bytecode support was removed, see 'Internal.Compat.LinkDeps') are ignored.
touchBcoCache :: [Linkable] -> MakeState -> MakeState
touchBcoCache linkables make =
  make {bcoCache = List.foldl' touch make.bcoCache withBcos, bcoAccessCounter = counter'}
  where
    counter' = make.bcoAccessCounter + 1
    withBcos = filter (not . null . linkableBCOs) linkables
    touch cache lnk =
      Map.insert (linkableModule lnk) BcoCacheEntry {linkable = lnk, lastAccess = counter', size = linkableBcoCount lnk} cache

-- | If the tracked total size of 'MakeState.bcoCache' exceeds the given limit, unload the least-recently-used
-- entries: reset their 'HomeModInfo' in the HPT to have no bytecode (so it can be lazily reconstructed again later),
-- and call GHC's 'GHC.Linker.Loader.unload' with the linkables that are kept, since that function's contract is to
-- unload everything from the interpreter's loader state that isn't passed to it.
--
-- Caveat: the interpreter's loader state may hold linkables unrelated to lazily-loaded dependency bytecode (e.g. from
-- direct interactive evaluation). 'unload' would discard those too, since we can only tell it what to keep, not what
-- to unload. This does not arise in the worker's normal make-mode compilation flow (bytecode is only linked when a
-- Template Haskell splice needs it, which is exactly what this cache tracks), but it means this eviction should not be
-- reused verbatim in a context that also runs interactive GHCi-style evaluation.
evictBcoCache :: HscEnv -> Int -> MakeState -> IO MakeState
evictBcoCache hsc_env limit make
  | totalSize <= limit = pure make
  | otherwise = do
      traverse_ (dropFromHpt make.hug) (Map.toList evicted)
      unload (hscInterp hsc_env) hsc_env (linkable <$> Map.elems kept)
      pure make {bcoCache = kept}
  where
    totalSize = sum (size <$> Map.elems make.bcoCache)
    -- Oldest (least recently used) first.
    sorted = List.sortOn (lastAccess . snd) (Map.toList make.bcoCache)
    evictedList = fst (selectEvictions (totalSize - limit) sorted)
    evicted = Map.fromList evictedList
    kept = Map.difference make.bcoCache evicted

-- | Greedily select the oldest entries to evict until the running deficit (@need@) is covered.
selectEvictions :: Int -> [(Module, BcoCacheEntry)] -> ([(Module, BcoCacheEntry)], Int)
selectEvictions need entries
  | need <= 0 = ([], need)
  | otherwise =
      case entries of
        [] -> ([], need)
        (e@(_, entry) : rest) ->
          let (more, need') = selectEvictions (need - entry.size) rest
          in (e : more, need')

-- | Reset a module's bytecode in the HPT to 'Nothing', if it is currently present, so it is reconstructed again on
-- demand the next time a splice needs it.
dropFromHpt :: UnitEnvGraph HomeUnitEnv -> (Module, BcoCacheEntry) -> IO ()
dropFromHpt graph (m, _) =
  traverse_ update (unitEnv_lookup_maybe (moduleUnitId m) graph)
  where
    update hue = do
      existing <- lookupHpt (homeUnitEnv_hpt hue) (moduleName m)
      traverse_ (reset hue) existing

    reset hue hmi =
      unless (isNothing hmi.hm_linkable.homeMod_bytecode) do
        addHomeModInfoToHpt hmi {hm_linkable = hmi.hm_linkable {homeMod_bytecode = Nothing}} (homeUnitEnv_hpt hue)
