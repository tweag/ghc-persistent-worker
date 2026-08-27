-- | LRU tracking and unloading of lazily-loaded bytecode (the @lazyByteCode@ feature, see 'Types.FeatureFlags').
module Internal.Cache.Bytecode where

import Data.Bifunctor (bimap)
import Data.Foldable (foldr', for_, traverse_)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Set (Set)
import GHC.ByteCode.Types (bc_bcos)
import GHC.Data.FlatBag (sizeFlatBag)
import GHC.Driver.Env (HscEnv, hscInterp)
import GHC.Linker.Types (Linkable (..), linkableBCOs, linkableModule)
import GHC.Unit.Home.Graph (HomeUnitEnv (..), UnitEnvGraph (..), unitEnv_lookup_maybe)
import GHC.Unit.Home.ModInfo (HomeModInfo (..), HomeModLinkable (..))
import GHC.Unit.Home.PackageTable (addHomeModInfoToHpt, lookupHpt)
import GHC.Unit.Types (Module, moduleName, moduleUnitId)
import Internal.Compat.Unload (unloadModules)
import Types.State.Make (BcoCacheEntry (..), BcoHistoryEntry (..), MakeState (..))

-- | Count the number of BCOs in a 'Linkable' as a coarse proxy measure for memory usage.
-- Note: this ignores @LazyBCOs@.
linkableBcoCount :: Linkable -> Int
linkableBcoCount lnk =
  sum (fromIntegral . sizeFlatBag . bc_bcos <$> linkableBCOs lnk)

-- | Increase the access counter and update the LRU cache entries for each 'Linkable'. Also refreshes the
-- corresponding 'MakeState.bcoHistory' entries, which (unlike 'MakeState.bcoCache') are never removed, so the
-- instrumentation UI's bytecode browser can still display modules after they've been evicted.
touchBcoCache :: [Linkable] -> MakeState -> MakeState
touchBcoCache linkables make =
  make {
    bcoCache = foldr' touch make.bcoCache withBcos,
    bcoHistory = foldr' touchHistory make.bcoHistory withBcos,
    bcoAccessCounter
  }
  where
    withBcos = filter (not . null . linkableBCOs) linkables

    touch linkable = Map.insert linkable.linkableModule (newEntry linkable)

    newEntry linkable = BcoCacheEntry {linkable, lastAccess = bcoAccessCounter, size = linkableBcoCount linkable}

    touchHistory linkable = Map.insert linkable.linkableModule (newHistEntry linkable)

    newHistEntry linkable = BcoHistoryEntry {lastAccess = bcoAccessCounter, size = linkableBcoCount linkable}

    bcoAccessCounter = make.bcoAccessCounter + 1

-- | Clear the bytecode field of a module's HPT entry.
dropFromHpt :: UnitEnvGraph HomeUnitEnv -> Module -> IO ()
dropFromHpt hug target =
  for_ unitEnv \ hue -> do
    lookupHpt (homeUnitEnv_hpt hue) (moduleName target) >>= traverse_ \ hmi ->
      addHomeModInfoToHpt (clear hmi) (homeUnitEnv_hpt hue)
  where
    clear hmi = hmi {hm_linkable = hmi.hm_linkable {homeMod_bytecode = Nothing}}

    unitEnv = unitEnv_lookup_maybe (moduleUnitId target) hug

-- | Break the list of cache entries where the total size of BCOs exceeds the given number.
splitEvictions :: Int -> [(Module, BcoCacheEntry)] -> ([(Module, BcoCacheEntry)], [(Module, BcoCacheEntry)])
splitEvictions limit entries =
  bimap (fmap fst) (fmap fst) $
  break (\ (_, size) -> size >= limit) $
  zip entries $
  scanl (+) 0 [entry.size | (_, entry) <- entries]

-- | Shared implementation for 'evictBcoCache' and 'evictSpecific': clear the bytecode fields in the HPT for the
-- given evicted modules and inform the interpreter's loader that only the remaining ('MakeState.bcoCache' minus
-- the evicted modules) linkables should stay live. No-ops if the evicted list is empty.
unloadEvicted :: HscEnv -> [(Module, BcoCacheEntry)] -> MakeState -> IO MakeState
unloadEvicted hsc_env evicted make
  | null evicted = pure make
  | otherwise = do
      traverse_ (dropFromHpt make.hug . fst) evicted
      unloadModules (hscInterp hsc_env) hsc_env modules
      pure make {bcoCache = Map.withoutKeys make.bcoCache (Set.fromList modules)}
  where
    modules = fst <$> evicted

-- | If the tracked total size of 'MakeState.bcoCache' exceeds the given limit, unload the least recently used excess
-- entries.
evictBcoCache :: HscEnv -> Int -> MakeState -> IO MakeState
evictBcoCache hsc_env limit make =
  unloadEvicted hsc_env evicted make
  where
    (_, evicted) = splitEvictions limit sorted

    sorted = reverse (List.sortOn ((.lastAccess) . snd) (Map.toList make.bcoCache))

-- | Unconditionally evict the given set of modules from 'MakeState.bcoCache', regardless of recency or size, in
-- response to an explicit eviction request from the instrumentation UI (see 'Internal.State.withState'). Modules not
-- currently present in the cache are ignored.
evictSpecific :: HscEnv -> Set Module -> MakeState -> IO MakeState
evictSpecific hsc_env targets make
  | Set.null targets = pure make
  | otherwise = unloadEvicted hsc_env evicted make
  where
    evicted = Map.toList (Map.restrictKeys make.bcoCache targets)
