{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeApplications #-}

-- | Prototype for sharing already-compiled bytecode between the main @ghc-server@ process and its
-- execute-subprocess child (see 'GhcServer.Build.Process') via a @\/dev\/shm@-backed @mmap@ region, instead of
-- relying solely on the child reconstructing bytecode from the on-disk interface cache
-- ('Internal.Cache.Hpt.loadCachedDep').
--
-- What actually gets shared is a 'GhcServer.Build.BytecodeMirror.MirrorLinkable', a 'Name'-free surrogate for
-- the *unlinked* bytecode ('GHC.ByteCode.Types.CompiledByteCode') found in each home module's
-- 'GHC.Unit.Home.ModInfo.HomeModLinkable' -- __not__ the real 'GHC.Linker.Types.Linkable' itself. A real
-- 'Linkable' cannot be put in a 'GHC.Compact.Compact' region directly: every 'GHC.Types.Name.Name' reachable
-- from a fully-assembled 'GHC.ByteCode.Types.UnlinkedBCO' carries an 'GHC.Types.Name.Occurrence.OccName' whose
-- underlying 'GHC.Data.FastString.FastString' has a memoized, unconditionally *pinned* Z-encoding cache
-- ('GHC.Data.FastString.fs_zenc'), and 'GHC.Compact.compact' rejects any pinned object reachable from the
-- compacted value (see @Test.CompactBytecodeTest@ and 'GhcServer.Build.BytecodeMirror' for the full root-cause
-- writeup and the mirroring strategy this module builds on). Linkables containing constructs that cannot be
-- mirrored (breakpoints, static pointers, @CoreBindings@\/@LazyBCOs@ parts -- see
-- 'GhcServer.Build.BytecodeMirror' for details) are simply excluded from the shared map; the child falls back
-- to its usual cache-based reconstruction for those modules.
--
-- Parent side: 'exportSharedBytecode' walks the parent's persisted 'GHC.Unit.Home.Graph.HomeUnitGraph'
-- (available from 'Types.State.Make.MakeState' without needing a live 'HscEnv'), mirrors and compacts every
-- mirrorable module's bytecode into a single 'GHC.Compact.Compact' region, and writes it into a freshly created
-- @\/dev\/shm@ file via a raw @mmap@\/@munmap@ FFI binding (the @unix@ package has no wrapper for @mmap@; see
-- 'Test.ForkTest' for the precedent).
--
-- | Child side: 'importSharedBytecode' opens the same file, @mmap@s it read-only, and reconstructs a 'Compact'
-- value in the child's own heap via 'GHC.Compact.Serialized.importCompactByteStrings' (which allocates fresh
-- compact blocks locally and relocates internal pointers -- it does not require matching virtual addresses
-- across processes). 'bytecodeImportEntries' then wraps each mirrored module into a closure that rehydrates it
-- back into a real 'GHC.Linker.Types.Linkable' on demand (reconstructing every mirrored 'Name' via the child's
-- own 'GHC.Types.Name.Cache.NameCache', see 'GhcServer.Build.BytecodeMirror.rehydrateName'); these closures are
-- installed into 'Types.State.Make.MakeState.bytecodeImport' so that 'Internal.State.Linkables.addLazyByteCode'
-- consults them lazily -- only for modules actually reached by a splice's link dependencies -- instead of
-- rehydrating every mirrored module up front regardless of whether it is ever linked.
module GhcServer.Build.SharedBytecode where

import Control.Monad (forM, forM_)
import Data.ByteString.Unsafe (unsafePackCStringLen)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Unique (hashUnique, newUnique)
import Data.Word (Word64, Word8)
import Foreign.C.Error (throwErrnoIf)
import Foreign.C.Types (CInt (..), CSize (..))
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (Ptr, castPtr, nullPtr, plusPtr, ptrToWordPtr, wordPtrToPtr)
import Foreign.Storable (peekByteOff, pokeByteOff)
import GHC (Module, ModuleName, mkModuleName, moduleNameString)
import GHC.Compact (compact, getCompact)
import GHC.Compact.Serialized (SerializedCompact (..), importCompactByteStrings, withSerializedCompact)
import GHC.Driver.Env (HscEnv)
import GHC.Linker.Types (Linkable)
import GHC.Unit.Home.Graph (HomeUnitEnv (..), HomeUnitGraph, UnitEnvGraph (..), homeUnitEnv_hpt)
import GHC.Unit.Home.ModInfo (HomeModInfo (..), HomeModLinkable (..))
import GHC.Unit.Home.PackageTable (concatHpt)
import GHC.Unit.Module.ModIface (mi_module)
import GHC.Unit.Types (UnitId, moduleName, stringToUnitId, unitIdString)
import GhcServer.Build.BytecodeMirror (MirrorLinkable, mirrorLinkable, rehydrateLinkable)
import Prelude hiding (log)
import System.Directory (getFileSize, removeFile)
import System.IO (IOMode (ReadMode, ReadWriteMode), hSetFileSize, openBinaryFile)
import System.Posix.IO (closeFd, handleToFd)
import System.Posix.Process (getProcessID)

-- | Everything shared across the process boundary: one mirrored 'MirrorLinkable' per home module, keyed by the
-- unit it belongs to and its module name. Only modules for which the parent's HPT already has bytecode that was
-- successfully mirrored (i.e. that were actually compiled with no unmirrorable construct, see
-- 'GhcServer.Build.BytecodeMirror.mirrorLinkable') are included.
--
-- Keys are plain 'String's rather than 'UnitId'\/'ModuleName': both wrap a 'GHC.Data.FastString.FastString',
-- whose pinned payload would make 'GHC.Compact.compact' fail with "cannot compact pinned objects".
type BytecodeMap = Map (String, String) MirrorLinkable

-- ---------------------------------------------------------------------------
-- Raw @mmap@ FFI (see 'Test.ForkTest' for the precedent: @unix@ has no binding for this).
-- ---------------------------------------------------------------------------

foreign import ccall unsafe "mmap"
  c_mmap :: Ptr () -> CSize -> CInt -> CInt -> CInt -> CSize -> IO (Ptr Word8)

foreign import ccall unsafe "munmap"
  c_munmap :: Ptr Word8 -> CSize -> IO CInt

protRead :: CInt
protRead = 0x1

protReadWrite :: CInt
protReadWrite = 0x1 + 0x2

-- | @MAP_SHARED@ (Linux/POSIX value). We map a real file descriptor, not an anonymous region, so
-- @MAP_ANONYMOUS@ is not needed here (unlike 'Test.ForkTest').
mapShared :: CInt
mapShared = 0x01

mmapFailed :: Ptr Word8
mmapFailed = castPtr (wordPtrToPtr maxBound)

mmapFd :: CInt -> Int -> Int -> IO (Ptr Word8)
mmapFd prot fd size =
  throwErrnoIf (== mmapFailed) "mmap" (c_mmap nullPtr (fromIntegral size) prot mapShared (fromIntegral fd) 0)

munmapPtr :: Ptr Word8 -> Int -> IO ()
munmapPtr ptr size = () <$ c_munmap ptr (fromIntegral size)

-- ---------------------------------------------------------------------------
-- Wire format
--
-- [magic :: Word64][blockCount :: Word64][rootAddr :: Word64]
-- for each block: [origAddr :: Word64][size :: Word64]
-- <block bytes, concatenated, in the same order as the header>
-- ---------------------------------------------------------------------------

sharedBytecodeMagic :: Word64
sharedBytecodeMagic = 0x67686373626f6300 -- "ghcsboc\0"

headerSize :: Int -> Int
headerSize blockCount = 8 + 8 + 8 + blockCount * 16

writeHeader :: Ptr Word8 -> [(Ptr (), Word)] -> Ptr () -> IO ()
writeHeader base blocks root = do
  pokeByteOff base 0 sharedBytecodeMagic
  pokeByteOff base 8 (fromIntegral (length blocks) :: Word64)
  pokeByteOff base 16 (ptrToWord64 root)
  forM_ (zip [0 ..] blocks) \ (i, (ptr, size)) -> do
    let off = 24 + i * 16
    pokeByteOff base off (ptrToWord64 ptr)
    pokeByteOff base (off + 8) (fromIntegral size :: Word64)
  where
    ptrToWord64 :: Ptr a -> Word64
    ptrToWord64 p = fromIntegral (ptrToWordPtr p)

readHeader :: Ptr Word8 -> IO (Int, Ptr (), [(Ptr (), Word)])
readHeader base = do
  magic <- peekByteOff base 0 :: IO Word64
  () <- if magic == sharedBytecodeMagic then pure () else ioError (userError "shared bytecode: bad magic")
  blockCount <- fromIntegral <$> (peekByteOff base 8 :: IO Word64)
  rootWord <- peekByteOff base 16 :: IO Word64
  metas <- forM [0 .. blockCount - 1] \ i -> do
    let off = 24 + i * 16
    addrWord <- peekByteOff base off :: IO Word64
    size <- peekByteOff base (off + 8) :: IO Word64
    pure (wordToPtr addrWord, fromIntegral size :: Word)
  pure (blockCount, wordToPtr rootWord, metas)
  where
    wordToPtr :: Word64 -> Ptr ()
    wordToPtr w = wordPtrToPtr (fromIntegral w)

-- ---------------------------------------------------------------------------
-- Parent side
-- ---------------------------------------------------------------------------

-- | Collect every module's bytecode from every home unit currently present in the parent's persisted
-- 'HomeUnitGraph', mirror it into a 'Name'-free surrogate, and key it by @(unit, module name)@. Modules whose
-- 'HomeModLinkable' has no bytecode (interface only, or object-code only), or whose bytecode could not be
-- mirrored (breakpoints, static pointers, unsupported linkable parts), are omitted.
--
-- TODO compile interface bytecode when missing
collectBytecode :: HomeUnitGraph -> IO BytecodeMap
collectBytecode hug = do
  perUnit <- forM (Map.toList (unitEnv_graph hug)) \ (uid, hue) -> do
    entries <- concatHpt (bytecodeEntry uid) (homeUnitEnv_hpt hue)
    pure entries
  pure (Map.fromList (concat perUnit))
  where
    bytecodeEntry uid hmi =
      case hmi.hm_linkable.homeMod_bytecode >>= mirrorLinkable of
        Just mirrored -> [((unitIdString uid, moduleNameString (moduleName (mi_module hmi.hm_iface))), mirrored)]
        Nothing -> []

-- | Compact 'collectBytecode''s result and write it into a fresh @\/dev\/shm@ file, returning its path. Returns
-- 'Nothing' (and creates no file) if there is nothing to share, which the caller should treat as "the child
-- falls back to its usual cache-only bytecode reconstruction".
exportSharedBytecode :: BytecodeMap -> IO (Maybe FilePath)
exportSharedBytecode bytecodeMap
  | Map.null bytecodeMap = pure Nothing
  | otherwise = do
      c <- compact bytecodeMap
      withSerializedCompact c \ SerializedCompact {serializedCompactBlockList, serializedCompactRoot} -> do
        pid <- getProcessID
        uniq <- hashUnique <$> newUnique
        let path = "/dev/shm/ghc-server-bco-" ++ show pid ++ "-" ++ show uniq
            totalSize = headerSize (length serializedCompactBlockList) + sum (map (fromIntegral . snd) serializedCompactBlockList)
        h <- openBinaryFile path ReadWriteMode
        hSetFileSize h (fromIntegral totalSize)
        fd <- handleToFd h
        ptr <- mmapFd protReadWrite (fromIntegral fd) totalSize
        writeHeader ptr serializedCompactBlockList serializedCompactRoot
        copyBlocks ptr (headerSize (length serializedCompactBlockList)) serializedCompactBlockList
        munmapPtr ptr totalSize
        closeFd fd
        pure (Just path)
  where
    copyBlocks base off0 blocks =
      () <$ foldM' off0 blocks \ off (blockPtr, size) -> do
        copyBytes (base `plusPtr` off) (castPtr blockPtr) (fromIntegral size)
        pure (off + fromIntegral size)

    foldM' z xs f = go z xs
      where
        go acc [] = pure acc
        go acc (x : xs') = f acc x >>= \ acc' -> go acc' xs'

-- | Remove the file created by 'exportSharedBytecode'. Safe to call unconditionally after the child that read it
-- has exited (the parent spawns it via a blocking 'System.Process.Typed.readProcess', so there is no race
-- between the child opening the file and the parent removing it).
cleanupSharedBytecode :: FilePath -> IO ()
cleanupSharedBytecode = removeFile

-- ---------------------------------------------------------------------------
-- Child side
-- ---------------------------------------------------------------------------

-- | Open the file written by 'exportSharedBytecode', @mmap@ it read-only, and reconstruct the parent's
-- 'BytecodeMap' via 'GHC.Compact.Serialized.importCompactByteStrings'. Returns 'Nothing' if the region turns out
-- to be corrupt or its pointers could not be relocated.
importSharedBytecode :: FilePath -> IO (Maybe BytecodeMap)
importSharedBytecode path = do
  size <- fromIntegral <$> getFileSize path
  h <- openBinaryFile path ReadMode
  fd <- handleToFd h
  ptr <- mmapFd protRead (fromIntegral fd) size
  (blockCount, root, metas) <- readHeader ptr
  let dataOffsets = scanl (\ off (_, sz) -> off + fromIntegral sz) (headerSize blockCount) metas
  bss <- forM (zip metas dataOffsets) \ ((_, size'), off) ->
    unsafePackCStringLen (castPtr (ptr `plusPtr` off), fromIntegral size')
  result <- importCompactByteStrings (SerializedCompact metas root) bss
  munmapPtr ptr size
  closeFd fd
  pure (getCompact <$> result)

-- | Convert an imported 'BytecodeMap' into the closures expected by 'Types.State.Make.MakeState.bytecodeImport':
-- for each mirrored module, a function that rehydrates it into a real 'Linkable' given the 'HscEnv' and target
-- 'Module' supplied later by 'Internal.State.Linkables.addLazyByteCode', at the point a splice's link dependencies
-- actually need it. This is what lets 'GhcServer.Build.Process.runEval' populate
-- 'Types.State.Make.MakeState.bytecodeImport' with modules whose bytecode is rehydrated lazily, only when actually
-- linked, instead of eagerly rehydrating every mirrored module up front regardless of whether it's ever linked.
bytecodeImportEntries :: BytecodeMap -> Map (UnitId, ModuleName) (HscEnv -> Module -> IO Linkable)
bytecodeImportEntries =
  Map.map (\ mirrored hsc_env target -> rehydrateLinkable hsc_env target mirrored)
  .
  Map.mapKeys (\ (uid, modName) -> (stringToUnitId uid, mkModuleName modName))
