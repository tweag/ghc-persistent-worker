-- | Recreating the interpreter allocations referenced by a 'GHC.ByteCode.Types.CompiledByteCode' in another
-- process.
--
-- 'bc_strs' and 'bc_itbls' map 'Name's to memory allocated by the *compiling* process's interpreter (top-level
-- string literals and data constructor info tables). The bytecode linker resolves module-local references only
-- through these tables, so a process importing mirrored bytecode has to allocate equivalent memory itself.
--
-- This module provides the process-independent descriptions of those allocations and the functions that
-- allocate them via 'interpCmd':
--
-- * String literals: their contents are read from the exporting process's memory, which requires the internal
--   interpreter (the 'GHCi.RemoteTypes.RemotePtr' is then a local address). Only the start address is stored and
--   the allocation has a trailing NUL added by @GHCi.Run.mkString0@, so the length is determined by the first NUL.
--   Literals containing NUL bytes (possible for primitive @"..."#@ literals, but not for ordinary 'String'
--   literals, which GHC encodes without NULs) are therefore truncated. The interface's Core can't be used instead,
--   since it isn't retained in the HPT's 'ModIface' and lacks literals floated to the top level by CorePrep.
--
-- * Local string literals (not floated to the top level): the assembler mallocs them and replaces the
--   'GHC.ByteCode.Types.BCONPtrStr' by a 'GHC.ByteCode.Types.BCONPtrWord' containing the raw address (see
--   Note [Allocating string literals] in "GHC.ByteCode.Asm"), which is indistinguishable from a numeric literal.
--   'readLocalStrings' classifies a word as a string address heuristically, if it lies in a writable anonymous
--   mapping of the current process (where malloc allocates). A numeric literal that happens to fall into such a
--   range would be misinterpreted (and might crash while reading it); an address outside of those ranges would
--   be kept verbatim and crash the importing process when dereferenced.
--
-- * Info tables: either described by the arguments of 'MkConInfoTable', computed from the 'DataCon' exactly like
--   @GHC.ByteCode.InfoTable.make_constr_itbls@ does ('conInfoTable'), or rebuilt from scratch from the importing
--   process's own 'TyCon's for the module ('rebuildItbls').
module GhcServer.Build.InterpAllocations (
  ConInfoTable (..),
  conInfoTable,
  allocConInfoTable,
  readStrings,
  readLocalStrings,
  allocStrings,
  allocString,
  rebuildItbls,
) where

import Control.Exception (evaluate)
import Data.ByteString (ByteString, packCString)
import Data.ByteString.Short (ShortByteString, fromShort, toShort)
import Data.Containers.ListUtils (nubOrd)
import Data.List (isPrefixOf)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Traversable (for)
import Foreign.Ptr (castPtr, ptrToWordPtr, wordPtrToPtr)
import Numeric (readHex)
import GHC.ByteCode.InfoTable (mkITbls)
import GHC.ByteCode.Types (AddrEnv, AddrPtr (..), ItblEnv, ItblPtr (..))
import GHC.Core.DataCon (DataCon, dataConIdentity, dataConRepArgTys, dataConTag)
import GHC.Core.TyCo.Rep (scaledThing)
import GHC.Driver.Env (HscEnv (..), hscInterp, hsc_HUG)
import GHC.Driver.DynFlags (targetProfile)
import GHC.Platform (platformConstants, platformTablesNextToCode, pc_MIN_PAYLOAD_SIZE)
import GHC.Platform.Profile (Profile, profilePlatform)
import GHC.Runtime.Interpreter (interpCmd)
import GHC.StgToCmm.Closure (tagForCon)
import GHC.StgToCmm.Layout (mkVirtConstrSizes)
import GHC.Types.Name.Env (NameEnv, mkNameEnv, nonDetNameEnvElts)
import GHC.Types.RepType (typePrimRep)
import GHC.Types.TypeEnv (typeEnvTyCons)
import GHC.Unit.Home.Graph (lookupHugByModule)
import GHC.Unit.Home.ModInfo (HomeModInfo (..))
import GHC.Unit.Module.ModDetails (ModDetails (..))
import GHC.Unit.Types (Module)
import GHC.Utils.Outputable (ppr, showSDocUnsafe)
import GHCi.Message (Message (MallocStrings, MkConInfoTable))
import GHCi.RemoteTypes (fromRemotePtr)

-- | The arguments of 'MkConInfoTable'. Uses 'ShortByteString' because a strict 'ByteString' is pinned and would
-- break compaction.
data ConInfoTable = ConInfoTable {
  tablesNextToCode :: Bool,
  ptrs :: Int,
  nptrs :: Int,
  conNo :: Int,
  ptrTag :: Int,
  descr :: ShortByteString
}

-- | Replicates the computation in @GHC.ByteCode.InfoTable.make_constr_itbls@, which isn't exported.
-- Constructor numbers start at zero, while 'dataConTag' starts at one.
conInfoTable :: Profile -> DataCon -> ConInfoTable
conInfoTable profile dcon =
  ConInfoTable {
    tablesNextToCode = platformTablesNextToCode platform,
    ptrs = ptrWords,
    nptrs = nptrsReally,
    conNo = dataConTag dcon - 1,
    ptrTag = tagForCon platform dcon,
    descr = toShort (dataConIdentity dcon)
  }
  where
    repArgs = [primRep | arg <- dataConRepArgTys dcon, primRep <- typePrimRep (scaledThing arg)]

    (totWords, ptrWords) = mkVirtConstrSizes profile repArgs

    nptrs' = totWords - ptrWords

    nptrsReally
      | ptrWords + nptrs' >= pc_MIN_PAYLOAD_SIZE (platformConstants platform) = nptrs'
      | otherwise = pc_MIN_PAYLOAD_SIZE (platformConstants platform) - ptrWords

    platform = profilePlatform profile

allocConInfoTable :: HscEnv -> ConInfoTable -> IO ItblPtr
allocConInfoTable env t =
  ItblPtr <$> interpCmd (hscInterp env) (MkConInfoTable t.tablesNextToCode t.ptrs t.nptrs t.conNo t.ptrTag (fromShort t.descr))

-- | Read the contents of all string literals from the (internal) interpreter's memory. See module docs.
readStrings :: AddrEnv -> IO (NameEnv ShortByteString)
readStrings strs =
  mkNameEnv <$> for (nonDetNameEnvElts strs) \ (n, AddrPtr p) ->
    (n,) . toShort <$> packCString (castPtr (fromRemotePtr p))

-- | Allocate a single string, returning its address as it is spliced into the instruction stream by the
-- assembler.
allocString :: HscEnv -> ByteString -> IO Word
allocString env bs =
  allocStrings env [bs] >>= \case
    [AddrPtr p] -> pure (fromIntegral (ptrToWordPtr (fromRemotePtr p)))
    _ -> fail "allocString: MallocStrings returned an unexpected number of pointers"

-- | Read the local string literals among the given literal words from memory. See module docs.
readLocalStrings :: [Word] -> IO (Map Word ShortByteString)
readLocalStrings ws = do
  ranges <- mallocRanges
  let candidates = filter (\ w -> any (\ (lo, hi) -> lo <= w && w < hi) ranges) (nubOrd ws)
  Map.fromList <$> for candidates \ w ->
    (w,) . toShort <$> packCString (wordPtrToPtr (fromIntegral w))

-- | Address ranges of writable anonymous mappings (including @[heap]@) of the current process.
mallocRanges :: IO [(Word, Word)]
mallocRanges = do
  maps <- readFile "/proc/self/maps"
  _ <- evaluate (length maps)
  pure (mapMaybe parse (lines maps))
  where
    parse :: String -> Maybe (Word, Word)
    parse l = case words l of
      range : perms : _ : _ : "0" : rest
        | "rw" `isPrefixOf` perms, null rest || rest == ["[heap]"]
        , [(lo, '-' : hiStr)] <- readHex range
        , [(hi, "")] <- readHex hiStr
        -> Just (lo, hi)
      _ -> Nothing

allocStrings :: HscEnv -> [ByteString] -> IO [AddrPtr]
allocStrings env strings =
  fmap AddrPtr <$> interpCmd (hscInterp env) (MallocStrings strings)

-- | Allocate info tables for all data constructors of the target module, using the 'TyCon's from the importing
-- process's own home unit graph (i.e. its loaded interface).
rebuildItbls :: HscEnv -> Module -> IO ItblEnv
rebuildItbls env target =
  lookupHugByModule target (hsc_HUG env) >>= \case
    Just hmi -> mkITbls (hscInterp env) (targetProfile env.hsc_dflags) (typeEnvTyCons hmi.hm_details.md_types)
    Nothing -> fail ("rebuildItbls: module not in home unit graph: " ++ showSDocUnsafe (ppr target))
