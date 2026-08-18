-- | \"Mirrored\" (Name-free) surrogate representations for GHC's bytecode types
-- ('GHC.ByteCode.Types.CompiledByteCode' / 'GHC.ByteCode.Types.UnlinkedBCO' / 'GHC.Linker.Types.Linkable'), plus
-- conversions to and from the real types.
--
-- __Why this exists__: every 'GHC.Types.Name.Name' reachable from a fully-assembled 'UnlinkedBCO' carries an
-- 'GHC.Types.Name.Occurrence.OccName' whose underlying 'GHC.Data.FastString.FastString' has a memoized
-- Z-encoding cache ('GHC.Data.FastString.fs_zenc'), which is an unconditionally *pinned* 'Data.ByteString.ByteString'.
-- 'GHC.Compact.compact' unconditionally rejects any pinned object reachable from the compacted value, so
-- 'CompiledByteCode' cannot be compacted directly (see @Test.CompactBytecodeTest@ for the regression test and
-- the full root-cause writeup). Both 'GHC.Types.Name.Name' and 'GHC.Data.FastString.FastZString' are exported
-- abstractly by @ghc@, so there is no way to patch or rebuild the pinned field in place; the only viable
-- workaround is to build a parallel, 'Name'-free representation from plain-data accessors, compact *that*, and
-- reconstruct real 'Name's on the importing side afterwards.
--
-- __How reconstruction works__: a 'Name' is not simply a numeric identity. Its 'GHC.Types.Unique.Unique' is
-- assigned per-process by each process's own 'GHC.Types.Name.Cache.NameCache' and has no meaning across a
-- process boundary. What *is* stable across processes is the pair (originating 'Module', 'OccName'): GHC's own
-- interface-loading machinery ('GHC.Iface.Env.lookupNameCache' / 'lookupOrig') already relies on this to give
-- every reference to the "same" external binder the same 'Name' (and hence the same 'Unique') within one
-- process, regardless of how many times or from how many different interfaces it is looked up. We reuse exactly
-- that mechanism: 'MirrorName' records a Name's originating module (as a plain 'String' pair, since 'GHC.Unit.Types.Unit'/
-- 'GHC.Unit.Types.Module' are reconstructible from a 'UnitId' string and a 'GHC.Unit.Module.Name.ModuleName' string
-- for any *real*, installed unit) and its 'OccName', and 'rehydrateName' feeds that pair back through the
-- importing process's own 'NameCache'. If the importing process has already loaded (or later loads) the
-- interface that defines that binder, 'lookupNameCache' returns the exact same 'Name' its own interface-loading
-- code would have produced; if not, it mints a fresh (but internally consistent) one.
--
-- __Known limitations (documented, not solved here)__:
--
-- * 'GHC.ByteCode.Types.bc_itbls'\/'bc_strs'\/'bc_ffis' hold 'GHCi.RemoteTypes.RemotePtr' addresses of memory
--   allocated in the *exporting* process's interpreter (info tables for the module's own data constructors,
--   top-level string literals, FFI call descriptors). They are not a re-derivable cache: the bytecode linker
--   resolves a module-local 'BCONPtrAddr'\/'BCONPtrItbl' only through these tables and otherwise falls back to a
--   symbol lookup that panics (@nameModule@) for internal names, and FFI descriptors are baked into instructions
--   as raw words. Verified against a real execute-subprocess run (a module's @$trModule@ string literals).
--   String literals and info tables are therefore described in a process-independent form and allocated anew by
--   the importing process (see "GhcServer.Build.InterpAllocations" and 'serializeItbls'); a module whose
--   literal contents or constructors can't be described is unmirrorable. 'bc_ffis' can't be recreated (the
--   'PrepFFI' arguments are not retained), so any 'CompiledByteCode' with FFI calls is unmirrorable, like
--   breakpoints below, and the child reconstructs that module itself.
-- * Static pointer table entries ('GHC.ByteCode.Types.bc_spt_entries', @['GHC.Types.SptEntry.SptEntry']@) embed a
--   full typed 'GHC.Types.Var.Id' binder, not just a 'Name', and are dropped entirely (rehydrated as @[]@). Any
--   module using the @StaticPointers@ extension will silently lose its static pointer table across the shared
--   path.
-- * Breakpoints ('GHC.ByteCode.Types.ModBreaks', @'GHC.ByteCode.Types.BCOPtrBreakArray'@) hold a 'GHCi.RemoteTypes.ForeignRef'
--   into the exporting process's interpreter memory; a 'CompiledByteCode' or nested 'UnlinkedBCO' containing
--   either is treated as unmirrorable and its entire enclosing 'Linkable' is excluded from sharing (the child
--   falls back to its usual cache-based reconstruction for that module).
-- * 'GHC.Linker.Types.CoreBindings' \/ 'GHC.Linker.Types.LazyBCOs' linkable parts (used for the
--   \"Interface Files with Core Definitions\" lazy-bytecode feature) are also treated as unmirrorable for the
--   same reason ('WholeCoreBindings' embeds full Core with 'Id's throughout); only 'GHC.Linker.Types.DotO'\/'DotA'\/'DotDLL'
--   (plain 'FilePath's, already safely compactable) and 'GHC.Linker.Types.BCOs' are handled.
-- * Record-field 'OccName's ('GHC.Types.Name.Occurrence.FldName') are mirrored as ordinary 'GHC.Types.Name.Occurrence.VarName'
--   occurrences. This is a best-effort approximation (top-level bytecode 'Name's are not expected to be field
--   selectors under this representation in practice) rather than a verified-correct case.
module GhcServer.Build.BytecodeMirror (
  MirrorModule (..),
  MirrorOccNS (..),
  MirrorName (..),
  MirrorBCONPtr (..),
  MirrorBCOPtr (..),
  MirrorUnlinkedBCO (..),
  MirrorCompiledByteCode (..),
  MirrorItbls (..),
  MirrorSource (..),
  mirrorSourceFor,
  serializeItbls,
  MirrorLinkablePart (..),
  MirrorLinkable (..),
  mirrorName,
  rehydrateName,
  mirrorUnlinkedBCO,
  mirrorCompiledByteCode,
  mirrorLinkable,
  rehydrateCompiledByteCode,
  rehydrateLinkable,
) where

import qualified Data.ByteString
import qualified Data.ByteString.Short as ShortByteString
import Data.ByteString.Short (ShortByteString, fromShort)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Traversable (for)
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Time (UTCTime)
import Data.Word (Word16, Word64, Word8)
import GHC.Builtin.PrimOps (PrimOp)
import GHC.ByteCode.Types (AddrEnv, BCOByteArray, BCONPtr (..), BCOPtr (..), CompiledByteCode (..), ItblEnv, UnlinkedBCO (..))
import GHC.Core.ConLike (ConLike (RealDataCon))
import GHC.Platform.Profile (Profile)
import GHC.Types.TyThing (TyThing (AConLike))
import GHC.Types.TypeEnv (TypeEnv, lookupTypeEnv)
import GHC.Unit.Home.ModInfo (HomeModInfo (..))
import GHC.Unit.Module.ModDetails (ModDetails (..))
import GhcServer.Build.InterpAllocations (
  ConInfoTable,
  allocConInfoTable,
  allocString,
  allocStrings,
  conInfoTable,
  readLocalStrings,
  readStrings,
  rebuildItbls,
  )
import qualified GHC.Data.FastString as FastString
import GHC.Data.FlatBag (elemsFlatBag, fromList)
import GHC.Driver.Env (HscEnv, hsc_NC)
import GHC.Iface.Env (lookupNameCache)
import GHC.Linker.Types (Linkable (..), LinkablePart (..))
import qualified GHC.Types.Name as Name
import GHC.Types.Name (Name)
import qualified GHC.Types.Name.Cache as Name.Cache
import GHC.Types.Name.Env (NameEnv, lookupNameEnv, mkNameEnv, nonDetNameEnvElts)
import GHC.Unit.Home.ModInfo (HomeModLinkable (..))
import Data.Foldable (toList)
import qualified GHC.Types.Name.Occurrence as Occ
import GHC.Types.Name.Occurrence (NameSpace, OccName)
import GHC.Types.Unique (getKey)
import qualified GHC.Unit.Types as Unit
import GHC.Unit.Types (Definite (..), GenUnit (..), Module, mkModule, moduleName, moduleUnitId, unitIdString)
import Language.Haskell.Syntax.Module.Name (mkModuleName, moduleNameString)

-- ---------------------------------------------------------------------------
-- Module / OccName mirrors
-- ---------------------------------------------------------------------------

-- | A 'Module''s stable, process-independent identity: an installed unit's textual 'UnitId' plus its module
-- name. Reconstructible via 'rehydrateModule' for any real (non-virtual, non-hole) unit, which covers every unit
-- involved in this project's builds.
data MirrorModule = MirrorModule {
  mirrorUnit :: String,
  mirrorModuleName :: String
} deriving stock (Show, Eq, Ord)

mirrorModule :: Module -> MirrorModule
mirrorModule m =
  MirrorModule (unitIdString (moduleUnitId m)) (moduleNameString (moduleName m))

rehydrateModule :: MirrorModule -> Module
rehydrateModule (MirrorModule uid modName) =
  mkModule (RealUnit (Definite (Unit.UnitId (FastString.mkFastString uid)))) (mkModuleName modName)

-- | Mirror of 'GHC.Types.Name.Occurrence.NameSpace'. Collapses 'GHC.Types.Name.Occurrence.FldName' into
-- 'OccVar' (see module docs).
data MirrorOccNS = OccVar | OccData | OccTcCls | OccTv
  deriving stock (Show, Eq, Ord)

mirrorOccNS :: NameSpace -> MirrorOccNS
mirrorOccNS ns
  | Occ.isDataConNameSpace ns = OccData
  | Occ.isTcClsNameSpace ns = OccTcCls
  | Occ.isTvNameSpace ns = OccTv
  | otherwise = OccVar

rehydrateOccNS :: MirrorOccNS -> NameSpace
rehydrateOccNS = \case
  OccVar -> Occ.varName
  OccData -> Occ.dataName
  OccTcCls -> Occ.tcClsName
  OccTv -> Occ.tvName

mirrorOccName :: OccName -> (MirrorOccNS, String)
mirrorOccName occ = (mirrorOccNS (Occ.occNameSpace occ), Occ.occNameString occ)

rehydrateOccName :: (MirrorOccNS, String) -> OccName
rehydrateOccName (ns, s) = Occ.mkOccName (rehydrateOccNS ns) s

-- ---------------------------------------------------------------------------
-- Name mirror
-- ---------------------------------------------------------------------------

-- | 'Name'-free surrogate for 'GHC.Types.Name.Name'. 'MirrorExternalName' covers 'Name's with a 'Module'.
-- 'MirrorLocalName' covers internal\/system names, which do occur in freshly compiled bytecode (e.g. the names of
-- non-exported top-level BCOs like @$comitField@ that sibling BCOs in the same 'CompiledByteCode' reference via
-- 'BCOPtrName'). The bytecode linker resolves those by 'GHC.Types.Unique.Unique' against the BCO group before
-- falling back to a module-based lookup (which panics for local names), so the original 'Unique' key is
-- recorded and 'rehydrateName' maps every occurrence of the same key to the same fresh 'Name'.
data MirrorName
  = MirrorExternalName MirrorModule MirrorOccNS String
  | MirrorLocalName Word64 MirrorOccNS String
  deriving stock (Show, Eq, Ord)

-- | Rehydration context: the importing session, plus a memo table from original local-name 'Unique' keys to the
-- fresh 'Name's minted for them, so that references to a local binder stay consistent with its definition.
data Rehydrate = Rehydrate {
  hscEnv :: HscEnv,
  target :: Module,
  locals :: IORef (Map Word64 Name)
}

newRehydrate :: HscEnv -> Module -> IO Rehydrate
newRehydrate hscEnv target = Rehydrate hscEnv target <$> newIORef Map.empty

mirrorName :: Name -> MirrorName
mirrorName n =
  case Name.nameModule_maybe n of
    Just m ->
      let (ns, s) = mirrorOccName (Name.nameOccName n)
      in MirrorExternalName (mirrorModule m) ns s
    Nothing ->
      let (ns, s) = mirrorOccName (Name.nameOccName n)
      in MirrorLocalName (getKey (Name.nameUnique n)) ns s

-- | Reconstruct a 'Name' via the importing process's own 'GHC.Types.Name.Cache.NameCache'. For
-- 'MirrorExternalName', this returns the exact same 'Name' (same 'GHC.Types.Unique.Unique') that the importing
-- process's own interface-loading code would produce for that (module, 'OccName') pair -- see module docs.
rehydrateName :: Rehydrate -> MirrorName -> IO Name
rehydrateName ctx = \case
  MirrorExternalName mm ns s ->
    lookupNameCache (hsc_NC ctx.hscEnv) (rehydrateModule mm) (rehydrateOccName (ns, s))
  MirrorLocalName key ns s ->
    Map.lookup key <$> readIORef ctx.locals >>= \case
      Just name -> pure name
      Nothing -> do
        name <- Name.mkSystemName <$> Name.Cache.takeUniqFromNameCache (hsc_NC ctx.hscEnv) <*> pure (rehydrateOccName (ns, s))
        name <$ modifyIORef' ctx.locals (Map.insert key name)

-- ---------------------------------------------------------------------------
-- BCONPtr / BCOPtr / UnlinkedBCO / CompiledByteCode mirrors
-- ---------------------------------------------------------------------------

data MirrorBCONPtr
  = MirrorBCONWord Word
  | MirrorBCONLbl String
  | MirrorBCONItbl MirrorName
  | MirrorBCONAddr MirrorName
  | MirrorBCONStr [Word8]
  deriving stock (Show)

mirrorBCONPtr :: Map Word ShortByteString -> BCONPtr -> MirrorBCONPtr
mirrorBCONPtr localStrs = \case
  BCONPtrWord w
    | Just s <- Map.lookup w localStrs -> MirrorBCONStr (ShortByteString.unpack s)
    | otherwise -> MirrorBCONWord w
  BCONPtrLbl fs -> MirrorBCONLbl (FastString.unpackFS fs)
  BCONPtrItbl n -> MirrorBCONItbl (mirrorName n)
  BCONPtrAddr n -> MirrorBCONAddr (mirrorName n)
  BCONPtrStr bs -> MirrorBCONStr (Data.ByteString.unpack bs)

rehydrateBCONPtr :: Rehydrate -> MirrorBCONPtr -> IO BCONPtr
rehydrateBCONPtr env = \case
  MirrorBCONWord w -> pure (BCONPtrWord w)
  MirrorBCONLbl s -> pure (BCONPtrLbl (FastString.mkFastString s))
  MirrorBCONItbl mn -> BCONPtrItbl <$> rehydrateName env mn
  MirrorBCONAddr mn -> BCONPtrAddr <$> rehydrateName env mn
  MirrorBCONStr ws -> BCONPtrWord <$> allocString env.hscEnv (Data.ByteString.pack ws)

-- | Mirror of 'GHC.ByteCode.Types.BCOPtr'. 'BCOPtrPrimOp' is kept verbatim (a plain enum, no pinned data, no
-- 'Name'). 'BCOPtrBreakArray' has no mirror -- its presence makes the enclosing 'UnlinkedBCO' (and hence the
-- whole 'CompiledByteCode'\/'Linkable') unmirrorable, see 'mirrorUnlinkedBCO'.
data MirrorBCOPtr
  = MirrorBCOName MirrorName
  | MirrorBCOPrimOp !PrimOp
  | MirrorBCOBCO MirrorUnlinkedBCO

-- | 'BCOPtrBreakArray' has no representation here: encountering one during mirroring aborts the whole
-- enclosing 'UnlinkedBCO' (returned as 'Nothing' from 'mirrorBCOPtr').
mirrorBCOPtr :: Map Word ShortByteString -> BCOPtr -> Maybe MirrorBCOPtr
mirrorBCOPtr localStrs = \case
  BCOPtrName n -> Just (MirrorBCOName (mirrorName n))
  BCOPtrPrimOp op -> Just (MirrorBCOPrimOp op)
  BCOPtrBCO bco -> MirrorBCOBCO <$> mirrorUnlinkedBCO localStrs bco
  BCOPtrBreakArray _ -> Nothing

rehydrateBCOPtr :: Rehydrate -> MirrorBCOPtr -> IO BCOPtr
rehydrateBCOPtr env = \case
  MirrorBCOName mn -> BCOPtrName <$> rehydrateName env mn
  MirrorBCOPrimOp op -> pure (BCOPtrPrimOp op)
  MirrorBCOBCO mbco -> BCOPtrBCO <$> rehydrateUnlinkedBCO env mbco

data MirrorUnlinkedBCO = MirrorUnlinkedBCO {
  mirrorBCOName :: MirrorName,
  mirrorBCOArity :: Int,
  mirrorBCOInstrs :: BCOByteArray Word16,
  mirrorBCOBitmap :: BCOByteArray Word,
  mirrorBCOLits :: [MirrorBCONPtr],
  mirrorBCOPtrs :: [MirrorBCOPtr]
}

-- | 'Nothing' if 'unlinkedBCOPtrs' contains an unmirrorable 'GHC.ByteCode.Types.BCOPtrBreakArray' anywhere
-- (including transitively, through nested 'BCOPtrBCO's). Words found in @localStrs@ are local string literals,
-- see 'bcoLitWords'.
mirrorUnlinkedBCO :: Map Word ShortByteString -> UnlinkedBCO -> Maybe MirrorUnlinkedBCO
mirrorUnlinkedBCO localStrs u = do
  ptrs <- traverse (mirrorBCOPtr localStrs) (elemsFlatBag u.unlinkedBCOPtrs)
  pure MirrorUnlinkedBCO {
    mirrorBCOName = mirrorName u.unlinkedBCOName,
    mirrorBCOArity = u.unlinkedBCOArity,
    mirrorBCOInstrs = u.unlinkedBCOInstrs,
    mirrorBCOBitmap = u.unlinkedBCOBitmap,
    mirrorBCOLits = mirrorBCONPtr localStrs <$> elemsFlatBag u.unlinkedBCOLits,
    mirrorBCOPtrs = ptrs
  }

rehydrateUnlinkedBCO :: Rehydrate -> MirrorUnlinkedBCO -> IO UnlinkedBCO
rehydrateUnlinkedBCO env m = do
  name <- rehydrateName env m.mirrorBCOName
  lits <- traverse (rehydrateBCONPtr env) m.mirrorBCOLits
  ptrs <- traverse (rehydrateBCOPtr env) m.mirrorBCOPtrs
  pure UnlinkedBCO {
    unlinkedBCOName = name,
    unlinkedBCOArity = m.mirrorBCOArity,
    unlinkedBCOInstrs = m.mirrorBCOInstrs,
    unlinkedBCOBitmap = m.mirrorBCOBitmap,
    unlinkedBCOLits = fromList (fromIntegral (length lits)) lits,
    unlinkedBCOPtrs = fromList (fromIntegral (length ptrs)) ptrs
  }

-- | Mirror of 'GHC.ByteCode.Types.CompiledByteCode'. Carries the top-level 'UnlinkedBCO's plus descriptions of
-- the interpreter allocations referenced by them (see "GhcServer.Build.InterpAllocations").
data MirrorCompiledByteCode = MirrorCompiledByteCode {
  mirrorBCOs :: [MirrorUnlinkedBCO],
  mirrorStrs :: [(MirrorName, ShortByteString)],
  mirrorItbls :: MirrorItbls
}

-- | Info tables, either serialized as 'MkConInfoTable' arguments or rebuilt by the importing process from its own
-- 'TyCon's for the module. See 'serializeItbls'.
data MirrorItbls
  = MirrorItblsSerialized [(MirrorName, ConInfoTable)]
  | MirrorItblsRebuild

-- | Manual toggle: 'True' serializes info table descriptions computed in the exporting process, 'False' makes
-- the importing process rebuild them from the module's 'TyCon's in its home unit graph (which requires the
-- interface to be loaded there).
serializeItbls :: Bool
serializeItbls = True

-- | Data from the exporting process needed to describe a module's interpreter allocations.
data MirrorSource = MirrorSource {
  profile :: Profile,
  typeEnv :: TypeEnv,
  topStrings :: NameEnv ShortByteString,
  -- | Local string literals, keyed by the address baked into the instruction stream. See 'readLocalStrings'.
  localStrings :: Map Word ShortByteString
}

-- | Reads the string literals of the module's bytecode from memory, see 'readStrings' and 'readLocalStrings'.
mirrorSourceFor :: Profile -> HomeModInfo -> IO MirrorSource
mirrorSourceFor profile hmi = do
  topStrings <- foldMap readStrings (bc_strs <$> cbcs)
  localStrings <- readLocalStrings (foldMap bcoLitWords (foldMap (elemsFlatBag . bc_bcos) cbcs))
  pure MirrorSource {profile, typeEnv = hmi.hm_details.md_types, topStrings, localStrings}
  where
    cbcs = [cbc | l <- toList hmi.hm_linkable.homeMod_bytecode, BCOs cbc <- toList l.linkableParts]

-- | All 'BCONPtrWord' literals of a BCO, including nested ones. After assembly, local string literals are
-- indistinguishable from numeric ones at this level (see Note [Allocating string literals] in
-- "GHC.ByteCode.Asm"), so 'readLocalStrings' classifies them by address.
bcoLitWords :: UnlinkedBCO -> [Word]
bcoLitWords u =
  [w | BCONPtrWord w <- elemsFlatBag u.unlinkedBCOLits]
  ++ [w | BCOPtrBCO b <- elemsFlatBag u.unlinkedBCOPtrs, w <- bcoLitWords b]

mirrorStr :: MirrorSource -> Name -> Maybe (MirrorName, ShortByteString)
mirrorStr src n = (mirrorName n,) <$> lookupNameEnv src.topStrings n

mirrorItbl :: MirrorSource -> Name -> Maybe (MirrorName, ConInfoTable)
mirrorItbl src n =
  lookupTypeEnv src.typeEnv n >>= \case
    AConLike (RealDataCon dc) -> Just (mirrorName n, conInfoTable src.profile dc)
    _ -> Nothing

mirrorItbls :: MirrorSource -> ItblEnv -> Maybe MirrorItbls
mirrorItbls src itbls
  | serializeItbls = MirrorItblsSerialized <$> traverse (mirrorItbl src . fst) (nonDetNameEnvElts itbls)
  | otherwise = Just MirrorItblsRebuild

-- | Mirror of 'GHC.ByteCode.Types.CompiledByteCode'. Drops 'bc_spt_entries' (see module docs). 'Nothing' if
-- 'bc_breaks' is set, if 'bc_ffis' is non-empty, if a string literal or info table can't be described (see
-- "GhcServer.Build.InterpAllocations"), or if any contained 'UnlinkedBCO' is unmirrorable.
mirrorCompiledByteCode :: MirrorSource -> CompiledByteCode -> Maybe MirrorCompiledByteCode
mirrorCompiledByteCode src cbc
  | Just _ <- cbc.bc_breaks = Nothing
  | not (null cbc.bc_ffis) = Nothing
  | otherwise = do
      bcos <- traverse (mirrorUnlinkedBCO src.localStrings) (elemsFlatBag cbc.bc_bcos)
      strs <- traverse (mirrorStr src . fst) (nonDetNameEnvElts cbc.bc_strs)
      itbls <- mirrorItbls src cbc.bc_itbls
      pure MirrorCompiledByteCode {mirrorBCOs = bcos, mirrorStrs = strs, mirrorItbls = itbls}

rehydrateStrs :: Rehydrate -> [(MirrorName, ShortByteString)] -> IO AddrEnv
rehydrateStrs ctx strs = do
  names <- traverse (rehydrateName ctx . fst) strs
  ptrs <- allocStrings ctx.hscEnv (fromShort . snd <$> strs)
  pure (mkNameEnv [(n, (n, p)) | (n, p) <- zip names ptrs])

rehydrateItbls :: Rehydrate -> MirrorItbls -> IO ItblEnv
rehydrateItbls ctx = \case
  MirrorItblsSerialized itbls ->
    mkNameEnv <$> for itbls \ (mn, t) -> do
      n <- rehydrateName ctx mn
      p <- allocConInfoTable ctx.hscEnv t
      pure (n, (n, p))
  MirrorItblsRebuild -> rebuildItbls ctx.hscEnv ctx.target

-- | Reconstructs a 'CompiledByteCode', allocating string literals and info tables in the importing process's
-- interpreter. 'bc_ffis'\/'bc_spt_entries' are empty and 'bc_breaks' is 'Nothing' (see module docs).
rehydrateCompiledByteCode :: Rehydrate -> MirrorCompiledByteCode -> IO CompiledByteCode
rehydrateCompiledByteCode env m = do
  bcos <- traverse (rehydrateUnlinkedBCO env) m.mirrorBCOs
  strs <- rehydrateStrs env m.mirrorStrs
  itbls <- rehydrateItbls env m.mirrorItbls
  pure CompiledByteCode {
    bc_bcos = fromList (fromIntegral (length bcos)) bcos,
    bc_itbls = itbls,
    bc_ffis = [],
    bc_strs = strs,
    bc_breaks = Nothing,
    bc_spt_entries = []
  }

data MirrorLinkablePart
  = MirrorBCOsPart MirrorCompiledByteCode
  | MirrorRawPart LinkablePart

-- | 'Nothing' if any part is a 'BCOs' with an unmirrorable 'CompiledByteCode', or a 'CoreBindings'\/'LazyBCOs'
-- part (see module docs) -- either aborts sharing for the *entire* enclosing 'Linkable'.
mirrorLinkablePart :: MirrorSource -> LinkablePart -> Maybe MirrorLinkablePart
mirrorLinkablePart src = \case
  BCOs cbc -> MirrorBCOsPart <$> mirrorCompiledByteCode src cbc
  part@(DotO _ _) -> Just (MirrorRawPart part)
  part@(DotA _) -> Just (MirrorRawPart part)
  part@(DotDLL _) -> Just (MirrorRawPart part)
  CoreBindings _ -> Nothing
  LazyBCOs _ _ -> Nothing

rehydrateLinkablePart :: Rehydrate -> MirrorLinkablePart -> IO LinkablePart
rehydrateLinkablePart env = \case
  MirrorBCOsPart mcbc -> BCOs <$> rehydrateCompiledByteCode env mcbc
  MirrorRawPart part -> pure part

data MirrorLinkable = MirrorLinkable {
  mirrorLinkableTime :: UTCTime,
  mirrorLinkableParts :: NonEmpty MirrorLinkablePart
}

-- | 'Nothing' if any part of the 'Linkable' is unmirrorable (see 'mirrorLinkablePart').
mirrorLinkable :: MirrorSource -> Linkable -> Maybe MirrorLinkable
mirrorLinkable src l = do
  parts <- traverse (mirrorLinkablePart src) l.linkableParts
  pure MirrorLinkable { mirrorLinkableTime = l.linkableTime, mirrorLinkableParts = parts }

-- | Reconstructs a 'Linkable' for the given (already-resolved, real) target 'Module' -- callers already know
-- this from the importing process's own home unit graph (see 'GhcServer.Build.SharedBytecode.bytecodeImportEntries'),
-- so there is no need to round-trip 'Module' identity through a 'MirrorModule'.
rehydrateLinkable :: HscEnv -> Module -> MirrorLinkable -> IO Linkable
rehydrateLinkable env target m = do
  ctx <- newRehydrate env target
  parts <- traverse (rehydrateLinkablePart ctx) m.mirrorLinkableParts
  pure Linkable { linkableTime = m.mirrorLinkableTime, linkableModule = target, linkableParts = parts }
