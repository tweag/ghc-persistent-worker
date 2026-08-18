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
-- * 'GHC.ByteCode.Types.bc_itbls'\/'bc_strs'\/'bc_ffis' cache 'GHCi.RemoteTypes.RemotePtr' addresses into the
--   *exporting* process's interpreter address space; these are meaningless in the importing process and are
--   dropped entirely (rehydrated as empty). This relies on GHC's own bytecode linker
--   ('GHC.Linker.Loader'\/'GHC.Runtime.Interpreter') re-resolving those addresses from 'Name's on demand when a
--   'CompiledByteCode' is actually linked for execution -- these tables are a cross-compile-unit lookup cache,
--   not part of the instruction encoding itself, so an empty cache only costs a few redundant lookups, not
--   correctness. This has not been verified against a real execute-subprocess run with data constructors from a
--   dependency module; treat as an unverified mitigation, not a proven fix.
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
import Data.List.NonEmpty (NonEmpty)
import Data.Time (UTCTime)
import Data.Word (Word16, Word8)
import GHC.Builtin.PrimOps (PrimOp)
import GHC.ByteCode.Types (BCOByteArray, BCONPtr (..), BCOPtr (..), CompiledByteCode (..), UnlinkedBCO (..))
import qualified GHC.Data.FastString as FastString
import GHC.Data.FlatBag (elemsFlatBag, fromList)
import GHC.Driver.Env (HscEnv, hsc_NC)
import GHC.Iface.Env (lookupNameCache)
import GHC.Linker.Types (Linkable (..), LinkablePart (..))
import qualified GHC.Types.Name as Name
import GHC.Types.Name (Name)
import qualified GHC.Types.Name.Cache as Name.Cache
import GHC.Types.Name.Env (emptyNameEnv)
import qualified GHC.Types.Name.Occurrence as Occ
import GHC.Types.Name.Occurrence (NameSpace, OccName)
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

-- | 'Name'-free surrogate for 'GHC.Types.Name.Name'. 'MirrorExternalName' covers every 'Name' actually expected
-- to appear in a fully-assembled 'UnlinkedBCO' (see module docs: top-level bindings, 'DataCon's, top-level
-- string-literal bindings are all 'External'\/'WiredIn'). 'MirrorLocalName' is a defensive fallback for any
-- 'Name' without a 'Module' (internal\/system names); such a 'Name' cannot be given a cross-process-stable
-- identity, so 'rehydrateName' mints a fresh local binder for it instead of attempting to match the original.
data MirrorName
  = MirrorExternalName MirrorModule MirrorOccNS String
  | MirrorLocalName MirrorOccNS String
  deriving stock (Show, Eq, Ord)

mirrorName :: Name -> MirrorName
mirrorName n =
  case Name.nameModule_maybe n of
    Just m ->
      let (ns, s) = mirrorOccName (Name.nameOccName n)
      in MirrorExternalName (mirrorModule m) ns s
    Nothing ->
      let (ns, s) = mirrorOccName (Name.nameOccName n)
      in MirrorLocalName ns s

-- | Reconstruct a 'Name' via the importing process's own 'GHC.Types.Name.Cache.NameCache'. For
-- 'MirrorExternalName', this returns the exact same 'Name' (same 'GHC.Types.Unique.Unique') that the importing
-- process's own interface-loading code would produce for that (module, 'OccName') pair -- see module docs.
rehydrateName :: HscEnv -> MirrorName -> IO Name
rehydrateName env = \case
  MirrorExternalName mm ns s ->
    lookupNameCache (hsc_NC env) (rehydrateModule mm) (rehydrateOccName (ns, s))
  MirrorLocalName ns s ->
    Name.mkSystemName <$> Name.Cache.takeUniqFromNameCache (hsc_NC env) <*> pure (rehydrateOccName (ns, s))

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

mirrorBCONPtr :: BCONPtr -> MirrorBCONPtr
mirrorBCONPtr = \case
  BCONPtrWord w -> MirrorBCONWord w
  BCONPtrLbl fs -> MirrorBCONLbl (FastString.unpackFS fs)
  BCONPtrItbl n -> MirrorBCONItbl (mirrorName n)
  BCONPtrAddr n -> MirrorBCONAddr (mirrorName n)
  BCONPtrStr bs -> MirrorBCONStr (Data.ByteString.unpack bs)

rehydrateBCONPtr :: HscEnv -> MirrorBCONPtr -> IO BCONPtr
rehydrateBCONPtr env = \case
  MirrorBCONWord w -> pure (BCONPtrWord w)
  MirrorBCONLbl s -> pure (BCONPtrLbl (FastString.mkFastString s))
  MirrorBCONItbl mn -> BCONPtrItbl <$> rehydrateName env mn
  MirrorBCONAddr mn -> BCONPtrAddr <$> rehydrateName env mn
  MirrorBCONStr ws -> pure (BCONPtrStr (Data.ByteString.pack ws))

-- | Mirror of 'GHC.ByteCode.Types.BCOPtr'. 'BCOPtrPrimOp' is kept verbatim (a plain enum, no pinned data, no
-- 'Name'). 'BCOPtrBreakArray' has no mirror -- its presence makes the enclosing 'UnlinkedBCO' (and hence the
-- whole 'CompiledByteCode'\/'Linkable') unmirrorable, see 'mirrorUnlinkedBCO'.
data MirrorBCOPtr
  = MirrorBCOName MirrorName
  | MirrorBCOPrimOp !PrimOp
  | MirrorBCOBCO MirrorUnlinkedBCO

-- | 'BCOPtrBreakArray' has no representation here: encountering one during mirroring aborts the whole
-- enclosing 'UnlinkedBCO' (returned as 'Nothing' from 'mirrorBCOPtr').
mirrorBCOPtr :: BCOPtr -> Maybe MirrorBCOPtr
mirrorBCOPtr = \case
  BCOPtrName n -> Just (MirrorBCOName (mirrorName n))
  BCOPtrPrimOp op -> Just (MirrorBCOPrimOp op)
  BCOPtrBCO bco -> MirrorBCOBCO <$> mirrorUnlinkedBCO bco
  BCOPtrBreakArray _ -> Nothing

rehydrateBCOPtr :: HscEnv -> MirrorBCOPtr -> IO BCOPtr
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
-- (including transitively, through nested 'BCOPtrBCO's).
mirrorUnlinkedBCO :: UnlinkedBCO -> Maybe MirrorUnlinkedBCO
mirrorUnlinkedBCO u = do
  ptrs <- traverse mirrorBCOPtr (elemsFlatBag u.unlinkedBCOPtrs)
  pure MirrorUnlinkedBCO {
    mirrorBCOName = mirrorName u.unlinkedBCOName,
    mirrorBCOArity = u.unlinkedBCOArity,
    mirrorBCOInstrs = u.unlinkedBCOInstrs,
    mirrorBCOBitmap = u.unlinkedBCOBitmap,
    mirrorBCOLits = mirrorBCONPtr <$> elemsFlatBag u.unlinkedBCOLits,
    mirrorBCOPtrs = ptrs
  }

rehydrateUnlinkedBCO :: HscEnv -> MirrorUnlinkedBCO -> IO UnlinkedBCO
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

-- | Mirror of 'GHC.ByteCode.Types.CompiledByteCode'. Only carries the top-level 'UnlinkedBCO's; see the
-- 'rehydrateCompiledByteCode' docs for what is dropped.
data MirrorCompiledByteCode = MirrorCompiledByteCode {
  mirrorBCOs :: [MirrorUnlinkedBCO]
}

-- | Mirror of 'GHC.ByteCode.Types.CompiledByteCode'. Drops 'bc_itbls'\/'bc_strs'\/'bc_ffis' (process-local
-- 'GHCi.RemoteTypes.RemotePtr' caches, rehydrated as empty; see module docs) and 'bc_spt_entries' (dropped
-- entirely, also see module docs). 'Nothing' if 'bc_breaks' is set (unmirrorable, see module docs) or if any
-- contained 'UnlinkedBCO' is unmirrorable.
mirrorCompiledByteCode :: CompiledByteCode -> Maybe MirrorCompiledByteCode
mirrorCompiledByteCode cbc
  | Just _ <- cbc.bc_breaks = Nothing
  | otherwise = do
      bcos <- traverse mirrorUnlinkedBCO (elemsFlatBag cbc.bc_bcos)
      pure MirrorCompiledByteCode { mirrorBCOs = bcos }

-- | Reconstructs a 'CompiledByteCode' with empty 'bc_itbls'\/'bc_strs'\/'bc_ffis'\/'bc_spt_entries' and no
-- breakpoints ('bc_breaks' = 'Nothing'). See module docs for why this is expected to be safe: those tables are
-- re-populated by GHC's own bytecode linker from 'Name's on demand, not baked into the instruction encoding.
rehydrateCompiledByteCode :: HscEnv -> MirrorCompiledByteCode -> IO CompiledByteCode
rehydrateCompiledByteCode env m = do
  bcos <- traverse (rehydrateUnlinkedBCO env) m.mirrorBCOs
  pure CompiledByteCode {
    bc_bcos = fromList (fromIntegral (length bcos)) bcos,
    bc_itbls = emptyNameEnv,
    bc_ffis = [],
    bc_strs = emptyNameEnv,
    bc_breaks = Nothing,
    bc_spt_entries = []
  }

data MirrorLinkablePart
  = MirrorBCOsPart MirrorCompiledByteCode
  | MirrorRawPart LinkablePart

-- | 'Nothing' if any part is a 'BCOs' with an unmirrorable 'CompiledByteCode', or a 'CoreBindings'\/'LazyBCOs'
-- part (see module docs) -- either aborts sharing for the *entire* enclosing 'Linkable'.
mirrorLinkablePart :: LinkablePart -> Maybe MirrorLinkablePart
mirrorLinkablePart = \case
  BCOs cbc -> MirrorBCOsPart <$> mirrorCompiledByteCode cbc
  part@(DotO _ _) -> Just (MirrorRawPart part)
  part@(DotA _) -> Just (MirrorRawPart part)
  part@(DotDLL _) -> Just (MirrorRawPart part)
  CoreBindings _ -> Nothing
  LazyBCOs _ _ -> Nothing

rehydrateLinkablePart :: HscEnv -> MirrorLinkablePart -> IO LinkablePart
rehydrateLinkablePart env = \case
  MirrorBCOsPart mcbc -> BCOs <$> rehydrateCompiledByteCode env mcbc
  MirrorRawPart part -> pure part

data MirrorLinkable = MirrorLinkable {
  mirrorLinkableTime :: UTCTime,
  mirrorLinkableParts :: NonEmpty MirrorLinkablePart
}

-- | 'Nothing' if any part of the 'Linkable' is unmirrorable (see 'mirrorLinkablePart').
mirrorLinkable :: Linkable -> Maybe MirrorLinkable
mirrorLinkable l = do
  parts <- traverse mirrorLinkablePart l.linkableParts
  pure MirrorLinkable { mirrorLinkableTime = l.linkableTime, mirrorLinkableParts = parts }

-- | Reconstructs a 'Linkable' for the given (already-resolved, real) target 'Module' -- callers already know
-- this from the importing process's own home unit graph (see 'GhcServer.Build.SharedBytecode.bytecodeImportEntries'),
-- so there is no need to round-trip 'Module' identity through a 'MirrorModule'.
rehydrateLinkable :: HscEnv -> Module -> MirrorLinkable -> IO Linkable
rehydrateLinkable env target m = do
  parts <- traverse (rehydrateLinkablePart env) m.mirrorLinkableParts
  pure Linkable { linkableTime = m.mirrorLinkableTime, linkableModule = target, linkableParts = parts }
