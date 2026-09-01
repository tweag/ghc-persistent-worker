{-# LANGUAGE ViewPatterns, CPP, OverloadedStrings, PatternSynonyms, RankNTypes #-}

module Internal.Compile.Make where

import Data.IORef (readIORef)
import qualified GHC
import GHC (
  DynFlags (..),
  GeneralFlag (..),
  Ghc,
  GhcException (..),
  GhcMonad (..),
  IsBootInterface (..),
  ModIface,
  ModLocation (..),
  pattern ModLocation,
  ModSummary (..),
  Module,
  gopt,
  mgLookupModule,
  )
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Driver.DynFlags (gopt_set)
import GHC.Driver.Env (HscEnv (..), hscInsertHPT)
import GHC.Driver.Env.Types (Hsc (..))
import GHC.Driver.Errors.Types (GhcMessage (..))
import GHC.Driver.Hooks (Hooks (..))
import GHC.Driver.Main (hscParse', tcRnModule')
import GHC.Driver.Make (summariseFile)
import GHC.Driver.Pipeline (compileOne, runPhase)
import GHC.Driver.Pipeline.Phases (PhaseHook (..), TPhase (..))
import GHC.Runtime.Loader (initializeSessionPlugins)
import GHC.Tc.Types (FrontendResult (..))
import GHC.Unit.Env (ue_unsafeHomeUnit)
import GHC.Unit.Home.ModInfo (HomeModInfo (..), HomeModLinkable (..))
import GHC.Utils.Monad (MonadIO (..))
import GHC.Utils.Outputable (ppr, showPprUnsafe, text, (<+>))
import GHC.Utils.Panic (throwGhcExceptionIO)
import GHC.Utils.TmpFs (TmpFs, cleanCurrentModuleTempFiles, keepCurrentModuleTempFiles)
import Internal.Compat.GHC914 (hscModuleGraph)
import Internal.Compile.TopEnv (patchTopEnv, withCaptureTopEnv)
import Internal.Debug (pprModuleFull)
import Internal.Error (eitherMessages, noteGhc)
import Internal.Log (logTimedD)
import System.OsPath.Extra (fromOsPath)
import Types.Instrument (Event (..))
import Types.Log (Logger (..))
import Types.Target (ModuleTarget (..), Target (..), TargetSpec (..), renderTargetSpec)

#if FIXED_NODES

import Control.Exception (finally)
import GHC.Unit.Module.Graph (ModuleNodeInfo (..))

#endif

-- | Update the location of the result of @summariseFile@ to point to the locations specified on the command line, since
-- these are placed in the source file's directory by that function.
setHiLocation :: HscEnv -> ModSummary -> ModSummary
setHiLocation
  HscEnv {hsc_dflags = DynFlags {outputHi = Just ml_hi_file, outputFile_ = Just ml_obj_file}}
  summ@ModSummary {ms_location = ModLocation {ml_obj_file = _, ml_hi_file = _, ..}}
  =
  summ {ms_location = ModLocation {ml_hi_file, ml_obj_file, ..}}
  where
setHiLocation _ summ = summ

cleanCurrentModuleTempFilesMaybe :: MonadIO m => GHC.Logger -> TmpFs -> DynFlags -> m ()
cleanCurrentModuleTempFilesMaybe logger tmpfs dflags =
  if gopt Opt_KeepTmpFiles dflags
    then liftIO $ keepCurrentModuleTempFiles logger tmpfs
    else liftIO $ cleanCurrentModuleTempFiles logger tmpfs

computeSummary ::
  Logger ->
  HscEnv ->
  FilePath ->
  IO ModSummary
computeSummary logger hsc_env src = do
    logTimedD logger ("Computing fresh ModSummary for" <+> text src) do
      summResult <- summariseFile hsc_env (ue_unsafeHomeUnit (hsc_unit_env hsc_env)) mempty src Nothing Nothing
      setHiLocation hsc_env <$> eitherMessages GhcDriverMessage summResult

-- | Find a module in the module graph and return its `ModSummary`.
lookupSummary ::
  Logger ->
  HscEnv ->
  Module ->
  IO ModSummary
lookupSummary _logger hsc_env target =
  check =<< noteGhc notFound (mgLookupModule (hscModuleGraph hsc_env) target)
  where
    notFound =
      "Could not find ModSummary in the module graph for "
      ++
      showPprUnsafe (pprModuleFull target NotBoot)

#if FIXED_NODES
    check = \case
      ModuleNodeCompile ms -> pure ms
      ModuleNodeFixed _ OsPathModLocation {ml_hs_file_ospath} ->
        case ml_hs_file_ospath of
          Just src ->
            computeSummary _logger hsc_env (fromOsPath src)
          Nothing ->
            throwGhcExceptionIO (PprProgramError "Fixed node without source path" (ppr target))
#else
    check = pure
#endif

-- | Obtain a `ModSummary` for the current target.
-- If the target was specified by module name, we assume that the new workflow is used, in which the module graph is
-- fully initialized in the metadata request, and look it up there.
--
-- Otherwise, the source file path is used to generate a fresh summary.
ensureSummary ::
  Logger ->
  HscEnv ->
  TargetSpec ->
  IO ModSummary
ensureSummary logger hsc_env = \case
  TargetModule (ModuleTarget m) -> do
    logTimedD logger ("Fetching ModSummary for" <+> ppr m <+> "from module graph") do
      lookupSummary logger hsc_env m
  TargetModuleInterp (ModuleTarget m) -> do
    logTimedD logger ("Fetching ModSummary for" <+> ppr m <+> "from module graph") do
      lookupSummary logger hsc_env m
  TargetSource (Target src) -> do
    computeSummary logger hsc_env (fromOsPath src)
  TargetUnit unit ->
    throwGhcExceptionIO (PprProgramError "Specified target unit for compile request" (ppr unit))
  TargetUnknown spec ->
    throwGhcExceptionIO (PprProgramError "Invalid target spec using TargetUnknown" (text spec))

-- | Compile a module with multiple home units in the session state, using the home package table to look up
-- dependencies.
--
-- First, update the current unit's configuration to include this module's dependencies.
-- Buck only provides @-package@ flags for deps that are used by a given module, while the unit state is designed to be
-- initialized up front with the deps of all modules.
-- Note: This should soon be obsolete, since we now have full control over the metadata step.
--
-- Next, perform the steps that usually happen in make mode's upsweep:
-- - Create a @ModSummary@ using @summariseFile@
-- - Call the module compilation function @compileOne@
-- - Store the resulting @HomeModInfo@ in the current unit's home package table.
compileModuleWithDepsInHpt ::
  Logger ->
  -- | Sink for instrumentation events. Called with a 'PhaseEvent' for each 'GHC.Driver.Pipeline.Phases.TPhase'
  -- that 'phaseLabel' names, in addition to whatever other event kinds a future caller wants to report.
  (Event -> IO ()) ->
  -- | Id correlating this compilation's events (see 'withPhase'), allocated once per compilation job\/task
  -- dispatch by the caller (see @GhcServer.Build.Propagate.nextRequestId@, @GhcWorker.Instrumentation@'s
  -- @allocRequestId@).
  Int ->
  TargetSpec ->
  Ghc (Maybe ModIface)
compileModuleWithDepsInHpt logger emitEvent requestId target =
  logTimedD logger "Compiling" do
    initializeSessionPlugins
    hsc_env <- getSession
    hmi <- liftIO do
      summary <- ensureSummary logger hsc_env target
      (hsc_env', captured) <- prepareCapture hsc_env
      result <-
        compileOne (withFrontendEvents emitEvent requestId target (withPhaseEvents emitEvent requestId target hsc_env')) (forceRecomp summary) 1 100000 Nothing
          (HomeModLinkable Nothing Nothing)
      cleanCurrentModuleTempFilesMaybe (hsc_logger hsc_env') (hsc_tmpfs hsc_env') summary.ms_hspp_opts
      applyCapture captured result
    liftIO $ hscInsertHPT hmi hsc_env
    pure (Just hmi.hm_iface)
  where
    -- This bypasses another recompilation check in 'compileOne'
    forceRecomp summary =
      summary {ms_hspp_opts = gopt_set summary.ms_hspp_opts Opt_ForceRecomp}

    -- For 'TargetModuleInterp', install a static plugin that captures the module's real top-level environment during
    -- typechecking, so it can be used to patch @mi_top_env@ on the resulting interface after compilation.
    prepareCapture hsc_env
      | TargetModuleInterp _ <- target = fmap Just <$> withCaptureTopEnv hsc_env
      | otherwise = pure (hsc_env, Nothing)

    applyCapture captured hmi =
      case captured of
        Just ref -> do
          topEnv <- readIORef ref
          pure $ maybe hmi (\ env -> hmi {hm_iface = patchTopEnv env hmi.hm_iface}) topEnv
        Nothing -> pure hmi

-- | Names the pipeline phases that are reported as 'PhaseEvent's, matching the phases GHC always runs for a single
-- module compilation with any backend: type/instance-checking and desugaring ('T_Hsc'), the post-typecheck backend
-- action selection e.g. "needs code generation" vs. "interface only" ('T_HscPostTc'), and code generation proper
-- ('T_HscBackend'). Every other 'TPhase' constructor (preprocessing, assembling, linking, ...) is not reported.
phaseLabel :: TPhase a -> Maybe String
phaseLabel = \case
  T_HsPp {} -> Just "cpp"
  T_Hsc {} -> Just "main"
  T_HscPostTc {} -> Just "simplify"
  T_HscBackend {} -> Just "backend"
  _ -> Nothing

-- | Report a 'Types.Instrument.PhaseStart'\/'Types.Instrument.PhaseEnd' pair of 'Event's around running @act@,
-- recording the wall-clock duration (in milliseconds) between the two. This is the single combinator used by every
-- phase-observing hook in this module (see 'withPhaseEvents', 'frontendEvents'); it deliberately does not guarantee
-- that 'PhaseEnd' is emitted if @act@ throws, since one of its use sites runs in GHC's 'Hsc' monad, which has no
-- 'GHC.Utils.Exception.ExceptionMonad' instance (its internal warning-message state can't survive being caught) and
-- therefore cannot support exception-safe cleanup in general.
withPhase ::(Event -> IO ()) -> Int -> TargetSpec -> String -> IO a -> IO a
withPhase emitEvent requestId target phase act = do
  emitEvent PhaseStart {target = renderTargetSpec target, phase, requestId}
  startNs <- getMonotonicTimeNSec
  result <- finally act do
    endNs <- getMonotonicTimeNSec
    emitEvent PhaseEnd {target = renderTargetSpec target, durationMs = fromIntegral ((endNs - startNs) `div` 1_000_000), requestId}
  pure result

-- | Install a 'GHC.Driver.Hooks.runPhaseHook' on the given 'HscEnv' that reports a 'PhaseEvent' for each phase named
-- by 'phaseLabel'.
withPhaseEvents :: (Event -> IO ()) -> Int -> TargetSpec -> HscEnv -> HscEnv
withPhaseEvents emitEvent requestId target hsc_env =
  hsc_env {hsc_hooks = (hsc_hooks hsc_env) {runPhaseHook = Just (PhaseHook run)}}
  where
    run :: forall a . TPhase a -> IO a
    run tPhase =
      wrap tPhase (runPhase tPhase)

    wrap :: forall a . TPhase a -> IO a -> IO a
    wrap tPhase =
      maybe id (withPhase emitEvent requestId target) (phaseLabel tPhase)

unliftHsc :: (forall a . IO a -> IO a) -> Hsc b -> Hsc b
unliftHsc f (Hsc hsc) =
  Hsc \ e m -> f (hsc e m)

frontendEvents :: (Event -> IO ()) -> Int -> TargetSpec -> ModSummary -> Hsc FrontendResult
frontendEvents emitEvent requestId target mod_summary = do
  hpm <- (phase "parse" (hscParse' mod_summary))
  FrontendTypecheck <$> phase "typecheck" (tcRnModule' mod_summary False hpm)
  where
    phase :: String -> Hsc a -> Hsc a
    phase name = unliftHsc (withPhase emitEvent requestId target name)

withFrontendEvents :: (Event -> IO ()) -> Int -> TargetSpec -> HscEnv -> HscEnv
withFrontendEvents emitEvent requestId target hsc_env =
  hsc_env {hsc_hooks = (hsc_hooks hsc_env) {hscFrontendHook = Just (frontendEvents emitEvent requestId target)}}
