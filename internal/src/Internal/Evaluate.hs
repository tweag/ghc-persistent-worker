{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Internal.Evaluate where

import Control.Concurrent (withMVar)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (for_)
import Data.Maybe (mapMaybe)
import GHC (Ghc, getSession, getSessionDynFlags, moduleNameString, runTcInteractive, setInteractiveDynFlags, setSession)
import GHC.Builtin.Types (boolTy, charTy, doubleTy, floatTy, intTy, integerTy, stringTy)
import GHC.Core.TyCo.Compare (eqType)
import GHC.Core.Type (Type)
import GHC.Data.Bag (emptyBag)
import GHC.Driver.Config (initEvalOpts)
import GHC.Driver.DynFlags (wopt_unset)
import GHC.Driver.Env (hscInterp, hscSetActiveUnitId, hsc_home_unit, hsc_interp, mkInteractiveHscEnv, runInteractiveHsc)
import GHC.Driver.Env.Types (HscEnv (hsc_IC), hsc_mod_graph)
import GHC.Driver.Errors.Types (hoistTcRnMessage)
import GHC.Driver.Flags (WarningFlag (Opt_WarnUnusedLocalBinds))
import GHC.Driver.Main (hscParseStmtWithLocation, hscParsedStmt, ioMsgMaybe)
import GHC.Driver.Monad (GhcMonad)
import GHC.Hs.Extension (GhcPs)
import GHC.Iface.Load (loadSrcInterface)
import GHC.Runtime.Context (InteractiveContext (..), InteractiveImport (..))
import GHC.Runtime.Eval (execOptions, exprType, setContext)
import GHC.Runtime.Eval.Types (ExecOptions (..), IcGlobalRdrEnv (..), isStep)
import GHC.Runtime.Interpreter (evalStmt, wormhole)
import GHC.Tc.Module (TcRnExprMode (TM_Inst))
import GHC.Tc.Utils.Env (lookupGlobal)
import GHC.Tc.Utils.TcType (tcSplitIOType_maybe)
import GHC.Types.Avail (AvailInfo (..))
import GHC.Types.Name (nameOccName)
import GHC.Types.Name.Occurrence (OccName, mkOccEnv, mkVarOcc)
import GHC.Types.Name.Reader (
  GREInfo,
  GlobalRdrEltX (..),
  GlobalRdrEnvX,
  IfGlobalRdrEnv,
  Parent (NoParent),
  hydrateGlobalRdrEnv,
  plusGlobalRdrEnv,
  )
import GHC.Types.PkgQual (PkgQual (NoPkgQual, ThisPkg))
import GHC.Types.TyThing (tyThingGREInfo)
import GHC.Unit (moduleUnitId)
import GHC.Unit.Finder qualified as Finder
import GHC.Unit.Finder.Types (FindResult (..))
import GHC.Unit.Home (homeUnitId)
import GHC.Unit.Module.ModIface (mi_exports)
import GHC.Unit.Types (IsBootInterface (..), moduleName)
import GHC.Utils.Outputable (ppr, text, (<+>))
import GHCi.Message (EvalResult (..), EvalStatus_ (..))
import GHCi.RemoteTypes (ForeignHValue, HValueRef)
import Internal.Cache.Hpt (loadHomeUnit)
import Internal.Log (logDebugD, logTimed)
import Language.Haskell.Syntax.Expr (GhciLStmt)
import Language.Haskell.Syntax.Module.Name (ModuleName (..), mkModuleName)
import System.OsPath.Extra (toOsPath)
import Types.Env (Env (..))
import Types.State (WorkerState (..))
import Types.State.Make (EModuleGraph (..), MakeState (..))
import Types.Target (ModuleTarget (..))
import Unsafe.Coerce (unsafeCoerce)

-- | Load a target's home unit into the current session, activate its unit id, and refresh interactive
-- 'DynFlags' so subsequent evaluation runs against the freshly loaded state. Returns 'Nothing' when no home
-- unit was configured for the calling context (e.g. no @--home-unit@ was passed to the worker), in which case
-- the caller cannot proceed with evaluation at all.
setupEvaluationSession :: Env -> Maybe String -> ModuleTarget -> Ghc (Maybe HscEnv)
setupEvaluationSession _ Nothing _ = pure Nothing
setupEvaluationSession env (Just homeUnit) target = do
  hsc_env0 <- getSession
  dflags0 <- getSessionDynFlags
  hsc_env2 <- liftIO $ withMVar env.state \ state -> do
    (_, hsc_env1) <-
      loadHomeUnit env.log dflags0 (moduleUnitId target.module_) (state, hsc_env0) (toOsPath homeUnit)
    pure hsc_env1 {hsc_mod_graph = state.make.moduleGraphState.moduleGraph}
  let hsc_env = hscSetActiveUnitId (moduleUnitId target.module_) hsc_env2
  setSession hsc_env
  dflags <- getSessionDynFlags
  setInteractiveDynFlags dflags
  pure (Just hsc_env)

-- | Reasons 'GHC.Unit.Finder.findImportedModule' can fail to resolve a target module, retained distinctly so
-- callers can report an actionable message rather than a generic failure.
data ModuleLookupError
  = ModuleNotFound
  | ModulePackageNotFound
  | ModuleAmbiguous

-- | Render a 'ModuleLookupError' into a human-readable message naming the module that could not be resolved.
renderModuleLookupError :: ModuleName -> ModuleLookupError -> String
renderModuleLookupError modname = \case
  ModuleNotFound -> "module not found: " ++ moduleNameString modname
  ModulePackageNotFound -> "package not found for module " ++ moduleNameString modname
  ModuleAmbiguous -> "multiple candidate modules found for " ++ moduleNameString modname

-- | Confirm that a target module can actually be located by GHC's finder within the given package qualifier.
-- Callers only need the yes/no answer (they already have the 'ModuleName' they're resolving), so the located
-- 'GHC.Unit.Finder.Types.FindResult' payload is discarded.
resolveTargetModule :: HscEnv -> ModuleName -> PkgQual -> IO (Either ModuleLookupError ())
resolveTargetModule hsc_env modname pkgqual = do
  result <- Finder.findImportedModule hsc_env modname pkgqual
  pure case result of
    Found _ _ -> Right ()
    NoPackage _ -> Left ModulePackageNotFound
    FoundMultiple _ -> Left ModuleAmbiguous
    NotFound {} -> Left ModuleNotFound

evaluate :: Env -> Maybe String -> ModuleTarget -> [String] -> String -> Ghc Bool
evaluate env mHomeUnit target imports expr =
  logTimed env.log "evaluate is called" do
    setupEvaluationSession env mHomeUnit target >>= \case
      Nothing -> pure False
      Just hsc_env -> do
        let modname = moduleName target.module_
            pkgqual = ThisPkg (homeUnitId (hsc_home_unit hsc_env))
        liftIO (resolveTargetModule hsc_env modname pkgqual) >>= \case
          Left err -> logDebugD env.log (text (renderModuleLookupError modname err)) >> pure False
          Right () -> do
            setContext [IIModule modname]

            for_ imports \ imp ->
              loadImport env (mkModuleName imp) >>= \case
                Left _ -> pure ()
                Right rdr_env -> updateGlobalRdrEnv env rdr_env

            evalStmtCustom expr execOptions >>= \case
              EvalComplete _ (EvalSuccess (fhv : _)) -> do
                let Just interp = hsc_interp hsc_env
                hv <- liftIO (wormhole interp fhv)
                let (_total, failed) = unsafeCoerce hv :: (Int, Int)
                pure (failed == 0)
              _ -> pure False

-- | Classifies a module's @main@ result type for return-value propagation. 'IO a' where @a@ is a stringly or
-- numeric type has its value surfaced (via 'ResultString'/'ResultShowable', see 'executeMain'); any other result
-- type (in particular, the common @main :: IO ()@) is not, and evaluation falls back to running @main@ bare with
-- its result discarded, as before this feature was added.
data MainResultKind
  = -- | @main :: IO String@ -- printed directly (no quoting/escaping) via @putStr@.
    ResultString
  | -- | @main :: IO a@ for another stringly/numeric @a@ (currently: 'Int', 'Integer', 'Double', 'Float', 'Char',
    -- 'Bool') -- printed via @print@ (i.e. its 'Show' instance).
    ResultShowable

-- | Inspect a type of the shape @IO a@ (as returned by 'GHC.Runtime.Eval.exprType') and classify @a@ per
-- 'MainResultKind', or 'Nothing' if the type isn't @IO@-shaped at all or @a@ isn't one of the small set of
-- wired-in stringly/numeric types checked here. Deliberately does not attempt to support arbitrary
-- user-defined 'Show' instances (e.g. custom result records) -- that would require a real type-directed dispatch
-- mechanism (dictionary-passing), not just an equality check against wired-in types, which is a substantially
-- larger feature.
classifyMainResultType :: Type -> Maybe MainResultKind
classifyMainResultType ty = do
  (_ioTyCon, resTy) <- tcSplitIOType_maybe ty
  if eqType resTy stringTy
    then Just ResultString
    else if any (eqType resTy) ([intTy, integerTy, doubleTy, floatTy, charTy, boolTy] :: [Type])
      then Just ResultShowable
      else Nothing

-- | Execute a module's exported @main@ binding via GHC's statement-evaluation machinery, mirroring the worker's
-- @--expr@ mode (see 'GhcWorker.GhcHandler.dispatch'\'s @ModeEval@ branch). Unlike 'evaluate' (which unsafely
-- coerces its statement's bound value to a fixed test-harness type, @(Int, Int)@), this coerces to 'String' --
-- see 'classifyMainResultType' for how the statement text is chosen so that the bound value's runtime
-- representation actually is a 'String' whenever a result is exfiltrated at all.
--
-- Distinguishes two categories of non-execution, which used to be conflated in a single 'Nothing' result
-- (see the mitigation for the "silent failure" gap documented in @kb-instrument-ui@'s execute-feature section):
--
-- * 'Left' reason -- a genuine setup failure: the module's home unit could not be resolved, or
--   'GHC.Unit.Finder.findImportedModule' could not locate the target module at all. These are real,
--   actionable problems that the caller ('GhcServer.Build.Execute.executeModuleTask') should surface as a
--   failed task, not a silent skip.
-- * 'Right' 'Nothing' -- the module was found but does not export a binding named @main@. This remains a
--   deliberate, silent skip (not an error): plenty of modules in a project have no @main@, and that is not a
--   failure worth reporting.
-- * 'Right' ('Just' (success, mResult)) -- @main@ was actually invoked; @success@ reports whether the
--   statement evaluation completed, and @mResult@ carries the exfiltrated result value when @main@'s result
--   type was recognized by 'classifyMainResultType' (via @main@ directly for 'ResultString', or via
--   @fmap show main@ for 'ResultShowable' -- letting GHC's own typechecker perform the 'Show' dispatch rather
--   than reflecting on the runtime value ourselves).
executeMain :: Env -> Maybe String -> ModuleTarget -> Ghc (Either String (Maybe (Bool, Maybe String)))
executeMain env mHomeUnit target =
  logTimed env.log "executeMain is called" do
    setupEvaluationSession env mHomeUnit target >>= \case
      Nothing -> pure (Left "executeMain: no home unit configured for execute target")
      Just hsc_env -> do
        let modname = moduleName target.module_
            pkgqual = ThisPkg (homeUnitId (hsc_home_unit hsc_env))
        liftIO (resolveTargetModule hsc_env modname pkgqual) >>= \case
          Left err -> pure (Left ("executeMain: " ++ renderModuleLookupError modname err))
          Right () -> do
            hasMain <- liftIO (moduleHasMain hsc_env modname pkgqual)
            if not hasMain
              then pure (Right Nothing)
              else runMain hsc_env modname
  where
    -- Inspect @main@'s result type (post-typecheck, via GHC's own @:type@-style machinery) to decide whether its
    -- return value can be usefully exfiltrated. Only a small set of wired-in stringly/numeric types is
    -- recognized (see 'classifyMainResultType'); anything else (in particular the ordinary @main :: IO ()@)
    -- falls back to running @main@ bare, discarding its result as before. When a result type is recognized, the
    -- statement text is chosen so that the statement's own bound value already has runtime representation
    -- 'String' (either @main@ itself, for 'ResultString', or @fmap show main@, for 'ResultShowable' -- 'show' is
    -- dispatched by GHC's typechecker while type-checking this very statement, not by us), letting the bound
    -- 'ForeignHValue' be unsafely coerced directly to 'String' rather than printed.
    runMain hsc_env modname = do
      setContext [IIModule modname]
      mty <- exprType TM_Inst "main"
      let mkind = classifyMainResultType mty
          -- A bind statement ('execResult <- ...'), not a bare expression statement: GHC's interactive
          -- statement typechecker ('GHC.Tc.Module.tcUserStmt') unconditionally attempts to print the result
          -- of a bare expression statement whenever its type isn't '()', regardless of whether the expression
          -- itself has type 'IO a' -- so evaluating a bare "main" of type 'IO String' would both exfiltrate
          -- the result below *and* print it a second time (via GHC's injected 'print') to the real stdout,
          -- which is captured alongside the module's own output. A bind statement only prints its bound value
          -- when 'Opt_PrintBindResult' is set, which this driver never sets.
          stmtText = case mkind of
            Just ResultShowable -> "execResult <- fmap show main"
            _ -> "execResult <- main"
      evalStmtCustom stmtText execOptions >>= \case
        EvalComplete _ (EvalSuccess (fhv : _)) -> do
          mResultStr <- case mkind of
            Nothing -> pure Nothing
            Just _ -> do
              let Just interp = hsc_interp hsc_env
              hv <- liftIO (wormhole interp fhv)
              pure (Just (unsafeCoerce hv :: String))
          pure (Right (Just (True, mResultStr)))
        _ -> pure (Right (Just (False, Nothing)))

-- | Check whether a module's interface exports a binding named @main@.
moduleHasMain :: HscEnv -> ModuleName -> PkgQual -> IO Bool
moduleHasMain hsc_env modname pkgqual = do
  iface <-
    runInteractiveHsc hsc_env $
      ioMsgMaybe $ hoistTcRnMessage $ GHC.runTcInteractive hsc_env $
        loadSrcInterface (text "checking for main") modname NotBoot pkgqual
  pure (any isMain (mi_exports iface))
  where
    isMain (Avail n) = nameOccName n == mkVarOcc "main"
    isMain (AvailTC _ _) = False

loadImport :: Env -> ModuleName -> Ghc (Either String (GlobalRdrEnvX GREInfo))
loadImport env modname = do
  hsc_env <- getSession
  logDebugD env.log ("try to import" <+> ppr modname)
  liftIO $ Finder.findImportedModule hsc_env modname NoPkgQual >>= \case
    Found _ _ -> do
      all_env <-
            liftIO
          $ runInteractiveHsc hsc_env
          $ ioMsgMaybe $ hoistTcRnMessage $ GHC.runTcInteractive hsc_env
          $ do
            iface <- loadSrcInterface (text "imported by GHCi") modname NotBoot NoPkgQual
            let es :: [AvailInfo]
                es = mi_exports iface

                convert (Avail n) = Just (nameOccName n, [GRE {gre_name = n, gre_par = NoParent, gre_lcl = True, gre_imp = emptyBag, gre_info = ()}])
                convert (AvailTC _ _) = Nothing

                converted :: [(OccName, [GlobalRdrEltX ()])]
                converted = mapMaybe convert es
                exports :: IfGlobalRdrEnv
                exports = mkOccEnv converted

                get_GRE_info nm = tyThingGREInfo <$> lookupGlobal hsc_env nm
                exports_env = hydrateGlobalRdrEnv get_GRE_info exports
            pure exports_env
      pure (Right all_env)
    _ -> do
      logDebugD env.log ("failed to import" <+> ppr modname)
      pure (Left ("error importing " ++ moduleNameString modname))

updateGlobalRdrEnv :: Env -> GlobalRdrEnvX GREInfo -> Ghc ()
updateGlobalRdrEnv _env rdr_env = do
  hsc_env <- getSession
  let old_ic = hsc_IC hsc_env
      -- Merges into the existing import environment rather than overwriting it, so that imports accumulated
      -- across multiple 'loadImport' calls (see 'evaluate') all remain visible.
      extendImportEnv igre import_env = igre {igre_env = import_env `plusGlobalRdrEnv` igre_env igre}
      final_gre_cache = ic_gre_cache old_ic `extendImportEnv` rdr_env
  setSession hsc_env {hsc_IC = old_ic {ic_gre_cache = final_gre_cache}}

-- | Dump the current interactive context's global reader environment to the debug log. Not called anywhere in
-- the current codebase; kept as a diagnostic tool for interactively debugging import/scope issues surfaced by
-- 'loadImport'/'updateGlobalRdrEnv'.
checkGlobalRdrEnv :: Env -> Ghc ()
checkGlobalRdrEnv env = do
  hsc_env <- getSession
  let rdr_env = igre_env (ic_gre_cache (hsc_IC hsc_env))
  logDebugD env.log (text "==== checkGlobalRdrEnv ====")
  logDebugD env.log (ppr rdr_env)

-- | Run a statement in the current interactive context.
evalStmtCustom ::
  GhcMonad m =>
  -- | a statement (bind or expression)
  String ->
  ExecOptions ->
  m (EvalStatus_ [ForeignHValue] [HValueRef])
evalStmtCustom input exec_opts@ExecOptions{..} = do
    hsc_env <- getSession
    liftIO (runInteractiveHsc hsc_env (hscParseStmtWithLocation execSourceFile execLineNumber input)) >>= \case
      -- Empty statement / comment: nothing to run, so trivially succeed with no bound values.
      Nothing -> pure (EvalComplete 0 (EvalSuccess []))
      Just stmt -> evalStmt' stmt input exec_opts

-- | Evaluate a single parsed statement outside of GHCi's full REPL loop. Unlike
-- 'GHC.Runtime.Eval.execStmt'', this does not update the session's 'InteractiveContext' with the statement's
-- bound identifiers or fixity declarations afterwards: every caller in this module runs at most one statement
-- per session (either a one-shot test expression in 'evaluate', or a module's @main@ in 'executeMain'), so there
-- is no subsequent statement in the same session that would need to see those bindings.
evalStmt' :: GhcMonad m => GhciLStmt GhcPs -> String -> ExecOptions -> m (EvalStatus_ [ForeignHValue] [HValueRef])
evalStmt' stmt _stmt_text ExecOptions{..} = do
    hsc_env <- getSession
    let interp = hscInterp hsc_env

    -- Turn off -fwarn-unused-local-binds when running a statement, to hide warnings about the implicit bindings
    -- introduced by the statement's own desugaring.
    let idflags' = ic_dflags hsc_env.hsc_IC `wopt_unset` Opt_WarnUnusedLocalBinds
        hsc_env' = mkInteractiveHscEnv hsc_env {hsc_IC = hsc_env.hsc_IC {ic_dflags = idflags'}}

    liftIO $ hscParsedStmt hsc_env' stmt >>= \case
      -- Empty statement / comment: nothing to run, so trivially succeed with no bound values.
      Nothing -> pure (EvalComplete 0 (EvalSuccess []))
      Just (_ids, hval, _fix_env) -> liftIO do
        let eval_opts = initEvalOpts idflags' (isStep execSingleStep)
        evalStmt interp eval_opts (execWrap hval)

