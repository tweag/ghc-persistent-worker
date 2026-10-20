{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Internal.Evaluate where

import Control.Concurrent (withMVar)
import Control.Monad.IO.Class (liftIO)
import GHC (Ghc, getSession, getSessionDynFlags, moduleNameString, runTcInteractive, setInteractiveDynFlags, setSession)
import GHC.Builtin.Types (boolTy, charTy, doubleTy, floatTy, intTy, integerTy, mkBoxedTupleTy, stringTy)
import GHC.Core.TyCo.Compare (eqType)
import GHC.Core.Type (Type)
import GHC.Driver.Config (initEvalOpts)
import GHC.Driver.DynFlags (wopt_unset)
import GHC.Driver.Env (hscInterp, hscSetActiveUnitId, hsc_interp, mkInteractiveHscEnv, runInteractiveHsc)
import GHC.Driver.Env.Types (HscEnv (hsc_IC), hsc_mod_graph)
import GHC.Driver.Errors.Types (hoistTcRnMessage)
import GHC.Driver.Flags (WarningFlag (Opt_WarnUnusedLocalBinds))
import GHC.Driver.Main (hscParseStmtWithLocation, hscParsedStmt, ioMsgMaybe)
import GHC.Driver.Monad (GhcMonad)
import GHC.Hs.Extension (GhcPs)
import GHC.Hs.ImpExp (simpleImportDecl)
import GHC.Iface.Load (loadSrcInterface)
import GHC.Runtime.Context (InteractiveContext (..), InteractiveImport (..))
import GHC.Runtime.Eval (execOptions, exprType, setContext)
import GHC.Runtime.Eval.Types (ExecOptions (..), isStep)
import GHC.Runtime.Interpreter (evalStmt, wormhole)
import GHC.Tc.Module (TcRnExprMode (TM_Inst))
import GHC.Tc.Utils.TcType (tcSplitIOType_maybe)
import GHC.Types.Avail (AvailInfo (..))
import GHC.Types.Name (nameOccName)
import GHC.Types.Name.Occurrence (mkVarOcc)
import GHC.Types.PkgQual (PkgQual)
import GHC.Unit (moduleUnitId)
import GHC.Unit.Finder qualified as Finder
import GHC.Unit.Finder.Types (FindResult (..))
import GHC.Unit.Module.ModIface (mi_exports)
import GHC.Unit.Types (IsBootInterface (..), moduleName)
import GHC.Utils.Outputable (text)
import GHCi.Message (EvalResult (..), EvalStatus_ (..))
import GHCi.RemoteTypes (ForeignHValue, HValueRef)
import Internal.Cache.Hpt (loadHomeUnit)
import Language.Haskell.Syntax.Expr (GhciLStmt)
import Language.Haskell.Syntax.ImpExp (ImportDecl (ideclQualified), ImportDeclQualifiedStyle (QualifiedPre))
import Language.Haskell.Syntax.Module.Name (ModuleName (..), mkModuleName)
import System.OsPath.Extra (toOsPath)
import Text.Read (readMaybe)
import Types.Args (Args (..))
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
      loadHomeUnit env.log dflags0 env.args.settings (moduleUnitId target.module_) (state, hsc_env0) (toOsPath homeUnit)
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

-- | How an @IO (Int, Int)@ result ('ResultCounts') is interpreted.
data TupleMode
  = -- | The pair is @(total, failed)@, and the evaluation fails if @failed /= 0@.
    TupleFailureCount
  | -- | The pair is an ordinary showable result, like any other.
    TuplePlain
  deriving stock (Eq, Show)

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
  | -- | @IO (Int, Int)@ -- shown like 'ResultShowable', and additionally interpreted as @(total, failed)@ if the
    -- caller asks for it with 'TupleFailureCount'.
    ResultCounts

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
    else if eqType resTy (mkBoxedTupleTy [intTy, intTy])
      then Just ResultCounts
    else if any (eqType resTy) ([intTy, integerTy, doubleTy, floatTy, charTy, boolTy] :: [Type])
      then Just ResultShowable
      else Nothing

-- | The statement that runs an expression and binds its result to @execResult@, for 'runExpression'.
-- It is a bind statement ('execResult <- ...'), not a bare expression statement: GHC's interactive
-- statement typechecker ('GHC.Tc.Module.tcUserStmt') unconditionally attempts to print the result
-- of a bare expression statement whenever its type isn't '()', so an expression of type 'IO String' would
-- both exfiltrate the result below and print it a second time to the real stdout. A bind statement only
-- prints its bound value when 'Opt_PrintBindResult' is set, which this driver never sets.
statementText :: Maybe MainResultKind -> String -> String
statementText mkind expr =
  case mkind of
    Just ResultShowable -> "execResult <- Prelude.fmap Prelude.show (" ++ expr ++ ")"
    Just ResultCounts -> "execResult <- Prelude.fmap Prelude.show (" ++ expr ++ ")"
    _ -> "execResult <- (" ++ expr ++ ")"

-- | Run an @IO a@ expression in the context of the target module, returning whether evaluation succeeded and the
-- exfiltrated result if there is one.
--
-- Exceptions thrown by the expression are caught by the interpreter and reported as a failed evaluation
-- (@EvalException@), as is any other outcome than success; with 'TupleFailureCount', a @(total, failed)@ result
-- with @failed /= 0@ is a failure as well.
--
-- The expression's result type (post-typecheck, via GHC's @:type@ machinery) decides whether its value can be
-- exfiltrated; see 'classifyMainResultType'. Anything unrecognized (in particular @IO ()@) is run with its
-- result discarded. For recognized types the statement is chosen so that the bound value is a 'String' at
-- runtime (the expression itself, or @fmap show@ of it, with 'show' dispatched by GHC's typechecker), which
-- allows coercing the 'ForeignHValue' directly.
runExpression :: TupleMode -> HscEnv -> ModuleTarget -> String -> Ghc (Bool, Maybe String)
runExpression mode hsc_env target expr = do
  setContext [IIModule (moduleName target.module_), qualifiedPrelude]
  mty <- exprType TM_Inst expr
  let mkind = classifyMainResultType mty
  evalStmtCustom (statementText mkind expr) execOptions >>= \case
    EvalComplete _ (EvalSuccess (fhv : _)) -> do
      mResultStr <- case mkind of
        Nothing -> pure Nothing
        Just _ -> do
          let Just interp = hsc_interp hsc_env
          hv <- liftIO (wormhole interp fhv)
          pure (Just (unsafeCoerce hv :: String))
      pure (resultSucceeded mode mkind mResultStr, mResultStr)
    _ -> pure (False, Nothing)

-- | @import qualified Prelude@, so that the statements generated by 'runExpression' don't depend on the target
-- module's own scope (a module loaded without its renamer environment only exposes its top-level bindings).
qualifiedPrelude :: InteractiveImport
qualifiedPrelude =
  IIDecl (simpleImportDecl (mkModuleName "Prelude")) {ideclQualified = QualifiedPre}

-- | Whether a completed evaluation counts as a success: always, unless the result is a @(total, failed)@ pair
-- that is to be interpreted as such. An unparseable pair is a failure.
resultSucceeded :: TupleMode -> Maybe MainResultKind -> Maybe String -> Bool
resultSucceeded TupleFailureCount (Just ResultCounts) mResultStr =
  case mResultStr >>= readMaybe @(Int, Int) of
    Just (_total, failed) -> failed == 0
    Nothing -> False
resultSucceeded _ _ _ = True

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
-- per session (either an expression in 'evaluateExpr', or a module's @main@ in 'executeMain'), so there
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

