module Internal.Evaluate where

import Control.Concurrent (withMVar)
import Control.Monad.IO.Class (liftIO)
import Data.Functor ((<&>))
import GHC (Ghc, getSession, getSessionDynFlags, moduleNameString, runTcInteractive, setInteractiveDynFlags, setSession)
import GHC.Builtin.Types (boolTy, charTy, doubleTy, floatTy, intTy, integerTy, mkBoxedTupleTy, stringTy)
import GHC.Core.TyCo.Compare (eqType)
import GHC.Core.Type (Type)
import GHC.Driver.Config (initEvalOpts)
import GHC.Driver.DynFlags (wopt_unset)
import GHC.Driver.Env (hscInterp, hscSetActiveUnitId, hsc_home_unit, mkInteractiveHscEnv, runInteractiveHsc)
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
import GHC.Tc.Module (TcRnExprMode (..))
import GHC.Tc.Utils.TcType (tcSplitIOType_maybe)
import GHC.Types.Avail (AvailInfo (..))
import GHC.Types.Name (nameOccName)
import GHC.Types.Name.Occurrence (mkVarOcc)
import GHC.Types.PkgQual (PkgQual (..))
import GHC.Unit (moduleUnitId)
import GHC.Unit.Finder (findImportedModule)
import GHC.Unit.Finder.Types (FindResult (..))
import GHC.Unit.Home (homeUnitId)
import GHC.Unit.Module.ModIface (mi_exports)
import GHC.Unit.Types (IsBootInterface (..), moduleName)
import GHC.Utils.Outputable (text)
import GHCi.Message (EvalResult (..), EvalStatus_ (..))
import GHCi.RemoteTypes (ForeignHValue, HValueRef)
import Internal.Cache.Hpt (loadHomeUnit)
import Language.Haskell.Syntax.Expr (GhciLStmt)
import Language.Haskell.Syntax.ImpExp (ImportDecl (..), ImportDeclQualifiedStyle (..))
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
--
-- TODO unclear, revisit later
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

-- | The Finder's possible failure reasons.
data ModuleLookupError =
  ModuleNotFound
  |
  ModulePackageNotFound
  |
  ModuleAmbiguous

renderModuleLookupError :: ModuleName -> ModuleLookupError -> String
renderModuleLookupError modName err =
  reason err ++ moduleNameString modName
  where
    reason = \case
      ModuleNotFound -> "Module not found: "
      ModulePackageNotFound -> "Package not found for module "
      ModuleAmbiguous -> "Multiple candidate modules found for "

-- | Verify that a module is present in the finder for the given pachage qualifier.
--
-- TODO Maybe, rename when clarified
resolveTargetModule :: HscEnv -> ModuleName -> PkgQual -> IO (Either ModuleLookupError ())
resolveTargetModule hsc_env modname pkgqual =
  findImportedModule hsc_env modname pkgqual <&> \case
    Found _ _ -> Right ()
    NoPackage _ -> Left ModulePackageNotFound
    FoundMultiple _ -> Left ModuleAmbiguous
    NotFound {} -> Left ModuleNotFound

-- | How an execution result of @IO (Int, Int)@ is interpreted, to accomodate the approach used in the Buck rules.
--
-- TODO do we actually need this? Ask Ian.
data TupleMode =
  -- | The pair is @(total, failed)@, and the evaluation fails if @failed /= 0@.
  TupleFailureCount
  |
  -- | No special treatment.
  TuplePlain
  deriving stock (Eq, Show)

-- | Classifies a module's @main@ result type.
-- For 'IO a', where @a@ is a stringly or numeric type, the result is propagated to the events stream, for clients to
-- display or process.
--
-- TODO check that these are true. Also move the documentation of the result action to where it happens
data MainResultKind =
  -- | @main :: IO String@
  -- Printed directly with 'putStr'.
  ResultString
  |
  -- | @main :: IO a@ for another stringly/numeric @a@
  -- Printed with 'print' (using 'Show').
  ResultShowable
  |
  -- | @IO (Int, Int)@
  -- Shown like 'ResultShowable', and additionally interpreted as @(total, failed)@ if the caller asks for it with
  -- 'TupleFailureCount'.
  ResultCounts
  deriving stock (Eq, Show)

-- | Inspect the result type of the main function for an evaluation task.
--
-- If it has the type @IO a@, try to classify it as one of the variants of 'MainResultKind'.
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

-- | Decide whether the result of an evaluation counts as a success.
--
-- If the user enabled 'TupleFailureCount' and the main function's type is @IO (Int, Int)@, parse a tuple and treat its
-- second element as the number of failures.
resultSucceeded :: TupleMode -> Maybe MainResultKind -> Maybe String -> Bool
resultSucceeded TupleFailureCount (Just ResultCounts) result =
  case result >>= readMaybe @(Int, Int) of
    Just (_total, failed) -> failed == 0
    Nothing -> False
resultSucceeded _ _ _ = True

-- | A qualified import of "Prelude" for the interpreter.
qualifiedPrelude :: InteractiveImport
qualifiedPrelude =
  IIDecl (simpleImportDecl (mkModuleName "Prelude")) {ideclQualified = QualifiedPre}

-- | Evaluate a single parsed statement as bytecode.
--
-- Unlike 'GHC.Runtime.Eval.execStmt'', this does not update the session's 'InteractiveContext' with the statement's
-- bound identifiers or fixity declarations afterwards: every caller in this module runs at most one statement
-- per session (either an expression in 'evaluateExpr', or a module's @main@ in 'executeMain'), so there
-- is no subsequent statement in the same session that would need to see those bindings.
--
-- TODO Examine more closely later
evalStmt' :: GhcMonad m => GhciLStmt GhcPs -> String -> ExecOptions -> m (EvalStatus_ [ForeignHValue] [HValueRef])
evalStmt' stmt _stmt_text ExecOptions{..} = do
    hsc_env <- getSession
    let -- Turn off -fwarn-unused-local-binds when running a statement, to hide warnings about the implicit bindings
        -- introduced by the statement's own desugaring.
        idflags' = ic_dflags hsc_env.hsc_IC `wopt_unset` Opt_WarnUnusedLocalBinds
        hsc_env' = mkInteractiveHscEnv hsc_env {hsc_IC = hsc_env.hsc_IC {ic_dflags = idflags'}}
    liftIO $ hscParsedStmt hsc_env' stmt >>= \case
      -- Empty statement / comment: nothing to run, so trivially succeed with no bound values.
      Nothing -> pure (EvalComplete 0 (EvalSuccess []))
      Just (_ids, hval, _fix_env) -> liftIO do
        let eval_opts = initEvalOpts idflags' (isStep execSingleStep)
        evalStmt (hscInterp hsc_env) eval_opts (execWrap hval)

-- | Run a statement in the current interactive context.
evalStmtCustom ::
  GhcMonad m =>
  -- | Code
  String ->
  ExecOptions ->
  m (EvalStatus_ [ForeignHValue] [HValueRef])
evalStmtCustom input exec_opts@ExecOptions{..} = do
    hsc_env <- getSession
    liftIO (runInteractiveHsc hsc_env (hscParseStmtWithLocation execSourceFile execLineNumber input)) >>= \case
      -- Empty statement / comment
      Nothing -> pure (EvalComplete 0 (EvalSuccess []))
      Just stmt -> evalStmt' stmt input exec_opts

-- | Check whether a module's interface exports a binding named @main@.
moduleHasMain :: HscEnv -> ModuleName -> PkgQual -> IO Bool
moduleHasMain hsc_env modname pkgqual = do
  iface <-
    runInteractiveHsc hsc_env $
    ioMsgMaybe $
    hoistTcRnMessage $
    GHC.runTcInteractive hsc_env $
    loadSrcInterface (text "checking for main") modname NotBoot pkgqual
  pure (any isMain (mi_exports iface))
  where
    isMain = \case
      Avail n -> nameOccName n == mkVarOcc "main"
      AvailTC _ _ -> False

-- | A statement that binds an expression's result to @execResult@.
--
-- The interpreter unconditionally prints results to stdout unless they're bound.
statementText :: String -> Maybe MainResultKind -> String
statementText expr = \case
  Just ResultShowable -> shown
  Just ResultCounts -> shown
  _ -> "execResult <- (" ++ expr ++ ")"
  where
    shown = "execResult <- Prelude.fmap Prelude.show (" ++ expr ++ ")"

-- | Interpret an expression in the context of the target module.
--
-- Return whether evaluation succeeded and, if possible, the return value of the expression.
--
-- TODO refactor
runExpression :: TupleMode -> HscEnv -> ModuleTarget -> String -> Ghc (Bool, Maybe String)
runExpression mode hsc_env target expr = do
  setContext [IIModule (moduleName target.module_), qualifiedPrelude]
  mty <- exprType TM_Inst expr
  let mkind = classifyMainResultType mty
  evalStmtCustom (statementText expr mkind) execOptions >>= \case
    EvalComplete _ (EvalSuccess (fhv : _)) -> do
      mResultStr <- case mkind of
        Nothing -> pure Nothing
        Just _ -> do
          hv <- liftIO (wormhole (hscInterp hsc_env) fhv)
          pure (Just (unsafeCoerce hv :: String))
      pure (resultSucceeded mode mkind mResultStr, mResultStr)
    _ -> pure (False, Nothing)

-- | Set up an evaluation session for the given target, ensure that its module exists, and run the given action in this
-- GHC session.
--
-- TODO refactor, use Text or FastString
withTargetModule :: String -> Env -> Maybe String -> ModuleTarget -> (HscEnv -> Ghc a) -> Ghc (Either String a)
withTargetModule label env mHomeUnit target use =
  setupEvaluationSession env mHomeUnit target >>= \case
    Nothing -> pure (Left (label ++ ": no home unit configured for execute target"))
    Just hsc_env -> do
      let modname = moduleName target.module_
          pkgqual = ThisPkg (homeUnitId (hsc_home_unit hsc_env))
      liftIO (resolveTargetModule hsc_env modname pkgqual) >>= \case
        Left err -> pure (Left (label ++ ": " ++ renderModuleLookupError modname err))
        Right () -> Right <$> use hsc_env
