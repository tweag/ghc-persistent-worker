{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Internal.Evaluate where

import Control.Concurrent (withMVar)
import Control.Monad.IO.Class (liftIO)
import GHC (Ghc, getSession, getSessionDynFlags, moduleNameString, setInteractiveDynFlags, setSession)
import GHC.Builtin.Types (boolTy, charTy, doubleTy, floatTy, intTy, integerTy, mkBoxedTupleTy, stringTy)
import GHC.Core.TyCo.Compare (eqType)
import GHC.Core.Type (Type)
import GHC.Driver.Config (initEvalOpts)
import GHC.Driver.DynFlags (wopt_unset)
import GHC.Driver.Env (hscInterp, hscSetActiveUnitId, mkInteractiveHscEnv)
import GHC.Driver.Env.Types (HscEnv (hsc_IC), hsc_mod_graph)
import GHC.Driver.Flags (WarningFlag (Opt_WarnUnusedLocalBinds))
import GHC.Driver.Main (hscParsedStmt)
import GHC.Driver.Monad (GhcMonad)
import GHC.Hs.Extension (GhcPs)
import GHC.Hs.ImpExp (simpleImportDecl)
import GHC.Runtime.Context (InteractiveContext (..), InteractiveImport (..))
import GHC.Runtime.Eval.Types (ExecOptions (..), isStep)
import GHC.Runtime.Interpreter (evalStmt)
import GHC.Tc.Utils.TcType (tcSplitIOType_maybe)
import GHC.Types.PkgQual (PkgQual)
import GHC.Unit (moduleUnitId)
import GHC.Unit.Finder qualified as Finder
import GHC.Unit.Finder.Types (FindResult (..))
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

