module Internal.Evaluate where

import Control.Concurrent (withMVar)
import Control.Monad.IO.Class (liftIO)
import Data.Functor ((<&>))
import GHC (Ghc, getSession, getSessionDynFlags, moduleNameString, setInteractiveDynFlags, setSession)
import GHC.Builtin.Types (boolTy, charTy, doubleTy, floatTy, intTy, integerTy, mkBoxedTupleTy, stringTy)
import GHC.Core.TyCo.Compare (eqType)
import GHC.Core.Type (Type)
import GHC.Driver.Env (hscSetActiveUnitId)
import GHC.Driver.Env.Types (HscEnv, hsc_mod_graph)
import GHC.Tc.Utils.TcType (tcSplitIOType_maybe)
import GHC.Types.PkgQual (PkgQual)
import GHC.Unit (moduleUnitId)
import GHC.Unit.Finder (findImportedModule)
import GHC.Unit.Finder.Types (FindResult (..))
import Internal.Cache.Hpt (loadHomeUnit)
import Language.Haskell.Syntax.Module.Name (ModuleName (..))
import System.OsPath.Extra (toOsPath)
import Types.Args (Args (..))
import Types.Env (Env (..))
import Types.State (WorkerState (..))
import Types.State.Make (EModuleGraph (..), MakeState (..))
import Types.Target (ModuleTarget (..))

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
