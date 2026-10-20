{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Internal.Evaluate where

import Control.Concurrent (withMVar)
import Control.Monad.IO.Class (liftIO)
import GHC (Ghc, getSession, getSessionDynFlags, moduleNameString, setInteractiveDynFlags, setSession)
import GHC.Driver.Env (hscSetActiveUnitId)
import GHC.Driver.Env.Types (HscEnv, hsc_mod_graph)
import GHC.Types.PkgQual (PkgQual)
import GHC.Unit (moduleUnitId)
import GHC.Unit.Finder qualified as Finder
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
