{-# LANGUAGE CPP #-}

-- | Compiler plugin for computing @mi_top_env@ for an interface without having to run the compilation pipeline again in
-- interpreter mode.
module Internal.Compile.TopEnv where

import Control.Monad.IO.Class (liftIO)
import Data.IORef (IORef, newIORef, writeIORef)
import GHC (ModIface)
import GHC.Driver.Env (HscEnv (..))
import GHC.Driver.Plugins (Plugin (..), PluginWithArgs (..), Plugins (..), StaticPlugin (..), defaultPlugin)
import GHC.Iface.Syntax (IfaceImport (..), ImpIfaceList (..))
import GHC.Tc.Types (ImpUserList (..), ImportUserSpec (..), TcGblEnv (..))
import GHC.Types.Name.Reader (GlobalRdrEnv, forceGlobalRdrEnv, globalRdrEnvLocal)
import GHC.Unit.Module.ModIface (IfaceTopEnv (..), set_mi_top_env)

#if MIN_VERSION_GLASGOW_HASKELL(9,14,0,0)

import GHC.Types.Avail (sortAvails)
import GHC.Types.Name.Reader (globalRdrEnvElts, gresToAvailInfo)

#endif

-- | The data captured from a module's 'TcGblEnv' that is sufficient to reconstruct its top-level environment.
data CapturedTopEnv =
  CapturedTopEnv {
    rdrEnv :: GlobalRdrEnv,
    importDecls :: [ImportUserSpec]
  }

-- | A 'Plugin' that stores the module's 'GlobalRdrEnv' and import declarations in the given 'IORef' as soon as
-- typechecking completes.
--
-- TODO Do we care about retaining the env here?
--
-- TODO Move to @internal@
captureTopEnvPlugin :: IORef (Maybe CapturedTopEnv) -> Plugin
captureTopEnvPlugin ref =
  defaultPlugin {
    typeCheckResultAction = \ _ _ tc_gbl@TcGblEnv {tcg_rdr_env, tcg_import_decls} -> do
      liftIO $ writeIORef ref (Just CapturedTopEnv {rdrEnv = tcg_rdr_env, importDecls = tcg_import_decls})
      pure tc_gbl
  }

-- | Add the @TopEnv@ plugin to the session's static plugins and return an 'IORef' that exfiltrates the env.
--
-- TODO Move to @internal@
withCaptureTopEnv :: HscEnv -> IO (HscEnv, IORef (Maybe CapturedTopEnv))
withCaptureTopEnv hsc_env = do
  ref <- newIORef Nothing
  pure (hsc_env {hsc_plugins = plugins {staticPlugins = plugin ref : staticPlugins plugins}}, ref)
  where
    plugins = hsc_env.hsc_plugins

    plugin ref = StaticPlugin (PluginWithArgs (captureTopEnvPlugin ref) [])

-- | Patch a 'ModIface' with an @mi_top_env@ reconstructed from captured typechecking data, following the same
-- construction as 'GHC.Iface.Make.mkIfaceTc' (@maybeGlobalRdrEnv@/@mkIfaceImports@), but using the real environment
-- unconditionally instead of only for the interpreter backend.
--
-- TODO Move to @internal@
--
-- TODO Inline @mkIfaceImports@ into a compat module instead of @toIfaceImport@
--
-- TODO Move CPP code to 9.14 compat module
patchTopEnv :: CapturedTopEnv -> ModIface -> ModIface
patchTopEnv CapturedTopEnv {rdrEnv, importDecls} =
  set_mi_top_env rdrs
  where
#if MIN_VERSION_GLASGOW_HASKELL(9,14,0,0)
    rdrs = IfaceTopEnv (sortAvails (gresToAvailInfo (globalRdrEnvElts exports))) imports
#else
    rdrs = Just (IfaceTopEnv exports imports)
#endif

    exports = forceGlobalRdrEnv (globalRdrEnvLocal rdrEnv)

    imports = toIfaceImport <$> importDecls

    toIfaceImport ImpUserSpec {ius_decl, ius_imports} =
      case ius_imports of
        ImpUserAll -> IfaceImport ius_decl ImpIfaceAll
        ImpUserExplicit env -> IfaceImport ius_decl (ImpIfaceExplicit (forceGlobalRdrEnv env))
        ImpUserEverythingBut ns -> IfaceImport ius_decl (ImpIfaceEverythingBut ns)
