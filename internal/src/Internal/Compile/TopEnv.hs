{-# LANGUAGE CPP #-}

-- | Support for populating @mi_top_env@ on a @ModIface@ produced by a regular compilation, so that GHCi-style
-- evaluation can resolve top-level names of that module without recompiling it with the interpreter backend.
--
-- The mechanism is a 'StaticPlugin' whose 'typeCheckResultAction' captures the real 'GlobalRdrEnv' and import
-- declarations from the 'TcGblEnv' right after typechecking, regardless of backend. This is the same data used by
-- 'GHC.Iface.Make.mkIfaceTc' to populate @mi_top_env@ for the interpreter backend; here we capture it unconditionally
-- and use it to patch the interface after the fact.
module Internal.Compile.TopEnv where

import Control.Monad.IO.Class (liftIO)
import Data.IORef (IORef, newIORef, writeIORef)
import GHC (ModIface)
import GHC.Driver.Env (HscEnv (..))
import GHC.Driver.Plugins (Plugin (typeCheckResultAction), PluginWithArgs (PluginWithArgs), Plugins (staticPlugins), StaticPlugin (StaticPlugin), defaultPlugin)
import GHC.Iface.Syntax (IfaceImport (IfaceImport), ImpIfaceList (ImpIfaceAll, ImpIfaceEverythingBut, ImpIfaceExplicit))
import GHC.Tc.Types (ImpUserList (ImpUserAll, ImpUserEverythingBut, ImpUserExplicit), ImportUserSpec (ImpUserSpec, ius_decl, ius_imports), TcGblEnv (tcg_import_decls, tcg_rdr_env))
import GHC.Types.Name.Reader (GlobalRdrEnv, forceGlobalRdrEnv, globalRdrEnvLocal)
import GHC.Unit.Module.ModIface (IfaceTopEnv (..), set_mi_top_env)

#if MIN_VERSION_GLASGOW_HASKELL(9,14,0,0)
import GHC.Types.Avail (sortAvails)
import GHC.Types.Name.Reader (globalRdrEnvElts, gresToAvailInfo)
#endif

-- | The data captured from a module's 'TcGblEnv' that is sufficient to reconstruct its top-level environment, mirroring
-- what 'GHC.Iface.Make.mkIfaceTc' uses to populate @mi_top_env@.
data CapturedTopEnv = CapturedTopEnv {
  rdrEnv :: GlobalRdrEnv,
  importDecls :: [ImportUserSpec]
}

-- | A 'Plugin' that stores the module's 'GlobalRdrEnv' and import declarations into the given 'IORef' as soon as
-- typechecking completes, regardless of backend.
captureTopEnvPlugin :: IORef (Maybe CapturedTopEnv) -> Plugin
captureTopEnvPlugin ref =
  defaultPlugin {
    typeCheckResultAction = \ _ _ tc_gbl -> do
      liftIO (writeIORef ref (Just CapturedTopEnv {rdrEnv = tcg_rdr_env tc_gbl, importDecls = tcg_import_decls tc_gbl}))
      pure tc_gbl
  }

-- | Add 'captureTopEnvPlugin' to the session's static plugins, returning both the modified 'HscEnv' and a fresh
-- 'IORef' that will hold the captured data once compilation has typechecked the module.
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
patchTopEnv :: CapturedTopEnv -> ModIface -> ModIface
patchTopEnv CapturedTopEnv {rdrEnv, importDecls} =
  set_mi_top_env rdrs
  where
    exports = forceGlobalRdrEnv (globalRdrEnvLocal rdrEnv)

    imports = map toIfaceImport importDecls

    toIfaceImport ImpUserSpec {ius_decl, ius_imports} =
      case ius_imports of
        ImpUserAll -> IfaceImport ius_decl ImpIfaceAll
        ImpUserExplicit env -> IfaceImport ius_decl (ImpIfaceExplicit (forceGlobalRdrEnv env))
        ImpUserEverythingBut ns -> IfaceImport ius_decl (ImpIfaceEverythingBut ns)

#if MIN_VERSION_GLASGOW_HASKELL(9,14,0,0)
    rdrs = IfaceTopEnv (sortAvails (gresToAvailInfo (globalRdrEnvElts exports))) imports
#else
    rdrs = Just (IfaceTopEnv exports imports)
#endif
