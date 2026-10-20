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
import GHC.Driver.Env (HscEnv (..))
import GHC.Driver.Plugins (Plugin (typeCheckResultAction), PluginWithArgs (PluginWithArgs), Plugins (staticPlugins), StaticPlugin (StaticPlugin), defaultPlugin)
import GHC.Tc.Types (ImportUserSpec, TcGblEnv (tcg_import_decls, tcg_rdr_env))
import GHC.Types.Name.Reader (GlobalRdrEnv)

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
