{-# LANGUAGE CPP #-}

-- | Compiler plugin for computing @mi_top_env@ for an interface without having to run the compilation pipeline again in
-- interpreter mode.
module Internal.Compile.TopEnv where

import Control.Monad.IO.Class (liftIO)
import Data.IORef (IORef, writeIORef)
import GHC.Driver.Plugins (Plugin (typeCheckResultAction), defaultPlugin)
import GHC.Tc.Types (ImportUserSpec, TcGblEnv (..))
import GHC.Types.Name.Reader (GlobalRdrEnv)

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
