{-# LANGUAGE CPP #-}

-- | Compiler plugin for computing @mi_top_env@ for an interface without having to run the compilation pipeline again in
-- interpreter mode.
module Internal.Compile.TopEnv where

import GHC.Tc.Types (ImportUserSpec)
import GHC.Types.Name.Reader (GlobalRdrEnv)

-- | The data captured from a module's 'TcGblEnv' that is sufficient to reconstruct its top-level environment.
data CapturedTopEnv =
  CapturedTopEnv {
    rdrEnv :: GlobalRdrEnv,
    importDecls :: [ImportUserSpec]
  }
