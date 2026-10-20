{-# LANGUAGE CPP #-}

-- | Support for populating @mi_top_env@ on a @ModIface@ produced by a regular compilation, so that GHCi-style
-- evaluation can resolve top-level names of that module without recompiling it with the interpreter backend.
--
-- The mechanism is a 'StaticPlugin' whose 'typeCheckResultAction' captures the real 'GlobalRdrEnv' and import
-- declarations from the 'TcGblEnv' right after typechecking, regardless of backend. This is the same data used by
-- 'GHC.Iface.Make.mkIfaceTc' to populate @mi_top_env@ for the interpreter backend; here we capture it unconditionally
-- and use it to patch the interface after the fact.
module Internal.Compile.TopEnv where

import GHC.Tc.Types (ImportUserSpec)
import GHC.Types.Name.Reader (GlobalRdrEnv)

-- | The data captured from a module's 'TcGblEnv' that is sufficient to reconstruct its top-level environment, mirroring
-- what 'GHC.Iface.Make.mkIfaceTc' uses to populate @mi_top_env@.
data CapturedTopEnv = CapturedTopEnv {
  rdrEnv :: GlobalRdrEnv,
  importDecls :: [ImportUserSpec]
}
