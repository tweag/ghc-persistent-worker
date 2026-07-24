-- | Description: Execute a compiled top-level @IO ()@ binding from a module using GHC's interactive evaluation
-- machinery, mimicking how an external test runner would invoke a test function from bytecode.
module Test.Interp where

import Control.Exception (SomeException, try)
import GHC (Ghc, InteractiveImport (IIModule), Module, compileExpr, moduleName, setContext)
import GHC.Utils.Monad (MonadIO (liftIO))
import Unsafe.Coerce (unsafeCoerce)

-- | Bring a module's top-level environment into scope, compile the named binding as an expression, and execute it as
-- an @IO ()@ action.
--
-- This exercises the same GHC machinery (@setContext@ + @compileExpr@) that interactive test runners use to invoke a
-- test function directly from compiled bytecode, without generating a @main@ wrapper module. Returns 'False' if
-- either compilation of the expression or execution of the resulting action throws an exception, so that a caller
-- driving several of these concurrently can report failures without crashing the whole build.
runInterpretedTest :: Module -> String -> Ghc Bool
runInterpretedTest modu name = do
  setContext [IIModule (moduleName modu)]
  hval <- compileExpr name
  result <- liftIO (try (unsafeCoerce hval :: IO ()) :: IO (Either SomeException ()))
  pure (either (const False) (const True) result)
