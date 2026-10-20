{-# LANGUAGE CPP #-}

module GhcWorker.GhcHandler where

import Common.Grpc (GrpcHandler (..))
import Control.Applicative ((<|>))
import Control.Concurrent (MVar)
import Control.Exception (throwIO, try)
import Control.Monad (when)
import Data.Coerce (coerce)
import Data.Foldable (for_, traverse_)
import Data.Functor ((<&>))
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Int (Int32)
import Data.Maybe (fromMaybe, isNothing)
import GHC (DynFlags (..), Ghc, ModIface, getSession)
import GHC.Driver.DynFlags (GhcMode (..))
import GHC.Driver.Monad (reflectGhc, reifyGhc)
import GhcWorker.CompileResult (CompileResult (..), usedDepFiles, writeResult)
import GhcWorker.Instrumentation (
  EvalReport (..),
  EvalStatus (..),
  Hooks (..),
  InstrumentedHandler (..),
  evalReport,
  evalWithEvents,
  )
import Internal.AbiHash (AbiHash (..), showAbiHash)
import Internal.Compile.Make (compileModuleWithDepsInHpt)
import Internal.DynFlags (modifyGlobalFlags)
import Internal.Evaluate (TupleMode (..), evaluateExpr)
import Internal.Log (newLogger)
import Internal.Metadata (computeMetadata)
import Internal.Session (withGhcMakeModule, withGhcMakeSource)
import Prelude hiding (log)
import System.OsPath.Extra (fromOsPath)
import Types.Api (targetFromWorkerSpec)
import Types.Args (Args (..))
import qualified Types.BuckArgs
import Types.BuckArgs (BuckArgs, IsInterpreted (..), Mode (..), checkModuleTarget, parseBuckArgs, toGhcArgs)
import Types.Env (Env (..))
import Types.Grpc (RequestArgs (..))
import Types.Log (Logger (..), TraceId, newLog)
import Types.Settings (Settings (..))
import Types.State (WorkerState (..))
import Types.Target (ModuleTarget, TargetSpec (..), renderTargetSpec)

#ifdef GHC_DEBUG

import GHC.Debug.Stub (withGhcDebugUnix)
import Internal.Debug (debugSocketPath)

#endif

#if __DEBUG__

import Internal.State (dumpState)

#endif

-- | Compile a single module.
-- Depending on @mode@ this will either use the old EPS-based oneshot-style compilation logic or the HPT-based
-- make-style implementation.
compileAndReadAbiHash ::
  GhcMode ->
  (TargetSpec -> Ghc (Maybe ModIface)) ->
  BuckArgs ->
  TargetSpec ->
  Ghc (Maybe CompileResult)
compileAndReadAbiHash ghcMode compile args target = do
  modifyGlobalFlags \ d -> d {ghcMode}
  compile target >>= traverse \ iface -> do
    hsc_env <- getSession
    let
      abiHash :: Maybe AbiHash
      abiHash = do
        path <- args.abiOut
        Just AbiHash {path, hash = showAbiHash hsc_env iface}
      depFiles = usedDepFiles hsc_env iface
    pure CompileResult {abiHash, depFiles}

-- | Ensure that the CLI args @--module@, @--unit@ and @--expr@ were specified.
checkEvalArgs ::
  Env ->
  BuckArgs ->
  IO (Either (Int32, Maybe TargetSpec) (ModuleTarget, String))
checkEvalArgs env args = do
  result <$> checkTarget <*> checkExpr
  where
    result (Just moduleTarget) (Just expr) =
      Right (moduleTarget, expr)
    result target _ =
      Left (1, targetFallback target)

    checkTarget = do
      target <- checkModuleTarget args
      when (isNothing target) do
        env.log.fatal "worker: No module specified for evaluation"
      pure target

    checkExpr = do
      when (isNothing args.expr) do
        env.log.fatal "worker: No expression specified for evaluation"
      pure args.expr

    targetFallback modTarget = (TargetModule <$> modTarget) <|> noTarget

    noTarget = TargetUnknown <$> args.evalTargetName

-- TODO use structured types for all return values all the way into the call graph
dispatchEval ::
  Hooks ->
  Env ->
  BuckArgs ->
  ModuleTarget ->
  String ->
  IO (Int32, Maybe TargetSpec)
dispatchEval hooks env args modTarget expr = do
  report <- eval args.evalTargetName
  case report.status of
    EvalException ->
      pure (1, Just targetFallback)
    EvalFailed err -> do
      env.log.info ("worker: Evaluation failed: " ++ err)
      pure (1, Just targetFallback)
    EvalFinished {success} ->
      pure (if success then 0 else 1, Just targetFallback)
  where
    eval mname = do
      for_ mname \ name -> env.log.setTarget (TargetUnknown name)
      withReport False evalInProcess

    -- Publish events for the evaluation of the module's target.
    withReport process =
      case targetFromWorkerSpec (TargetModule modTarget) of
        Just target -> evalWithEvents hooks target process
        Nothing -> id

    evalInProcess = do
      let mode = if args.evalCountFailures then TupleFailureCount else TuplePlain
      r <- withGhcMakeModule Interpreted modTarget Nothing env \ _ ->
        Just <$> evaluateExpr env (fromOsPath <$> args.homeUnit) modTarget mode expr
      case r of
        Nothing -> pure (evalReport EvalException)
        Just (Left message) -> pure (evalReport (EvalFailed {message}))
        Just (Right (success, result)) -> do
          traverse_ env.log.info result
          pure EvalReport {status = EvalFinished {success}, result, maxMemory = Nothing}

    targetFallback = TargetUnknown targetName

    targetName = fromMaybe "unknown" args.evalTargetName

-- | Process a worker request based on the operational mode specified in the request arguments, either compiling a
-- single module for 'ModeCompile' (@-c@), or computing and writing the module graph to a JSON file for 'ModeMetadata'
-- (@-M@).
dispatch ::
  Hooks ->
  Env ->
  BuckArgs ->
  IO (Int32, Maybe TargetSpec)
dispatch hooks env args =
  case args.mode of
    Just ModeCompile -> do
      compile >>= \case
        Nothing -> pure (1, failedTarget)
        Just (CompileResult {abiHash, depFiles}, target) -> do
          writeResult args abiHash depFiles
          pure (0, Just target)
    Just ModeMetadata -> do
      (success, target) <- computeMetadata hooks.events env
      pure (if success then 0 else 1, target)
    Just ModeEval ->
      either pure (uncurry (dispatchEval hooks env args)) =<< checkEvalArgs env args
    Just m -> error ("worker: mode not implemented: " ++ show m)
    Nothing -> error "worker: no mode specified"
  where
    -- The target of a failed compilation, so that the failure can be reported.
    failedTarget =
      (if args.interp == Interpreted then TargetModuleInterp else TargetModule) <$> env.args.moduleTarget

    compile = case env.args.moduleTarget of
      Just target -> do
        if args.interp == Interpreted
        then
          env.log.setTarget (TargetModuleInterp target)
        else
          env.log.setTarget (TargetModule target)
        withGhcMakeModule args.interp target Nothing env (withTarget compileHpt)
      Nothing ->
        withGhcMakeSource env (withTarget compileHpt . TargetSource)

    compileHpt = compileAndReadAbiHash CompManager (compileModuleWithDepsInHpt env.log hooks.events) args

    withTarget f (target :: TargetSpec) =
      reifyGhc $ \session -> do
        env.log.setTarget target
#ifdef GHC_DEBUG
        let path = debugSocketPath target
        (if featureOn FeatureInstrument env.args.settings then withGhcDebugUnix path else id) $
          reflectGhc (f target) session <&> fmap \ r -> (r, target)
#else
        reflectGhc (f target) session <&> fmap \ r -> (r, target)
#endif

processResult ::
  Logger ->
  MVar WorkerState ->
  Either IOError (Int32, Maybe TargetSpec) ->
  IO ([String], Int32)
processResult logger _stateVar result = do
#if __DEBUG__
  when (exitCode /= 0) do
    dumpState logger _stateVar _exception
#endif
  output <- logger.flush
  pure (output, exitCode)
  where
    ((exitCode, _target), _exception) = case result of
      Right out -> (out, Nothing)
      Left err -> ((1, Nothing), Just ("Exception: " ++ show err))

-- | Default implementation of an 'InstrumentedHandler' using our custom persistent worker GHC mode, either using HPT or
-- EPS for local dependency lookup.
--
-- Parses the request args from Buck, creates a new 'Log' and 'Env', and passes all of that to 'dispatch'.
-- Afterwards, extracts the log messages that GHC wrote to 'Log' and calls the instrumentation hook 'compileFinish',
-- providing the log and exit code.
--
-- If an exception was thrown, the hook is called without data.
ghcHandler ::
  -- | first req lock hack
  MVar WorkerState ->
  Settings ->
  Maybe TraceId ->
  InstrumentedHandler
ghcHandler state settings traceId =
  InstrumentedHandler \ hooks -> GrpcHandler \ commandEnv argv -> do
    rawLog <- newLogger <$> newLog traceId
    category <- newIORef "worker"
    let log = hooks.instrumentLogger (readIORef category) rawLog {
          setTarget = \ spec -> rawLog.setTarget spec *> writeIORef category (renderTargetSpec spec)
        }
    result <- try do
      buckArgs <- either parseError pure (parseBuckArgs commandEnv argv)
      args <- toGhcArgs buckArgs (Just settings)
      log.debug (unlines (coerce argv))
      let env = Env {log, state, args = args}
      dispatch hooks env buckArgs
    processResult log state result
  where
    parseError msg =
      throwIO (userError ("Parsing Buck args failed: " ++ msg))
