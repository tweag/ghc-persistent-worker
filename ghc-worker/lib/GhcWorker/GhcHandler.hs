{-# LANGUAGE CPP #-}

module GhcWorker.GhcHandler where

import Common.Grpc (GrpcHandler (..))
import Control.Concurrent (MVar, readMVar)
import Control.Exception (throwIO, try)
#ifdef __DEBUG__
import Control.Monad (when)
#endif
import Control.Monad.IO.Class (liftIO)
import Data.Coerce (coerce)
import Data.Functor ((<&>))
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Int (Int32)
#ifdef GHC_DEBUG
import GHC.Debug.Stub (withGhcDebugUnix)
#endif
import GHC (DynFlags (..), Ghc, ModIface, getSession)
import GHC.Driver.DynFlags (GhcMode (..))
import GHC.Driver.Monad (reflectGhc, reifyGhc)
import GHC.Utils.Outputable (text)
import GhcWorker.CompileResult (CompileResult (..), usedDepFiles, writeResult)
import GhcWorker.Instrumentation (
  EvalReport (..),
  Hooks (..),
  InstrumentedHandler (..),
  failedEval,
  publishEvent,
  reportEval,
  )
import Internal.AbiHash (AbiHash (..), showAbiHash)
import Internal.Compile.Make (compileModuleWithDepsInHpt)
#ifdef GHC_DEBUG
import Internal.Debug (debugSocketPath)
#endif
import Data.Foldable (for_)
import Internal.DynFlags (modifyGlobalFlags)
import qualified Internal.Evaluate
import Internal.Log (newLogger)
import Internal.Metadata (computeMetadata)
import Internal.Session (withGhcMakeModule, withGhcMakeSource)
import Prelude hiding (log)
import System.IO (stdout)
import System.IO.Silently (hCapture)
import System.OsPath.Extra (fromOsPath)
import Types.Api (targetFromWorkerSpec)
import Types.Args (Args (..))
import qualified Types.BuckArgs
import Types.BuckArgs (BuckArgs, IsInterpreted (..), Mode (..), checkModuleTarget, parseBuckArgs, toGhcArgs)
import Types.Env (Env (..))
import Types.Grpc (RequestArgs (..))
import Types.Log (Logger (..), TraceId, newLog)
import Types.State (WorkerState (..))
import Types.Target (TargetSpec (..), renderTargetSpec)

#if defined(GBC)

import Data.Char (isAlphaNum, isLower)
import qualified Data.Text as Text
import GHC.Unit.Types (moduleName, moduleUnitId)
import GhcWorker.Eval.Executor (runInExecutor)
import Types.Api (ExecutorId (..))
import Types.Target (ModuleTarget (..))

-- | Whether the expression is a plain, unqualified variable name, which is what executors can run as an entry.
isIdentifier :: String -> Bool
isIdentifier = \case
  c : cs -> (isLower c || c == '_') && all (\ x -> isAlphaNum x || x `elem` ("_'" :: String)) cs
  [] -> False

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
    Just ModeEval -> do
      mModTarget <- checkModuleTarget args
      case mModTarget of
        Just modTarget -> do
          case args.expr of
            Just expr -> do
              report <- eval args.evalTargetName modTarget expr
              case report.outcome of
                Left err -> do
                  env.log.info ("error: " ++ err)
                  pure (1, Just (TargetUnknown "test"))
                Right isSuccessful ->
                  pure (if isSuccessful then 0 else 1, Just (TargetUnknown "test"))
            Nothing -> do
              _ <- error "worker: no expr"
              pure (1, Just (TargetUnknown "test"))
        Nothing -> do
          _ <- error "worker: No modTarget"
          pure (1, Just (TargetUnknown "test"))
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
        withGhcMakeModule args.interp target env Nothing (withTarget compileHpt)
      Nothing ->
        withGhcMakeSource env (withTarget compileHpt . TargetSource)

    compileHpt = compileAndReadAbiHash CompManager (compileModuleWithDepsInHpt env.log hooks.events) args

    eval mname modTarget expr = do
      case mname of
        Nothing -> pure ()
        Just name -> env.log.setTarget (TargetUnknown name)
#if defined(GBC)
      case args.evalExecutor of
        Just executor | isIdentifier expr -> evalInExecutor (ExecutorId (Text.pack executor)) modTarget expr
        _ -> reported modTarget False (evalInProcess modTarget expr)
#else
      reported modTarget False (evalInProcess modTarget expr)
#endif

    -- Publish events for the evaluation of the module's target.
    reported modTarget process run =
      case targetFromWorkerSpec (TargetModule modTarget) of
        Just target -> reportEval hooks target process run
        Nothing -> run

#if defined(GBC)
    -- Run the expression in the persistent executor with the given id, if its type is supported by executors, which
    -- is determined in the evaluation session. Otherwise, fall back to in-process evaluation.
    evalInExecutor executorId modTarget expr = do
      prepared <-
        withGhcMakeModule Interpreted modTarget env Nothing
          (\_ -> Just <$> Internal.Evaluate.prepareExecutorEntry env (fromOsPath <$> args.homeUnit) modTarget expr)
      case prepared of
        Nothing -> reported modTarget False (pure (failedEval "worker: eval session failed"))
        Just (Left err) -> reported modTarget False (pure (failedEval err))
        Just (Right Internal.Evaluate.ExecutorEntry {entryKind = Nothing}) -> do
          env.log.info ("Expression type is not supported by executors, evaluating in-process: " ++ expr)
          reported modTarget False (evalInProcess modTarget expr)
        Just (Right Internal.Evaluate.ExecutorEntry {entryKind = Just kind, packageDbs}) ->
          -- The session has ended, so the worker state contains its loaded modules.
          reported modTarget True $
            runInExecutor (publishEvent hooks) env.log env.state executorId
              (concatMap (\ db -> ["-package-db", db]) packageDbs)
              args.evalEnv
              (moduleUnitId modTarget.module_, moduleName modTarget.module_) expr kind
#endif

    evalInProcess modTarget expr = do
      let mode = if args.evalCountFailures then Internal.Evaluate.TupleFailureCount else Internal.Evaluate.TuplePlain
      (res_stdout, r) <-
        hCapture [stdout] $
          withGhcMakeModule Interpreted modTarget env Nothing
            (\_ -> Just <$> Internal.Evaluate.evaluateExpr env (fromOsPath <$> args.homeUnit) modTarget mode expr)
      env.log.infoD (text res_stdout)
      case r of
        Nothing -> pure (failedEval "worker: eval session failed")
        Just (Left err) -> pure (failedEval err)
        Just (Right (ok, mResult)) -> do
          for_ mResult env.log.info
          pure EvalReport {outcome = Right ok, result = mResult, stats = Nothing}

    withTarget f (target :: TargetSpec) =
      reifyGhc $ \session -> do
        env.log.setTarget target
#ifdef GHC_DEBUG
        let path = debugSocketPath target
        (if env.args.settings.instrument then withGhcDebugUnix path else id) $
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
  Maybe TraceId ->
  InstrumentedHandler
ghcHandler state traceId =
  InstrumentedHandler \ hooks -> GrpcHandler \ commandEnv argv -> do
    rawLog <- newLogger <$> newLog traceId
    category <- newIORef "worker"
    let log = hooks.instrumentLogger (readIORef category) rawLog {
          setTarget = \ spec -> rawLog.setTarget spec *> writeIORef category (renderTargetSpec spec)
        }
    result <- try do
      buckArgs <- either parseError pure (parseBuckArgs commandEnv argv)
      settings <- (.settings) <$> readMVar state
      args <- toGhcArgs buckArgs (Just settings)
      log.debug (unlines (coerce argv))
      let env = Env {log, state, args = args}
      dispatch hooks env buckArgs
    processResult log state result
  where
    parseError msg =
      throwIO (userError ("Parsing Buck args failed: " ++ msg))
