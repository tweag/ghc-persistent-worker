{-# LANGUAGE CPP #-}

-- | Execute tasks dispatched to persistent executors.
--
-- With the GBC backport, the parent sends a bytecode plan ('GhcWorker.Executor.Plan') to an interface-free
-- executor session, so that the child only links the modules it doesn't have yet.
-- Modules whose @main@ can't be run from a plan (a result type that needs a 'Show' dictionary, or no bytecode
-- available for the closure) and builds without GBC use the executor's module mode instead, which restores the unit
-- from the cache like a one-shot child.
module GhcServer.Build.ExecutorTask where

import qualified GHC
import Control.Concurrent.MVar (withMVar)
import GhcServer.Build.Executor (callExecutor, ensureExecutor)
import GhcServer.Build.Process (executeModuleTaskWith)
import GhcServer.Data.BuildEnv (BuildEnv (..))
import GhcServer.Data.Unit (Unit)
import Test.Data.Scheduler (TaskResult)
import Types.Api (ExecutorId, ProcessStats)
import Types.State.Executor (ExecutorHandle (..))

#if defined(GBC)

import Control.Concurrent.MVar (readMVar)
import Control.Exception (SomeException, displayException, try)
import Data.List (find)
import qualified Data.Text as Text
import GHC.Core.TyCo.Rep (Type)
import GHC.Types.Avail (availNames)
import GHC.Types.Id (idType)
import GHC.Types.Name (nameOccName)
import GHC.Types.Name.Occurrence (occNameString)
import GHC.Types.TyThing (TyThing (AnId))
import GHC.Types.TypeEnv (lookupTypeEnv)
import GHC.Unit.Home.Graph (lookupHug)
import GHC.Unit.Home.ModInfo (HomeModInfo (..))
import GHC.Unit.Module.ModDetails (ModDetails (..))
import GHC.Unit.Module.ModIface (mi_exports)
import GhcServer.Build.Executor (respawnExecutor)
import GhcWorker.Executor.Plan (PlanError (..), displayPlanError, executePlanned)
import GhcServer.Build.Process (outputTaskResult)
import Internal.Executor.PayloadArena (payloadSegmentPath)
import Types.ExecutorPlan (EntryKind (..), ExecutorPlan, PayloadTransport (..), describePlan)
import GhcServer.Data.ProcessEval (ExecutorCall (..))
import Types.ProcessEval (EvalOutcome (..), ProcessEvalOutput (..), ProcessEvalResult (..))
import GhcServer.Data.Unit (Unit (..), unitId)
import GhcServer.Log (instrumentLogger)
import Internal.Evaluate (MainResultKind (..), classifyMainResultType)
import Internal.State (dynamicFeatureOn)
import System.OsPath.Extra (fromOsPath, (</>), osp)
import Test.Data.Scheduler (TaskResult (..))
import Types.Api (UnitName (..))
import Types.Log (Logger (..))
import Types.FeatureFlags (Feature (FeatureSharedPayloads))
import Types.State (WorkerState (..))
import Types.State.Executor (HomeModuleId)
import Types.State.Make (MakeState (..))

#endif

-- | Run a module's execute task via the persistent executor identified by 'ExecutorId', spawning it on first use.
executeModuleTaskExecutor ::
  BuildEnv ->
  ExecutorId ->
  Unit ->
  GHC.ModuleName ->
  IO (Maybe (TaskResult String), Maybe ProcessStats)

#if defined(GBC)

executeModuleTaskExecutor env executorId unit modName =
  mainEntry env root >>= \case
    NoEntry -> pure (Nothing, Nothing)
    ModuleEntry reason -> do
      logger.debug ("Executing " ++ GHC.moduleNameString modName ++ " in module mode: " ++ reason)
      executeModuleMode env executorId unit modName
    PlanEntry kind ->
      try @SomeException (runPlanned kind) >>= \case
        Left err -> pure (Just (TaskFailed ("Executor for " ++ GHC.moduleNameString modName ++ " crashed: " ++ displayException err)), Nothing)
        Right (Left (PlanUnavailable err)) -> do
          logger.debug ("Executing " ++ GHC.moduleNameString modName ++ " in module mode: " ++ err)
          executeModuleMode env executorId unit modName
        Right (Left err) -> pure (Just (TaskFailed (displayPlanError err)), Nothing)
        Right (Right (plan, output)) -> do
          logger.debug ("Executed " ++ GHC.moduleNameString modName ++ " with " ++ describePlan plan)
          outputTaskResult logger output
  where
    root = (unitId unit.name, modName)

    runPlanned kind = do
      options <- executorGhcOptions env unit
      transport <- payloadTransport env
      withRespawn env executorId (executePlanned ExecutePlan transport env.stateVar executorId options [] root "main" kind)

    logger = instrumentLogger env.instrChan logCategory env.log

    logCategory = Text.unpack unit.name.text ++ ":" ++ GHC.moduleNameString modName ++ ":executor"

-- | Shared memory if 'FeatureSharedPayloads' is enabled, otherwise files.
payloadTransport :: BuildEnv -> IO PayloadTransport
payloadTransport env =
  dynamicFeatureOn FeatureSharedPayloads env.stateVar >>= \case
    True -> PayloadSegment <$> payloadSegmentPath
    False -> pure (PayloadFiles (fromOsPath (env.outputDir </> [osp|executor-payloads|])))

-- | How a module's @main@ is run in an executor.
data Entry =
  -- | From a bytecode plan, with the given entry kind.
  PlanEntry EntryKind
  |
  -- | In the executor's module mode, for the given reason.
  ModuleEntry String
  |
  -- | Not at all, since the module exports no @main@.
  NoEntry

-- | Classify the root module's exported @main@ from its home unit graph entry.
mainEntry :: BuildEnv -> HomeModuleId -> IO Entry
mainEntry env (uid, name) = do
  state <- readMVar env.stateVar
  lookupHug state.make.hug uid name >>= \case
    Nothing -> pure (ModuleEntry "module not in the home unit graph")
    Just hmi ->
      pure case find isMain (concatMap availNames (mi_exports hmi.hm_iface)) of
        Nothing -> NoEntry
        Just main ->
          case lookupTypeEnv (md_types hmi.hm_details) main of
            Just (AnId i) -> entryForType (idType i)
            _ -> ModuleEntry "no type for main"
  where
    isMain n = occNameString (nameOccName n) == "main"

entryForType :: Type -> Entry
entryForType ty =
  case classifyMainResultType ty of
    Nothing -> PlanEntry EntryIO
    Just ResultString -> PlanEntry EntryString
    Just ResultShowable -> ModuleEntry "main's result is shown with its Show instance"
    Just ResultCounts -> ModuleEntry "main's result is shown with its Show instance"

-- | The options of the executor's session: the package databases the unit is compiled against, including the
-- external dependency store if the unit uses it.
-- Units with different databases can't share an executor session; see 'withRespawn'.
executorGhcOptions :: BuildEnv -> Unit -> IO [String]
executorGhcOptions env unit = do
  extDb <- readMVar env.extDepsDb
  pure (packageDbArgs unit.ghcArgs ++ foldMap extDbArgs (if null unit.extDeps then Nothing else extDb))
  where
    extDbArgs = \case
      Right db -> ["-package-db", fromOsPath db]
      Left _ -> []

-- | The @-package-db@ options in a list of GHC arguments.
packageDbArgs :: [String] -> [String]
packageDbArgs = \case
  "-package-db" : db : rest -> "-package-db" : db : packageDbArgs rest
  _ : rest -> packageDbArgs rest
  [] -> []

-- | Ensure the executor runs, execute the action, and if the executor couldn't be reached or its session has
-- incompatible options, respawn it and execute once more.
withRespawn ::
  BuildEnv ->
  ExecutorId ->
  IO (Either PlanError (ExecutorPlan, ProcessEvalOutput)) ->
  IO (Either PlanError (ExecutorPlan, ProcessEvalOutput))
withRespawn env executorId execute = do
  seen <- ensureExecutor env executorId
  execute >>= \case
    result
      | needsRespawn result -> respawnExecutor env executorId seen.incarnation *> execute
      | otherwise -> pure result

needsRespawn :: Either PlanError (ExecutorPlan, ProcessEvalOutput) -> Bool
needsRespawn = \case
  Left (ExecutorUnreachable _) -> True
  Right (_, ProcessEvalOutput {result = Right ProcessEvalResult {outcome = EvalOptionsMismatch _}}) -> True
  _ -> False

#else

executeModuleTaskExecutor = executeModuleMode

#endif

-- | Run the task in the executor's module mode, which restores the unit from the cache.
-- Failures to spawn or reach the executor are reported as a failed task by 'executeModuleTaskWith'.
executeModuleMode ::
  BuildEnv ->
  ExecutorId ->
  Unit ->
  GHC.ModuleName ->
  IO (Maybe (TaskResult String), Maybe ProcessStats)
executeModuleMode env executorId =
  executeModuleTaskWith call env
  where
    -- Holding the call lock keeps a concurrent respawn from stopping the process during the call.
    call config = do
      handle <- ensureExecutor env executorId
      withMVar handle.calls \ () -> callExecutor handle config
