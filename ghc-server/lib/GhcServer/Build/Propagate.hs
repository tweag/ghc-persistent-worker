-- | Bridge between the scheduler and GHC: task dispatch, metadata propagation, and resolution.
--
-- When a metadata task completes, the scheduler needs to know which compile tasks to activate
-- and what their module-level dependencies are.  This module computes that resolution data
-- from the unit's @cached_unit.json@ (written by 'runMetadata' for fresh units, or present from
-- a prior build for cached units) and promotes the newly eligible compile tasks.
--
-- The separation from 'GhcServer.Build.Classify' (request expansion) and 'GhcServer.Build'
-- (lifecycle management) keeps each module focused on one concern.
module GhcServer.Build.Propagate where

import Control.Concurrent.Chan (writeChan)
import Control.Monad.Extra (ifM)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Text as Text
import GHC (ModuleName, moduleNameString)
import qualified GHC.Utils.Outputable as O
import GHC.Utils.Outputable (ppr, (<+>))
import GhcServer.Build.Compile (compileSingleModule)
import GhcServer.Build.Execute (executeModuleTask)
import GhcServer.Build.Metadata (runMetadata)
import GhcServer.Build.Schedule (
  BuildExt (..),
  BuildStatus (..),
  ModuleInfo (..),
  ModuleKey (..),
  TaskKey (..),
  buildModuleCachedDeps,
  resolutionsFromModuleMap,
  resolveFromCachedUnit,
  )
import GhcServer.Data.BuildCache (BuildCache (..))
import GhcServer.Data.BuildEnv (BuildEnv (..))
import GhcServer.Data.BuildEvent (BuildEvent (..), logEvent)
import GhcServer.Data.Unit (UnitName (..))
import GhcServer.Scheduler (Phase (..), SchedulerState (..), Task (..), TaskResult (..), addResolutions)
import Types.Instrument (Event (..))
import Types.Log (Logger (..))


-- | Convert a list of error messages to a 'TaskResult'.
taskResultFromErrors :: [(a, String)] -> TaskResult String
taskResultFromErrors = \case
  [] -> TaskSuccess Nothing
  (_, msg) : _ -> TaskFailed msg

-- | Push an instrumentation event to the UI's channel, if instrumentation is enabled.
emitEvent :: BuildEnv -> Event -> IO ()
emitEvent env evt = maybe (pure ()) (`writeChan` evt) env.instrChan

-- | Send a 'CompileStart' event for a metadata or compile task about to run.
emitTaskStart :: BuildEnv -> String -> IO ()
emitTaskStart env target = emitEvent env CompileStart {target, canDebug = False}

-- | Send a 'CompileEnd' event for a metadata or compile task that just finished, deriving the exit code,
-- stderr content, and any exfiltrated result payload from the task's 'TaskResult' (see
-- 'GhcServer.Build.Execute.executeModuleTask' for the only task kind that ever produces a payload).
emitTaskEnd :: BuildEnv -> String -> TaskResult String -> IO ()
emitTaskEnd env target result =
  emitEvent env CompileEnd {
    target,
    exitCode = case result of
      TaskSuccess _ -> 0
      TaskFailed _ -> 1,
    stderr = case result of
      TaskSuccess _ -> ""
      TaskFailed msg -> msg,
    result = case result of
      TaskSuccess mResultStr -> Text.unpack <$> mResultStr
      TaskFailed _ -> Nothing
  }

-- | Run an instrumented task: emits 'CompileStart' before and 'CompileEnd' after, deriving the target's
-- display text from the given unit\/module description.
withTaskEvents :: BuildEnv -> String -> IO (TaskResult String) -> IO (TaskResult String)
withTaskEvents env target action = do
  emitTaskStart env target
  result <- action
  emitTaskEnd env target result
  pure result

-- | Skip metadata for a cached unit.
skipMetadata :: BuildEnv -> UnitName -> IO (TaskResult String)
skipMetadata env name = do
  env.log.debug ("Skipping metadata (cached): " ++ name.string)
  logEvent env.events (MetadataSkipped name)
  pure (TaskSuccess Nothing)

-- | Attempt to skip compilation for a module that was not directly requested.
--
-- If the module's @.dyn_hi@ interface file exists (indicating it was compiled
-- in a prior build), the compilation is skipped.  Otherwise, compilation
-- proceeds normally.
skipCompileIfCached :: BuildCache -> BuildExt -> BuildEnv -> UnitName -> ModuleName -> IO (TaskResult String)
skipCompileIfCached cache ext env unit moduleName =
  ifM (cache.interfaceExists unit moduleName)
  skip
  (compile ext env unit moduleName)
  where
    skip = do
      env.log.debugD ("Skipping compile (cached):" <+> ppr unit O.<> ":" O.<> ppr moduleName)
      logEvent env.events (CompileSkipped unit moduleName)
      pure (TaskSuccess Nothing)

-- | Compile a single module.
--
-- Before compilation, assembles 'CachedDeps' from the 'BuildExt' module map
-- and passes them to the worker for HPT pre-population.
compile :: BuildExt -> BuildEnv -> UnitName -> ModuleName -> IO (TaskResult String)
compile ext env name modName = do
  env.log.debugD ("Compile:" <+> ppr name O.<> ":" O.<> ppr modName)
  logEvent env.events (ModuleCompiled name modName)
  let modKey = ModuleKey {unit = name, name = modName}
  let cachedDeps = buildModuleCachedDeps ext.moduleMap modKey
  (result, _) <- compileSingleModule env name modName cachedDeps
  pure (taskResultFromErrors [(unit, errors) | (unit, _, errors) <- result])

-- | Dispatch a resolved build task to the appropriate GHC operation.
--
-- Metadata tasks run 'computeMetadata' via 'runMetadata'; compile tasks run
-- 'compileModuleWithDepsInHpt' via 'compileSingleModule'.  Both paths support
-- cache-based skipping that emulates an external build system omitting unchanged
-- work items.
dispatchTask :: BuildCache -> BuildEnv -> BuildExt -> Task TaskKey 'Resolved BuildStatus -> IO (TaskResult String)
dispatchTask cache env ext task = case task.key of
  MetaTask name
    | not task.value.rebuild && task.value.cached -> skipMetadata env name
    | otherwise ->
      withTaskEvents env (name.string ++ ":metadata") (taskResultFromErrors . fst <$> runMetadata env name)
  ResolvedModule name modName
    | shouldSkipCompile -> skipCompileIfCached cache ext env name modName
    | otherwise -> withTaskEvents env (name.string ++ ":" ++ moduleNameString modName) (compile ext env name modName)
    where
      shouldSkipCompile = not task.value.rebuild && not task.enabled
  ExecuteModule name modName ->
    executeModuleTask env ext name modName >>= \case
      Nothing -> pure (TaskSuccess Nothing)
      Just result -> do
        emitTaskStart env target
        emitTaskEnd env target result
        pure result
    where
      target = name.string ++ ":" ++ moduleNameString modName ++ ":execute"

-- | Compute the resolution map for a unit's compile tasks.
--
-- Loads @cached_unit.json@ from disk and reconstructs the resolution map
-- from its module data.  This works for both fresh units (whose cache was
-- just written by 'runMetadata') and cached units (whose cache exists from
-- a prior build).
--
-- TODO cache is always written in metadata, so it will always be available.
-- The event is pointless and the Nothing case should fail the task.
computeResolutions ::
  BuildCache ->
  BuildEnv ->
  UnitName ->
  SchedulerState TaskKey BuildStatus String BuildExt ->
  IO (Either String (Map ModuleKey ModuleInfo))
computeResolutions cache env name _state =
  cache.loadUnit name >>= traverse \case
    Nothing -> do
      env.log.debug ("computeResolutions: no cached_unit.json for " ++ name.string)
      pure Map.empty
    Just cu -> do
      logEvent env.events (ResolutionComputed name)
      pure (resolveFromCachedUnit name env.outputDir cu)

-- | Propagate a task's completion to the scheduler state.
--
-- On successful metadata completion, computes a resolution map for the unit's
-- compile tasks and promotes those that are enabled.  For all other completions
-- (compile tasks, failures), the state is returned unchanged.
propagateCompletion ::
  BuildCache ->
  BuildEnv ->
  TaskKey 'Resolved ->
  TaskResult String ->
  SchedulerState TaskKey BuildStatus String BuildExt ->
  IO (SchedulerState TaskKey BuildStatus String BuildExt)
propagateCompletion cache env (MetaTask name) (TaskSuccess _) state =
  computeResolutions cache env name state >>= \case
    Left err -> do
      env.log.debug ("Cache decode failure during propagation: " ++ err)
      pure state {failures = Map.insert (MetaTask name) err state.failures}
    Right newModules -> do
      let
        newResolutions = resolutionsFromModuleMap state.ext.moduleMap newModules
        ext' = state.ext {moduleMap = Map.union newModules state.ext.moduleMap}
      pure (addResolutions newResolutions state {ext = ext'})
propagateCompletion _ _ _ _ state =
  pure state
