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

import Control.Concurrent.MVar (readMVar)
import Data.Foldable (for_)
import Data.IORef (atomicModifyIORef')
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import qualified Data.Text as Text
import GHC (ModuleName, moduleNameString)
import qualified GHC.Utils.Outputable as O
import GHC.Utils.Outputable (ppr, (<+>))
import GhcServer.Build.Compile (compileSingleModule)
import GhcServer.Build.Diff (changedModuleKeys, moduleGraphDelta, staleClosure, UnitDiff (..))
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
import GhcServer.Log (emitLog)
import GhcServer.Log qualified as Log
import GhcServer.Scheduler (Phase (..), SchedulerState (..), Task (..), TaskResult (..), addResolutions)
import GhcWorker.Grpc (pushBytecodeState)
import Types.Instrument (Event (..))
import Types.Log (Logger (..))


-- | Convert a list of error messages to a 'TaskResult'.
taskResultFromErrors :: [(a, String)] -> TaskResult String
taskResultFromErrors = \case
  [] -> TaskSuccess Nothing
  (_, msg) : _ -> TaskFailed msg

-- | Push an instrumentation event to the UI's channel, if instrumentation is enabled.
emitEvent :: BuildEnv -> Event -> IO ()
emitEvent env = Log.emitEvent env.instrChan

-- | Push a snapshot of the bytecode cache to the UI's channel, if instrumentation is enabled. Called after every
-- 'emitTaskEnd', since a compile\/metadata\/execute task's session-store step (see 'Internal.State.withState') is
-- the only place the cache actually changes.
emitBytecodeState :: BuildEnv -> IO ()
emitBytecodeState env = for_ env.instrChan (pushBytecodeState env.stateVar)

-- | Allocate a fresh, server-lifetime-unique request id for a task-dispatch instance (see 'dispatchTask'),
-- included in the 'CompileStart'\/'CompileEnd'\/'PhaseStart'\/'PhaseEnd' events that instance emits, so the
-- @instrument@ UI can match events to the exact task instance instead of matching by target text (which
-- collides when the same target is dispatched more than once, e.g. as both a direct request and a transitive
-- dependency of another request).
nextRequestId :: BuildEnv -> IO Int
nextRequestId env = atomicModifyIORef' env.requestIdCounter \ n -> (n + 1, n)

-- | Send a 'CompileStart' event for a metadata or compile task about to run.
emitTaskStart :: BuildEnv -> Int -> String -> IO ()
emitTaskStart env requestId target = emitEvent env CompileStart {target, canDebug = False, requestId}

-- | Send a 'CompileEnd' event for a metadata or compile task that just finished, deriving the exit code,
-- stderr content, and any exfiltrated result payload from the task's 'TaskResult' (see
-- 'GhcServer.Build.Execute.executeModuleTask' for the only task kind that ever produces a payload).
emitTaskEnd :: BuildEnv -> Int -> String -> TaskResult String -> IO ()
emitTaskEnd env requestId target result = do
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
      TaskFailed _ -> Nothing,
    requestId
  }
  emitBytecodeState env

-- | Run an instrumented task: emits 'CompileStart' before and 'CompileEnd' after, deriving the target's
-- display text from the given unit\/module description.
withTaskEvents :: BuildEnv -> Int -> String -> IO (TaskResult String) -> IO (TaskResult String)
withTaskEvents env requestId target action = do
  emitTaskStart env requestId target
  result <- action
  emitTaskEnd env requestId target result
  pure result

-- | Skip metadata for a unit whose Phase 0 analysis found no changes.
skipMetadata :: BuildEnv -> UnitName -> IO (TaskResult String)
skipMetadata env name = do
  env.log.debug ("Skipping metadata (unchanged): " ++ name.string)
  logEvent env.events (MetadataSkipped name)
  pure (TaskSuccess Nothing)

-- | Compile a single module.
--
-- Before compilation, assembles 'CachedDeps' from the 'BuildExt' module map
-- and passes them to the worker for HPT pre-population.
compile :: BuildExt -> BuildEnv -> UnitName -> ModuleName -> Int -> IO (TaskResult String)
compile ext env name modName requestId = do
  env.log.debugD ("Compile:" <+> ppr name O.<> ":" O.<> ppr modName)
  logEvent env.events (ModuleCompiled name modName)
  let modKey = ModuleKey {unit = name, name = modName}
  let cachedDeps = buildModuleCachedDeps ext.moduleMap modKey
  (result, _) <- compileSingleModule env name modName cachedDeps requestId
  pure (taskResultFromErrors [(unit, errors) | (unit, _, errors) <- result])

-- | Dispatch a resolved build task to the appropriate GHC operation.
--
-- Dispatch is blind: all decisions (whether metadata must run, which modules are stale) were made
-- during classification (Phase 0) and metadata propagation (Phase 2).  A compile task only exists
-- for stale modules, so it always compiles.
dispatchTask :: BuildEnv -> BuildExt -> Task TaskKey 'Resolved BuildStatus -> IO (TaskResult String)
dispatchTask env ext task = case task.key of
  MetaTask name
    | task.value.runMeta -> do
      requestId <- nextRequestId env
      withTaskEvents env requestId (name.string ++ ":metadata") (taskResultFromErrors . fst <$> runMetadata env name)
    | otherwise -> skipMetadata env name
  ResolvedModule name modName -> do
    requestId <- nextRequestId env
    withTaskEvents env requestId (name.string ++ ":" ++ moduleNameString modName) (compile ext env name modName requestId)
  ExecuteModule name modName -> do
    requestId <- nextRequestId env
    executeModuleTask env ext name modName requestId >>= \case
      Nothing -> pure (TaskSuccess Nothing)
      Just result -> do
        emitTaskStart env requestId target
        emitTaskEnd env requestId target result
        pure result
    where
      target = name.string ++ ":" ++ moduleNameString modName ++ ":execute"

-- | Compute the resolution map for a unit's compile tasks.
--
-- Loads @cached_unit.json@ from disk and reconstructs the resolution map from its module data. Called only
-- after a metadata task succeeds, and 'runMetadata' always writes @cached_unit.json@ on success -- so a missing
-- file here indicates the metadata step and the cache are out of sync, and is treated as a task failure rather
-- than silently resolving to no modules.
computeResolutions ::
  BuildCache ->
  BuildEnv ->
  UnitName ->
  SchedulerState TaskKey BuildStatus String BuildExt ->
  IO (Either String (Map ModuleKey ModuleInfo))
computeResolutions cache env name _state =
  cache.loadUnit name >>= \case
    Left err -> pure (Left err)
    Right Nothing -> pure (Left ("computeResolutions: missing cached_unit.json for " ++ name.string ++ " after successful metadata"))
    Right (Just cu) -> do
      logEvent env.events (ResolutionComputed name)
      pure (Right (resolveFromCachedUnit name env.outputDir cu))

-- | Propagate a task's completion to the scheduler state.
--
-- On successful metadata completion, runs the Phase 2 analysis: seeds the staleness set with
-- the unit's changed modules (Phase 0), the module-graph delta against the pre-refresh graph,
-- and (for @--recompile@) the unit's entire module set; extends the current generation's stale
-- closure by downstream reachability over the merged module graph; and derives resolutions
-- restricted to stale modules.  For all other completions the state is returned unchanged.
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
      diffs <- readMVar env.diff
      let
        unitDiff = Map.lookup name diffs
        seeds = case unitDiff of
          Nothing -> Map.keysSet newModules
          Just d ->
            changedModuleKeys d.changed newModules
            <> moduleGraphDelta d.oldModules newModules
            <> (if d.forceAll then Map.keysSet newModules else Set.empty)
        merged = Map.union newModules state.ext.moduleMap
        -- Staleness accumulates within a generation (so a later unit's closure can follow
        -- edges into an earlier unit's stale modules) but must not survive across
        -- generations: each request re-derives its own staleness from the digest records on
        -- disk, and inheriting the previous request's set would re-schedule work that has
        -- already been done.
        priorStale
          | state.ext.staleGen == state.generation = state.ext.stale
          | otherwise = Set.empty
        stale = staleClosure (seeds <> priorStale) merged
        newResolutions = resolutionsFromModuleMap stale state.ext.moduleMap newModules
        ext' = BuildExt {moduleMap = merged, stale, staleGen = state.generation}
      emitLog env.instrChan (name.string ++ ":propagate") "debug" $
        "gen=" ++ show state.generation
          ++ " seeds=" ++ show (Set.toList seeds)
          ++ " priorStale=" ++ show (Set.toList priorStale)
          ++ " stale=" ++ show (Set.toList stale)
          ++ " newResolutions=" ++ show (Map.keys newResolutions)
      pure (addResolutions newResolutions state {ext = ext'})
propagateCompletion _ _ _ _ state =
  pure state
