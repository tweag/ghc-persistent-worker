{-# LANGUAGE CPP #-}

-- | Child side of the bytecode plan protocol for persistent executors (see 'GhcWorker.Executor.Plan' for the
-- parent side and the project note @task-executor-bytecode-transport@).
--
-- The executor keeps one interface-free GHC session for its lifetime, in a dedicated thread (so that the session's
-- interpreter stays alive between calls).
-- For each 'PlanRequest', it decodes the GBC payloads, links them through a 'Linkables' hook that returns exactly
-- the plan (bypassing @getLinkDeps@, which would need interfaces), looks up the entry in the name cache and runs it.
-- The loader state persists in the session's interpreter, so modules sent by earlier requests stay linked.
module Internal.Executor.Load where

import Control.Concurrent.MVar (MVar, newMVar)
import Data.Text (Text)
import Internal.Executor.Output (Attach)
import Types.Executor.Protocol (EvalOutcome (..), SetupError (..))
import Types.ExecutorPlan (Entry (..), PlanRequest (..))

#if defined(GBC)

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (modifyMVar, newEmptyMVar, putMVar, takeMVar, withMVar)
import Control.Concurrent.STM (TVar, atomically, check, modifyTVar', newTVarIO, readTVar)
import Control.Exception (SomeException, displayException, finally, try)
import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (for_)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Text (pack)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import GHC (Ghc, getSession)
import GHC.Builtin.Names (gHC_INTERNAL_GHCI_HELPERS)
import GHC.ByteCode.Serialize (readBinByteCode)
import GHC.Driver.Env (HscEnv (..), hscInterp)
import GHC.Iface.Env (lookupNameCache)
import GHC.Linker.Loader (loadName)
import GHC.Linker.Types (LinkDeps (..), Linkable, Linkables (..), mkLinkableUsage, mkModuleByteCodeLinkable)
import GHC.Runtime.Interpreter (evalIO, evalString)
import GHC.Types.Name.Occurrence (mkVarOcc)
import GHC.Types.Unique.DSet (emptyUniqDSet)
import GHC.Unit.Types (UnitId, mkModule, stringToUnit, stringToUnitId)
import Internal.Executor.GbcBuffer (decodeByteCode)
import Internal.Executor.Output (withAttached)
import Internal.Executor.PayloadArena (SegmentMappings, newSegmentMappings, sliceBytes)
import Internal.Session (simpleSessionWithDebugLog)
import Internal.State (newState)
import Internal.State.Linkables (loadDLL_)
import Language.Haskell.Syntax.Module.Name (mkModuleName)
import System.Environment (setEnv)
import System.Timeout (timeout)
import qualified Types.Args as Args
import Types.Args (emptyArgs)
import Types.ExecutorPlan (EntryKind (..), ExtraLib (..), Payload (..))
import Types.Settings (defaultSettings)

#endif

-- | A long-lived session serving jobs in its own thread, together with the GHC options it was created with.
-- Every job runs in a thread of its own, so entries run concurrently, but the plans are linked one at a time in the
-- order of their generations (see 'linkInOrder').
data PlanSession =
  PlanSession {
    options :: [String],
#if defined(GBC)
    jobs :: MVar (HscEnv -> IO ()),
    -- | Native libraries loaded so far, only accessed with 'linkLock' held.
    libs :: IORef (Set ExtraLib),
    -- | Mappings of shared payload segments.
    segments :: SegmentMappings,
    -- | Held while a plan is linked.
    linkLock :: MVar (),
    -- | The highest generation of the plans that have been linked (or failed to link) so far.
    linked :: TVar Int
#else
    jobs :: ()
#endif
  }

-- | The executor's session, created by the first plan request.
type PlanSessionVar = MVar (Maybe PlanSession)

newPlanSessionVar :: IO PlanSessionVar
newPlanSessionVar = newMVar Nothing

#if defined(GBC)

-- | Run a 'Ghc' action in a fresh 'Types.State.WorkerState' with the given GHC options.
freshSession :: [String] -> Ghc a -> IO (Either Text a)
freshSession options ma = do
  state <- newState defaultSettings
  maybe (Left "GHC session failed") Right <$>
    simpleSessionWithDebugLog state (emptyArgs []) {Args.ghcOptions = options} ma

-- | Start a session thread that runs submitted jobs, each in a thread of its own, with its 'HscEnv' until the process
-- exits.
startPlanSession :: [String] -> IO (Either Text PlanSession)
startPlanSession options = do
  jobs <- newEmptyMVar
  libs <- newIORef Set.empty
  segments <- newSegmentMappings
  linkLock <- newMVar ()
  linked <- newTVarIO 0
  ready <- newEmptyMVar
  _ <- forkIO do
    result <- try @SomeException $ freshSession options do
      env <- getSession
      liftIO do
        putMVar ready (Right ())
        forever (takeMVar jobs >>= \ job -> forkIO (job env))
    putMVar ready (either (Left . pack . displayException) (fmap (const ())) result)
  fmap (const PlanSession {options, jobs, libs, segments, linkLock, linked}) <$> takeMVar ready

-- | Run a job in a thread of the session and wait for its result.
-- The thread is attached to the call's output while the job runs.
runInSession :: PlanSession -> Attach -> (HscEnv -> IO a) -> IO (Either Text a)
runInSession session attach job = do
  result <- newEmptyMVar
  putMVar session.jobs \ env -> try @SomeException (withAttached attach (job env)) >>= putMVar result
  either (Left . pack . displayException) Right <$> takeMVar result

-- | The session for the request's options, started on first use.
-- A session can't change its options, so a request with different ones is answered with 'OptionsMismatch',
-- upon which the parent respawns the executor.
-- Concurrent first requests share the session that the first one starts.
ensurePlanSession :: PlanSessionVar -> [String] -> IO (Either SetupError PlanSession)
ensurePlanSession var options =
  modifyMVar var \case
    Just session
      | session.options == options -> pure (Just session, Right session)
      | otherwise ->
        pure (Just session, Left (OptionsMismatch "The executor's session was started with different GHC options"))
    Nothing ->
      startPlanSession options >>= \case
        Right session -> pure (Just session, Right session)
        Left err -> pure (Nothing, Left (LoadFailed err))

-- | How long a plan waits for its predecessors to be linked before it goes ahead regardless, in microseconds.
-- A predecessor can be missing if the parent planned it but couldn't deliver it.
linkWaitMicros :: Int
linkWaitMicros = 10_000_000

-- | Run the link phase of the plan with the given generation, after the plans with lower generations.
--
-- The parent derives each plan's delta from the modules of the plans before it, so a plan can only be linked after
-- those, but requests for concurrent calls may arrive in any order.
-- Generations increase by one per plan, so a plan waits until the generation before it has been linked.
-- Plans are linked one at a time.
linkInOrder :: PlanSession -> Int -> IO a -> IO a
linkInOrder session generation link = do
  void $ timeout linkWaitMicros $ atomically do
    done <- readTVar session.linked
    check (generation <= done + 1)
  withMVar session.linkLock \ () ->
    link `finally` atomically (modifyTVar' session.linked (max generation))

-- | Load the native libraries the session hasn't loaded yet.
loadExtraLibs :: PlanSession -> HscEnv -> [ExtraLib] -> IO ()
loadExtraLibs session env requested = do
  loaded <- readIORef session.libs
  let new = Set.fromList requested Set.\\ loaded
  for_ new \ lib -> loadDLL_ env (hscInterp env) lib.paths lib.name
  writeIORef session.libs (loaded <> new)

-- | A 'Linkables' hook that ignores the requested modules and returns the plan's linkables and units.
planLinkables :: [Linkable] -> [UnitId] -> Linkables
planLinkables linkables units =
  Linkables {
    linkablesResolve = \ _ _ -> pure (),
    linkablesSelect = \ _ () -> pure LinkDeps {
      ldNeededLinkables = linkables,
      ldAllLinkables = mkLinkableUsage <$> linkables,
      ldUnits = units,
      ldNeededUnits = emptyUniqDSet
    }
  }

-- | Decode the payloads with linkable times derived from the generation, so that a module sent again replaces the
-- previously linked version.
decodePayloads :: PlanSession -> HscEnv -> PlanRequest -> IO [Linkable]
decodePayloads session env request =
  traverse (fmap (mkModuleByteCodeLinkable time) . decodePayload) request.payloads
  where
    decodePayload = \case
      PayloadFile path -> readBinByteCode env path
      PayloadShared slice -> decodeByteCode env =<< sliceBytes session.segments slice

    time = posixSecondsToUTCTime (fromIntegral request.generation)

-- | Decode and link the plan, then run the entry.
-- Linking is ordered with 'linkInOrder', the entry runs concurrently with other plans.
-- Returns 'Left' for failures before the entry runs (i.e. the executor's loaded state is unknown), and the entry's
-- outcome otherwise.
runPlan :: PlanSession -> HscEnv -> PlanRequest -> IO (Either Text EvalOutcome)
runPlan session env0 request =
  try @SomeException (linkInOrder session request.generation link) >>= \case
    Left err -> pure (Left (pack (displayException err)))
    Right (env, fhv) -> Right <$> (runEntry fhv `finally` flushInterpHandles env)
  where
    link = do
      loadExtraLibs session env0 request.extraLibs
      linkables <- decodePayloads session env0 request
      let env = env0 {hsc_linkables = \ _ _ -> pure (planLinkables linkables (stringToUnitId <$> request.units))}
      name <- lookupNameCache env.hsc_NC entryModule (mkVarOcc request.entry.name)
      (fhv, _, _) <- loadName interp env name
      pure (env, fhv)

    runEntry fhv = do
      for_ request.env (uncurry setEnv)
      either (EvalFailed . pack . displayException) id <$> try @SomeException case request.entry.kind of
        EntryIO -> EvalSucceeded Nothing <$ evalIO interp fhv
        EntryString -> EvalSucceeded . Just . pack <$> evalString interp fhv

    entryModule = mkModule (stringToUnit request.entry.unit) (mkModuleName request.entry.module_)

    interp = hscInterp env0

-- | Flush the interpreted program's standard handles, like GHCi does after each statement.
-- In a static build the linked code has its own copy of @ghc-internal@, and hence its own buffered @stdout@, which
-- the executor's output capture doesn't see; dynamic executables share the host's handles.
-- Failures are ignored, since they must not mask the entry's outcome.
flushInterpHandles :: HscEnv -> IO ()
flushInterpHandles env =
  void $ try @SomeException do
    name <- lookupNameCache env.hsc_NC gHC_INTERNAL_GHCI_HELPERS (mkVarOcc "flushAll")
    (fhv, _, _) <- loadName (hscInterp env) env name
    evalIO (hscInterp env) fhv
-- | Serve a plan request in the executor's session.
executePlan :: PlanSessionVar -> Attach -> PlanRequest -> IO (Either SetupError EvalOutcome)
executePlan var attach request =
  ensurePlanSession var request.ghcOptions >>= \case
    Left err -> pure (Left err)
    Right session ->
      runInSession session attach (\ env -> runPlan session env request) >>= \case
        Left err -> pure (Left (LoadFailed err))
        Right (Left err) -> pure (Left (LoadFailed err))
        Right (Right outcome) -> pure (Right outcome)

#else

executePlan :: PlanSessionVar -> Attach -> PlanRequest -> IO (Either SetupError EvalOutcome)
executePlan _ _ _ =
  pure (Left (LoadFailed ("Bytecode plans require a GHC with the GBC backport" :: Text)))

#endif
