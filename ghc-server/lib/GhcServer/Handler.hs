-- | gRPC request handler for the standalone GHC server.
module GhcServer.Handler where

import Common.Grpc (GrpcHandler (..))
import Control.Concurrent.Chan (newChan)
import Control.Concurrent.MVar (modifyMVar_, newMVar, readMVar)
import Data.IORef (newIORef)
import Control.Concurrent.STM (atomically, modifyTVar')
import Control.Exception (SomeException (..), displayException, fromException, try)
import Control.Monad (filterM, when)
import Data.List (intercalate)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8Lenient)
import GHC (ModuleName, mkModuleName, moduleNameString)
import GhcServer.Build (Build (..), BuildResult (..), awaitBuild, newBuild, newBuildState, scheduleBatch)
import GhcServer.Build.Schedule (BuildExt (..), BuildStatus, ModuleKey (..), TaskKey (..), emptyBuildExt)
import GhcServer.Cabal (discoverCabalProject, findCabalFile)
import GhcServer.Data.BuildEnv (BuildEnv (..))
import GhcServer.Data.BuildEvent (newBuildEvents)
import GhcServer.Data.Config (ServerConfig (..))
import qualified GhcServer.Data.Request as Request
import GhcServer.Data.Request (ScheduleRequest (ScheduleRequest), UnitRequest (..))
import GhcServer.Data.Unit (ClientModule (..), Project (..), Unit (..), UnitCache (..), UnitName (..))
import GhcServer.Log (emitLog, newLogger)
import GhcServer.Path (cacheDirName, fp, osPath, outputDirName, tmpDirName)
import GhcServer.Project (discoverProject)
import GhcServer.Scheduler (Phase (..), SchedulerResources (..), SchedulerState (..))
import Internal.State (newState)
import Network.GRPC.Common.Protobuf (Proto, defMessage, (&), (.~))
import Network.GRPC.Server.Protobuf (ProtobufMethodsOf)
import Network.GRPC.Server.StreamType (Methods, mkNonStreaming, simpleMethods)
import Prelude hiding (log)
import Proto.GhcServer qualified as GS
import Proto.GhcServer (GhcServer)
import Proto.GhcServer_Fields qualified as Fields
import System.Directory.OsPath (doesDirectoryExist, removeDirectoryRecursive)
import System.Exit (ExitCode (..), exitSuccess)
import System.OsPath ((</>))
import qualified Text.Parsec as Parsec
import qualified Types.Args as Args
import Types.Args (emptyArgs)
import Types.FeatureFlags (FeatureFlags (..))
import Types.Grpc (CommandEnv (..), RequestArgs (..))

-- | Parsed schedule command with optional flags.
data ScheduleCommand =
  ScheduleCommand {
    request :: ScheduleRequest,
    -- | Whether the server should wait for completion before responding.
    scheduleWait :: Bool
  }

validateUnit :: Project -> UnitName -> UnitRequest -> Either String (UnitName, UnitRequest)
validateUnit project name req =
  case Map.lookup name project.units of
    Just _ -> Right (name, req)
    Nothing -> Left ("Unknown unit: " ++ name.string)

-- | Which unit(s) a target spec refers to.
data UnitSelector =
  AllUnits
  |
  NamedUnit String

-- | Which 'UnitRequest' variant a target spec's selector suffix denotes, prior to unit validation.
data Selector =
  SelAll
  |
  SelMetadata
  |
  SelModulesOnly
  |
  SelModule String
  |
  SelExecute
  |
  SelExecuteModule String

-- | Parses the @unitName@\/@*@ prefix of a target spec.
unitSelectorP :: Parsec.Parsec String () UnitSelector
unitSelectorP =
  (AllUnits <$ Parsec.char '*')
  Parsec.<|>
  (NamedUnit <$> Parsec.many1 (Parsec.satisfy (/= ':')))

-- | Parses the optional @:selector@ suffix of a target spec.
selectorP :: Parsec.Parsec String () Selector
selectorP =
  (SelAll <$ Parsec.eof)
  Parsec.<|>
    parseSuffix
  where
    parseSuffix = do
      _ <- Parsec.char ':'
      token <- Parsec.many1 (Parsec.satisfy (/= ':'))
      case token of
        "metadata" -> pure SelMetadata
        "modules" -> pure SelModulesOnly
        "execute" -> pure SelExecute
        moduleName ->
          (SelExecuteModule moduleName <$ (Parsec.char ':' *> Parsec.string "execute"))
          Parsec.<|>
          pure (SelModule moduleName)

-- | Parses a full target spec (@unitSelector@ followed by @selector@).
targetSpecP :: Parsec.Parsec String () (UnitSelector, Selector)
targetSpecP = do
  unit <- unitSelectorP
  sel <- selectorP
  Parsec.eof
  pure (unit, sel)

-- | Resolves a parsed selector suffix into a 'UnitRequest' (independent of which unit(s) it applies to).
selectorRequest :: Selector -> UnitRequest
selectorRequest = \case
  SelAll -> UnitAll
  SelMetadata -> UnitMetadata
  SelModulesOnly -> UnitModulesOnly
  SelExecute -> UnitExecute
  SelModule moduleName -> UnitModules [ClientModule moduleName]
  SelExecuteModule moduleName -> UnitExecuteModules [ClientModule moduleName]

-- | Resolves a parsed target spec against the project, expanding the @"*"@ wildcard to every known unit.
resolveTarget :: Project -> (UnitSelector, Selector) -> Either String [(UnitName, UnitRequest)]
resolveTarget project (unitSel, sel) =
  let req = selectorRequest sel
  in
    case unitSel of
      AllUnits -> Right [(name, req) | name <- Map.keys project.units]
      NamedUnit unit -> (: []) <$> validateUnit project (UnitName unit) req

-- | Parse a single target specification.
--
-- Grammar:
--   - @unitName@ or @*@ - 'UnitAll' (@*@ expands to every unit in the project)
--   - @unitName:metadata@ or @*:metadata@ - 'UnitMetadata'
--   - @unitName:modules@ - 'UnitModulesOnly'
--   - @unitName:ModuleName@ - 'UnitModules' (single module)
--   - @unitName:execute@ or @*:execute@ - 'UnitExecute' (compile + execute every module)
--   - @unitName:ModuleName:execute@ - 'UnitExecuteModules' (compile + execute a single module)
--
-- Returns one entry per matched unit, so the @"*"@ wildcard shares the same code path (and the same grammar) as
-- single-unit targets instead of being handled ad hoc by callers.
parseTarget :: Project -> String -> Either String [(UnitName, UnitRequest)]
parseTarget project spec =
  case Parsec.parse targetSpecP "" spec of
    Left _ -> Left ("Invalid target: " ++ spec)
    Right parsed -> resolveTarget project parsed

-- | Parse schedule arguments from the client's argv.
--
-- Format:
--   @schedule [targets...]@
--
-- Where each target is one of:
--   - @unitName@ build the entire unit (metadata + all modules)
--   - @unitName:metadata@ only run metadata for the unit
--   - @unitName:modules@ compile all modules (skip metadata)
--   - @unitName:ModuleName@ compile a specific module (skip metadata)
--
-- Requests are dispatched in the order specified, allowing the same unit to appear
-- multiple times with different request types.
parseScheduleArgs :: Project -> [String] -> Either String ScheduleCommand
parseScheduleArgs project = \case
  "schedule" : rest -> do
    let (flags, targets) = extractFlags rest
    steps <- case targets of
      [] -> Right [(name, UnitAll) | name <- Map.keys project.units]
      _ -> concat <$> traverse (parseTarget project) targets
    let
      recompile = flags.recompile || flags.rebuild
      rebuild = flags.rebuild
    Right ScheduleCommand {
      request = ScheduleRequest {steps, recompile, rebuild},
      scheduleWait = flags.wait
    }
  other ->
    Left ("Unknown command: " ++ unwords other)
  where
    extractFlags = go Flags {wait = False, recompile = False, rebuild = False}

    go acc = \case
      "--wait" : ts -> go (acc {wait = True} :: Flags) ts
      "--recompile" : ts -> go (acc {recompile = True} :: Flags) ts
      "--rebuild" : ts -> go (acc {rebuild = True} :: Flags) ts
      ts -> (acc, ts)

-- | Intermediate type for extracting flags from schedule argv.
data Flags =
  Flags {
    wait :: Bool,
    recompile :: Bool,
    rebuild :: Bool
  }

-- | Format a build result as a human-readable report.
formatResult :: BuildResult -> [String]
formatResult result
  | result.success =
    ["Build succeeded."]
  | otherwise =
    "Build failed:"
    :
    ["  metadata " ++ u.string ++ ": " ++ msg | (u, msg) <- result.metadataErrors]
    ++
    ["  compile " ++ u.string ++ ":" ++ moduleNameString modName ++ ": " ++ msg | (u, modName, msg) <- result.compileErrors]

-- | Everything created at server boot that is needed to serve both the GhcServer protocol and (optionally) the
-- Instrument protocol. 'WorkerState', 'Project', and the instrumentation channel are all reachable through
-- 'buildEnv' rather than duplicated here.
data ServerContext =
  ServerContext {
    grpcHandler :: GrpcHandler,
    build :: Build,
    buildEnv :: BuildEnv
  }

-- | Create the server context: discovers the project, creates the persistent 'WorkerState' and scheduler, and
-- builds the gRPC handler for the GhcServer protocol.
--
-- Starts the scheduler at boot. Each gRPC request submits a batch and awaits completion.
-- The scheduler persists across requests, accumulating 'WorkerState' and skipping previously completed tasks.
serverContext :: ServerConfig -> IO ServerContext
serverContext config = do
  let
    outputDir = config.projectRoot </> outputDirName
    tmpDir = config.projectRoot </> tmpDirName
  log <- newLogger config.verbose
  projectCabal <- if config.jsonConfig
    then pure Nothing
    else do
      cabal <- findCabalFile config.projectRoot
      traverse (discoverCabalProject log config.projectRoot outputDir tmpDir) cabal
  project <- maybe (discoverProject config.projectRoot outputDir tmpDir) pure projectCabal
  stateVar <- newBuildState
  events <- newBuildEvents
  instrChan <- if config.features.instrument then Just <$> newChan else pure Nothing
  extDepsDb <- newMVar Nothing
  diff <- newMVar Map.empty
  requestIdCounter <- newIORef 0
  let
    env = BuildEnv {
      baseArgs = (emptyArgs Map.empty) {Args.features = config.features},
      projectRoot = config.projectRoot,
      outputDir,
      tmpDir,
      stateVar,
      project,
      log,
      events,
      instrChan,
      extDepsDb,
      diff,
      requestIdCounter
    }
  -- Tracing is tied to the instrument feature flag rather than a dedicated CLI flag: 'instrChan' is 'Just'
  -- exactly when there's an instrument UI to exfiltrate the scheduler's decision log to, and that's also
  -- exactly when 'spawnGhcServer' starts this process. 'emitLog' itself no-ops on a 'Nothing' channel, so
  -- forwarding decisions to it is otherwise a no-op cost not worth guarding separately in production.
  build <- newBuild (\ decision -> emitLog instrChan "scheduler" "debug" (show decision)) config.maxJobs 300 env
  let
    grpcHandler = GrpcHandler \ _commandEnv (RequestArgs argv) ->
      case parseScheduleArgs project argv of
        Left err ->
          pure ([err], 1)
        Right cmd -> do
          scheduleBatch build cmd.request
          if cmd.scheduleWait
          then do
            result <- awaitBuild build
            let
              report = formatResult result
              exitCode = if result.success then 0 else 1
            pure (report, exitCode)
          else
            pure (["Scheduled."], 0)
  pure ServerContext {grpcHandler, build, buildEnv = env}

-- | Create the gRPC handler for the server.
serverHandler :: ServerConfig -> IO GrpcHandler
serverHandler config = (.grpcHandler) <$> serverContext config

-- | Convert protobuf env entries to 'CommandEnv'.
--
-- Duplicated from 'Common.Grpc.commandEnv' because it's pinned to the Buck 'Worker' protocol's generated
-- 'EnvironmentEntry' type; 'ghc-server' has its own, structurally identical but nominally distinct, generated type.
commandEnv :: [Proto GS.ExecuteCommand'EnvironmentEntry] -> CommandEnv
commandEnv =
  CommandEnv .
  Map.fromList .
  fmap \kv -> (fromBs kv.key, fromBs kv.value)
  where
    fromBs = Text.unpack . decodeUtf8Lenient

-- | Handle an 'Execute' request: run the 'GrpcHandler' against the decoded argv\/env and report the result.
--
-- Duplicated from 'Common.Grpc.execute' for the same reason as 'commandEnv' above.
executeCommand :: GrpcHandler -> Proto GS.ExecuteCommand -> IO (Proto GS.ExecuteResponse)
executeCommand handler req = do
  eres <- try (handler.run (commandEnv req.env) (RequestArgs argv))
  (output, exitCode) <-
    case eres of
      Right (output, exitCode) -> pure (output, exitCode)
      Left e@(SomeException e') ->
        case fromException e of
          Just ExitSuccess -> exitSuccess
          _ -> pure (["Uncaught exception: " ++ displayException e'], 1)
  pure $
    defMessage
      & Fields.exitCode .~ exitCode
      & Fields.stderr .~ Text.unlines (Text.pack <$> output)
  where
    argv = Text.unpack . decodeUtf8Lenient <$> req.argv

-- | Which scope a clean target spec refers to: the whole project, a single unit, or a single module within a
-- unit. Deliberately a separate, much simpler grammar than 'parseTarget'\/'UnitRequest' -- clean has no notion of
-- metadata\/modules\/execute selectors, only /how much/ state to discard.
data CleanTarget =
  CleanAll
  |
  CleanUnit UnitName
  |
  CleanModule UnitName ModuleName

-- | Parse a clean target spec: @"*"@ or empty text selects the whole project, a bare unit name selects that
-- unit, and @unitName:moduleName@ selects a single module within a unit.
parseCleanTarget :: Text.Text -> CleanTarget
parseCleanTarget spec
  | Text.null spec || spec == Text.pack "*" = CleanAll
  | otherwise =
    case Text.breakOn (Text.pack ":") spec of
      (unit, rest)
        | Text.null rest -> CleanUnit (UnitName (Text.unpack unit))
        | otherwise -> CleanModule (UnitName (Text.unpack unit)) (mkModuleName (Text.unpack (Text.drop 1 rest)))

-- | The unit a resolved 'TaskKey' belongs to.
taskUnit :: TaskKey p -> UnitName
taskUnit = \case
  MetaTask name -> name
  PendingSource name _ -> name
  ResolvedModule name _ -> name
  PendingExecute name _ -> name
  ExecuteModule name _ -> name

-- | Whether a resolved 'TaskKey' is a compile\/execute task (not metadata) for the given module.
taskIsModule :: UnitName -> ModuleName -> TaskKey 'Resolved -> Bool
taskIsModule name modName = \case
  ResolvedModule u m -> u == name && m == modName
  ExecuteModule u m -> u == name && m == modName
  MetaTask _ -> False

-- | Reset the scheduler's completion\/in-flight bookkeeping to empty, discarding every accumulated 'ext' (module
-- map, stale set) as well. Used for a whole-project clean: after this, the scheduler behaves as if freshly
-- started, so the next request re-derives everything from disk\/source (which the whole-project clean has also
-- just cleared).
--
-- NOTE: this does not touch 'unsatisfied'\/'ready'\/'pending' (in-flight tasks actively being processed by the
-- scheduler loop) or bump the generation counter, since a clean is expected to only be requested between
-- batches (no build in flight). This is a conservative, best-effort coherence improvement rather than a
-- guaranteed-safe reset under concurrent scheduling -- it has not been exercised against a live, mid-build clean.
resetSchedulerState :: Build -> IO ()
resetSchedulerState build =
  atomically $ modifyTVar' schedulerVar \ (state :: SchedulerState TaskKey BuildStatus String BuildExt) ->
    state
      { completed = Map.empty
      , accepted = Map.empty
      , failures = Map.empty
      , resolutions = Map.empty
      , ext = emptyBuildExt
      }
  where
    SchedulerResources {state = schedulerVar} = build.scheduler

-- | Drop every scheduler bookkeeping entry belonging to the given unit: completed\/accepted\/failures entries
-- keyed by a 'TaskKey' for that unit (metadata or any module), the unit's resolutions, and its entries in the
-- accumulated module map\/stale set. Leaves other units' state untouched.
invalidateUnitState :: Build -> UnitName -> IO ()
invalidateUnitState build name =
  atomically $ modifyTVar' schedulerVar \ (state :: SchedulerState TaskKey BuildStatus String BuildExt) ->
    state
      { completed = Map.filterWithKey (\ k _ -> taskUnit k /= name) state.completed
      , accepted = Map.filterWithKey (\ k _ -> taskUnit k /= name) state.accepted
      , failures = Map.filterWithKey (\ k _ -> taskUnit k /= name) state.failures
      , resolutions = Map.filterWithKey (\ k _ -> taskUnit k /= name) state.resolutions
      , ext =
        state.ext
          { moduleMap = Map.filterWithKey (\ k _ -> k.unit /= name) state.ext.moduleMap
          , stale = Set.filter (\ k -> k.unit /= name) state.ext.stale
          }
      }
  where
    SchedulerResources {state = schedulerVar} = build.scheduler

-- | Drop scheduler bookkeeping entries for a single module within a unit (its compile\/execute tasks only --
-- the unit's metadata task is untouched, since clean's module scope is meant to force only that module's own
-- recompilation, not a full unit metadata re-run).
invalidateModuleState :: Build -> UnitName -> ModuleName -> IO ()
invalidateModuleState build name modName =
  atomically $ modifyTVar' schedulerVar \ (state :: SchedulerState TaskKey BuildStatus String BuildExt) ->
    let key = ModuleKey {unit = name, name = modName}
        keep :: forall v. TaskKey 'Resolved -> v -> Bool
        keep k _ = not (taskIsModule name modName k)
    in
      state
        { completed = Map.filterWithKey keep state.completed
        , accepted = Map.filterWithKey keep state.accepted
        , failures = Map.filterWithKey keep state.failures
        , ext =
          state.ext
            { moduleMap = Map.delete key state.ext.moduleMap
            , stale = Set.delete key state.ext.stale
            }
        }
  where
    SchedulerResources {state = schedulerVar} = build.scheduler

-- | Replace 'BuildEnv.stateVar' (the in-memory module graph\/HPT\/HUG) with a fresh 'Types.State.WorkerState'.
-- Used only for a whole-project clean: unit\/module-scoped clean leaves this untouched, since a single shared
-- 'WorkerState' has no unit-level granularity to invalidate piecemeal (documented limitation -- see the clean-key
-- KB entry).
resetWorkerState :: BuildEnv -> IO ()
resetWorkerState env = do
  freshVar <- newState
  fresh <- readMVar freshVar
  modifyMVar_ env.stateVar (const (pure fresh))

-- | Handle a 'Clean' request: remove cache\/output directories for the requested scope and invalidate the
-- corresponding scheduler\/in-memory build state so a subsequent build doesn't skip work it can no longer
-- justify skipping (previously, only on-disk state was removed, which could leave the scheduler believing a
-- module\/unit was still up to date after its cache was deleted from under it -- see the clean-scoping KB entry).
--
-- * 'CleanAll': removes the project's 'output' and 'cache' directories, resets the scheduler's bookkeeping
--   (\'resetSchedulerState\') and the in-memory 'WorkerState' (\'resetWorkerState\'), and clears the accumulated
--   digest diffs.
-- * 'CleanUnit': removes only that unit's cache\/output subdirectories and invalidates only that unit's
--   scheduler entries (\'invalidateUnitState\'). The shared in-memory 'WorkerState' is left alone (see
--   'resetWorkerState'’s haddock).
-- * 'CleanModule': removes no disk state (the unit's cache directory is shared across its modules); only
--   invalidates that module's own scheduler entries (\'invalidateModuleState\'). The most conservative\/safe
--   scope, since Phase 0's digest-based staleness detection means a module's source changes are already
--   detected on the next build without any cache deletion.
cleanCommand :: Build -> BuildEnv -> Proto GS.CleanRequest -> IO (Proto GS.CleanResponse)
cleanCommand build env req =
  case parseCleanTarget req.target of
    CleanAll -> do
      removed <- filterM removeIfExists [env.outputDir, env.projectRoot </> cacheDirName]
      resetSchedulerState build
      resetWorkerState env
      modifyMVar_ env.diff (const (pure Map.empty))
      success (describeRemoved removed)
    CleanUnit name ->
      case Map.lookup name env.project.units of
        Nothing -> failure ("Unknown unit: " ++ name.string)
        Just unit -> do
          removed <- filterM removeIfExists [unit.cache.dir, env.outputDir </> osPath name.string]
          invalidateUnitState build name
          modifyMVar_ env.diff (pure . Map.delete name)
          success (describeRemoved removed)
    CleanModule name modName ->
      case Map.lookup name env.project.units of
        Nothing -> failure ("Unknown unit: " ++ name.string)
        Just _ -> do
          invalidateModuleState build name modName
          success "Invalidated scheduler state for module (no on-disk cache removed)."
  where
    success msg = pure (defMessage & Fields.success .~ True & Fields.message .~ Text.pack msg)
    failure msg = pure (defMessage & Fields.success .~ False & Fields.message .~ Text.pack msg)

    describeRemoved removed
      | null removed = "Nothing to clean."
      | otherwise = "Removed: " ++ intercalate ", " (fp <$> removed)

    removeIfExists dir = do
      exists <- doesDirectoryExist dir
      when exists (removeDirectoryRecursive dir)
      pure exists

-- | Create the gRPC 'Methods' for the server's own protocol ('ghc-server.proto', service 'GhcServer'), serving
-- both 'Execute' (a copy of the Buck worker protocol's RPC of the same name) and 'Clean'.
--
-- 'ServiceMethods'' generated type family lists RPC methods in alphabetical order, not declaration order, hence
-- 'Clean' preceding 'Execute' below.
serverMethods :: ServerContext -> Methods IO (ProtobufMethodsOf GhcServer)
serverMethods ctx =
  simpleMethods
    (mkNonStreaming (cleanCommand ctx.build ctx.buildEnv))
    (mkNonStreaming (executeCommand ctx.grpcHandler))
