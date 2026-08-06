-- | gRPC request handler for the standalone GHC server.
module GhcServer.Handler where

import Common.Grpc (GrpcHandler (..), fromGrpcHandler)
import Control.Concurrent.Chan (Chan, newChan)
import Control.Concurrent.MVar (MVar, newMVar)
import qualified Data.Map.Strict as Map
import GHC (moduleNameString)
import GhcServer.Build (Build, BuildResult (..), awaitBuild, newBuild, newBuildState, scheduleBatch)
import GhcServer.Cabal (discoverCabalProject)
import GhcServer.Data.BuildEnv (BuildEnv (..))
import GhcServer.Data.BuildEvent (newBuildEvents)
import GhcServer.Data.Config (ServerConfig (..))
import qualified GhcServer.Data.Request as Request
import GhcServer.Data.Request (ScheduleRequest (ScheduleRequest), UnitRequest (..))
import GhcServer.Data.Unit (ClientModule (..), Project (..), UnitName (..))
import GhcServer.Log (newLogger)
import GhcServer.Path (outputDirName, tmpDirName)
import GhcServer.Project (discoverProject)
import Network.GRPC.Server.Protobuf (ProtobufMethodsOf)
import Network.GRPC.Server.StreamType (Methods)
import Prelude hiding (log)
import Proto.Worker (Worker)
import System.OsPath ((</>))
import Types.Args (Args (..), emptyArgs)
import Types.FeatureFlags (FeatureFlags (..))
import Types.Grpc (RequestArgs (..))
import Types.Instrument (Event)
import Types.State (WorkerState)

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

-- | Parse a single target specification.
parseTarget :: Project -> String -> Either String (UnitName, UnitRequest)
parseTarget project spec =
  parse (break (== ':') spec)
  where
    parse (unit, suffix) =
      validateUnit project (UnitName unit) =<< parseSelection suffix

    parseSelection = \case
      "" ->
        Right UnitAll
      ":metadata" ->
        Right UnitMetadata
      ":modules" ->
        Right UnitModulesOnly
      ':' : moduleName ->
        Right (UnitModules [ClientModule moduleName])
      _ ->
        Left ("Invalid target: " ++ spec)

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
      _ -> traverse (parseTarget project) targets
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

-- | Everything created at server boot that is needed to serve both the Worker protocol and (optionally) the
-- Instrument protocol: the persistent 'WorkerState', the scheduler, and the discovered project (needed to parse
-- target specs the same way 'ghc-client' does).
data ServerContext =
  ServerContext {
    grpcHandler :: GrpcHandler,
    stateVar :: MVar WorkerState,
    build :: Build,
    project :: Project,
    -- | Channel carrying instrumentation events (metadata\/compile task start\/end) to the Instrument gRPC
    -- service, if the @instrument@ feature is enabled.
    instrChan :: Maybe (Chan Event)
  }

-- | Create the server context: discovers the project, creates the persistent 'WorkerState' and scheduler, and
-- builds the gRPC handler for the Worker protocol.
--
-- Starts the scheduler at boot. Each gRPC request submits a batch and awaits completion.
-- The scheduler persists across requests, accumulating 'WorkerState' and skipping previously completed tasks.
serverContext :: ServerConfig -> IO ServerContext
serverContext config = do
  let
    outputDir = config.projectRoot </> outputDirName
    tmpDir = config.projectRoot </> tmpDirName
  log <- newLogger config.verbose
  project <- if config.cabal
    then discoverCabalProject log config.projectRoot outputDir tmpDir
    else discoverProject config.projectRoot outputDir tmpDir
  stateVar <- newBuildState
  events <- newBuildEvents
  instrChan <- if config.features.instrument then Just <$> newChan else pure Nothing
  extDepsDb <- newMVar Nothing
  let
    env = BuildEnv {
      baseArgs = (emptyArgs Map.empty) {features = config.features},
      projectRoot = config.projectRoot,
      outputDir,
      tmpDir,
      stateVar,
      project,
      log,
      events,
      instrChan,
      extDepsDb
    }
  build <- newBuild config.maxJobs 300 env
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
  pure ServerContext {grpcHandler, stateVar, build, project, instrChan}

-- | Create the gRPC handler for the server.
serverHandler :: ServerConfig -> IO GrpcHandler
serverHandler config = (.grpcHandler) <$> serverContext config

-- | Create the gRPC 'Methods' for the server.
serverMethods :: ServerConfig -> IO (Methods IO (ProtobufMethodsOf Worker))
serverMethods config = fromGrpcHandler <$> serverHandler config
