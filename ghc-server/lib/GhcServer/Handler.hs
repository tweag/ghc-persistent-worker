-- | gRPC request handler for the standalone GHC server.
module GhcServer.Handler where

import Common.Grpc (GrpcHandler (..))
import Control.Concurrent.Chan (newChan)
import Control.Concurrent.MVar (newMVar)
import Control.Exception (SomeException (..), displayException, fromException, try)
import Control.Monad (filterM, when)
import Data.List (intercalate)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8Lenient)
import GHC (moduleNameString)
import GhcServer.Build (Build, BuildResult (..), awaitBuild, newBuild, newBuildState, scheduleBatch)
import GhcServer.Cabal (discoverCabalProject, findCabalFile)
import GhcServer.Data.BuildEnv (BuildEnv (..))
import GhcServer.Data.BuildEvent (newBuildEvents)
import GhcServer.Data.Config (ServerConfig (..))
import qualified GhcServer.Data.Request as Request
import GhcServer.Data.Request (ScheduleRequest (ScheduleRequest), UnitRequest (..))
import GhcServer.Data.Unit (ClientModule (..), Project (..), UnitName (..))
import GhcServer.Log (newLogger)
import GhcServer.Path (cacheDirName, fp, outputDirName, tmpDirName)
import GhcServer.Project (discoverProject)
import Network.GRPC.Common.Protobuf (Proto, defMessage, (&), (.~))
import Network.GRPC.Server.Protobuf (ProtobufMethodsOf)
import Network.GRPC.Server.StreamType (Methods, mkNonStreaming, simpleMethods)
import Prelude hiding (log)
import Proto.GhcServer (GhcServer)
import Proto.GhcServer qualified as GS
import Proto.GhcServer_Fields qualified as Fields
import System.Directory.OsPath (doesDirectoryExist, removeDirectoryRecursive)
import System.Exit (ExitCode (..), exitSuccess)
import System.OsPath (OsPath, (</>))
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
      diff
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

-- | Handle a 'Clean' request: remove the project's 'output' and 'cache' directories.
--
-- The paths are computed here from the project root the same way 'serverContext' computes them, rather than
-- accepted from the client, so that a client never has to duplicate that assumption to safely clean a project (this
-- replaces the instrument UI's previous client-side 'cleanGhcServer', which did exactly that).
cleanCommand :: OsPath -> Proto GS.CleanRequest -> IO (Proto GS.CleanResponse)
cleanCommand projectRoot _ = do
  removed <- filterM removeIfExists dirs
  let
    msg
      | null removed = "Nothing to clean."
      | otherwise = "Removed: " ++ intercalate ", " (fp <$> removed)
  pure $
    defMessage
      & Fields.success .~ True
      & Fields.message .~ Text.pack msg
  where
    dirs = [projectRoot </> outputDirName, projectRoot </> cacheDirName]

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
    (mkNonStreaming (cleanCommand ctx.buildEnv.projectRoot))
    (mkNonStreaming (executeCommand ctx.grpcHandler))
