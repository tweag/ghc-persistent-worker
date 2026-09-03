module Ghc.Ui.Event.Main where

import Brick (BrickEvent (..), halt, suspendAndResume', zoom)
import Brick.Forms (formState)
import Control.Lens (preuse, use, (.=))
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans (lift)
import Data.Foldable (for_, toList, traverse_)
import Data.Maybe (fromMaybe)
import Data.Monoid (First (..))
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Data.Text.IO as Text
import qualified Ghc.Ui.Data.Main as Main
import Ghc.Ui.Data.Main (MainEvent (..), currentSession)
import qualified Ghc.Ui.Data.Name as Name
import Ghc.Ui.Data.Name (Name (..))
import Ghc.Ui.Data.Project (ProjectState)
import Ghc.Ui.Data.ServerApi (ServerApi (..))
import Ghc.Ui.Data.ServerHandlers (ServerHandlers (..))
import Ghc.Ui.Data.ServerProcess (ServerConfig, describeServerRoot)
import Ghc.Ui.Data.Session (SessionState (..))
import qualified Ghc.Ui.Data.Sessions as Sessions
import Ghc.Ui.Data.WorkerId (WorkerId (..))
import Ghc.Ui.Event.Log (handleLogEvent, logMessage)
import Ghc.Ui.Event.OpLog (handleOpLogEvent, logMainEvent)
import Ghc.Ui.Event.Popup (focus, handleForm, handleListEventOf, listKeyEvent, openPopup, popupKeyEvent, staticDialog)
import qualified Ghc.Ui.Event.Project as Project
import Ghc.Ui.Event.Sessions (handleSessionsEvent)
import Ghc.Ui.Event.Tasks qualified as Tasks
import Ghc.Ui.GhcDebug (debug)
import Ghc.Ui.Monad (MainM, MonadUi, UiM, server, withApi, withApiSync)
import Ghc.Ui.OpLog (logOp, logOpDebug)
import qualified Ghc.Ui.Render.Log as Log
import Graphics.Vty (Event (..), Key (..))
import Internal.Debug (debugSocketPathTarget)
import System.OsPath (OsPath)
import System.OsPath.Extra (toOsPath)
import qualified Types.Api as TaskKind
import Types.Api (Target (..), TaskKind, renderTarget)
import Types.Text (showText)

withTarget ::
  (WorkerId -> Target -> MainM ()) ->
  MainM ()
withTarget handler = do
  current <- use #currentFocus
  First mtarget <- case current of
    Tasks -> zoom (currentSession . #tasks) (First <$> Tasks.getSelectedTarget)
    _ -> pure (First Nothing)
  case mtarget of
    Nothing -> logOp "No task selected"
    Just (wid, target) -> handler wid target

withProjectTargets ::
  (ProjectState -> Maybe a) ->
  (a -> MainM ()) ->
  MainM ()
withProjectTargets select handle = do
  project <- preuse (currentSession . #project)
  case select =<< project of
    Just targets ->
      handle targets
    _ ->
      logOp "No project row selected"

inProject :: UiM ProjectState () -> MainM ()
inProject = zoom (currentSession . #project)

writeLogToFile :: MainM ()
writeLogToFile = do
  preuse currentSession >>= \case
    Nothing -> logOp "Cannot write the log to a file when no session is active"
    Just state -> do
      let rendered = Log.formatEntry <$> toList state.log
      liftIO $ Text.writeFile "ui.log" (Text.unlines rendered)
      logOp ("Wrote " <> showText (length rendered) <> " log entries to ui.log")

-- TODO resetting the session state to show the splash screen is sloppy
requestQuit :: MainM ()
requestQuit = do
  logOp "Shutting down"
  withApiSync \ api -> api.clean TargetProject
  #sessions .= Sessions.initialState
  server.shutdown

nonEmptyPath :: Text -> Maybe OsPath
nonEmptyPath = \case
  "" -> Nothing
  path -> Just (toOsPath (Text.unpack path))

triggerTask ::
  Foldable t =>
  (ProjectState -> Maybe (t Target)) ->
  TaskKind ->
  MainM ()
triggerTask select kind =
  withProjectTargets select $ traverse_ \ target -> do
    logMessage "trigger" "debug" ("Trigger " <> showText kind <> ": " <> showText target)
    withApi \ api -> api.triggerTask target kind

startServer :: ServerConfig -> MainM ()
startServer input = do
  server.start input
  focus Project

evictBytecode ::
  Target ->
  MainM ()
evictBytecode target = do
  withApi \ api -> api.evictBytecode target
  inProject (Project.evictedBco target)

handleMainEvent ::
  MainEvent ->
  MainM ()
handleMainEvent = \case
  SetTime t ->
    #currentTime .= t

  Main.Sessions event -> do
    lift $ zoom #sessions do
      handleSessionsEvent event
    -- The first session to appear is auto-selected (see 'Sessions.handleEvent's 'StartSession' case)
    -- without the user dismissing any modal, so the idle screen's initial focus (the start-server form) has to be
    -- moved off explicitly here once that happens, mirroring what the other modals' "hide" logic does on Esc\/Enter.
    current <- use #currentFocus
    when (current == StartServer) do
      focus Project

  ProcessLog level stream line ->
    logMessage level stream line

  ServerStopped {..} ->
    for_ failedPath \ path ->
      logOp ("Failed to start ghc-server in " <> describeServerRoot path <> ": " <> stderr)

  OpLogMessage message -> logOp message

  CleanCompleted target -> do
    inProject (Project.clearMarks target)
    lift $ zoom (currentSession . #tasks) (Tasks.addSeparator ("Cleaned " <> renderTarget target))

  ShutdownComplete -> do
    logOp "Shutdown complete"
    lift halt

handleGlobalKey ::
  Name ->
  Event ->
  Key ->
  MainM ()
handleGlobalKey current event = \case
  KEsc -> requestQuit

  KChar 'q' -> requestQuit

  KChar 's' ->
    openPopup Name.Sessions

  KChar 'S' ->
    openPopup StartServer

  KChar 'K' -> do
    logMessage "server" "info" "Killing ghc-server"
    server.stop

  KChar 'R' -> do
    logMessage "server" "info" "Restarting ghc-server"
    server.restart

  KChar 'c' -> do
    project <- preuse (currentSession . #project)
    let target = fromMaybe TargetProject (Project.selectedCleanTarget =<< project)
    logOp ("Cleaning " <> renderTarget target)
    withApi \ api -> api.clean target

  KChar 'l' ->
    openPopup OpLogDebug

  KChar 'L' ->
    openPopup Log

  KChar 'W' -> writeLogToFile

  KChar 'd' ->
    withTarget \ _ target -> do
      result <- lift $ suspendAndResume' $ debug (debugSocketPathTarget target)
      either (\ err -> logOp ("ghc-debug: " <> Text.pack err)) pure result

  KChar '\t' ->
    case current of
      Tasks -> focus Project
      Project -> focus Tasks
      _ -> pure ()

  _ ->
    lift case current of
      Tasks -> handleListEventOf (currentSession . #tasks) event
      Project -> handleListEventOf (currentSession . #project . #rows) event
      _ -> pure ()

-- | Keys that operate on the project view specifically: build\/execute triggers (which read their targets from
-- the currently selected project-tree row) and eviction\/expand-toggle, which are also meaningful there. Any
-- other key falls through to 'handleGlobalKey'.
--
-- TODO move to Project
projectKey ::
  Event ->
  Key ->
  MainM ()
projectKey event = \case
  KChar 'm' ->
    triggerTask Project.selectedMetadataTargets TaskKind.Metadata

  KChar 'r' ->
    triggerTask Project.selectedCompileTargets (TaskKind.Build True)

  KChar 'b' ->
    triggerTask Project.selectedCompileTargets (TaskKind.Build False)

  KChar 'x' ->
    triggerTask (fmap Just . Project.selectedExecuteTarget) TaskKind.Execute

  KChar 'e' ->
    withProjectTargets Project.selectedEvictTarget \ target ->
      evictBytecode target

  KEnter ->
    inProject Project.toggleExpand

  key -> handleGlobalKey Project event key

-- | Keys that operate on the tasks view specifically: opening task details (either inline, via 'p', or as a
-- popup, via Enter) and eviction, which targets the bytecode owning the currently selected task's target
-- instead of a project-tree row. Any other key falls through to 'handleGlobalKey'.
--
-- TODO wtf is this inline details thing
tasksKey :: Event -> Key -> MainM ()
tasksKey event = \case
  -- KChar 'p' ->
  --   withTarget \ _ _ -> #currentFocus .= TaskDetails

  KEnter ->
    -- This ensures the cursor is not on a separator
    -- TODO improve
    withTarget \ _ _ -> openPopup TaskDetails

  key -> handleGlobalKey Tasks event key

keyEvent ::
  (Event -> Key -> MainM ()) ->
  Event ->
  MainM ()
keyEvent handle = \case
  event@(EvKey key []) -> handle event key
  _ -> pure ()

vtyEvent :: Name -> Event -> MainM ()
vtyEvent = \case
  Name.Sessions ->
    lift . listKeyEvent #sessions True

  StartServer -> do
    popupKeyEvent True (lift . handleForm #serverForm) do
      input <- formState <$> use #serverForm
      startServer input

  TaskDetails ->
    staticDialog

  OpLogDebug ->
    lift . popupKeyEvent False (zoom #opLog . handleOpLogEvent) (pure ())

  Log ->
    lift . popupKeyEvent False (zoom currentSession . handleLogEvent) (pure ())

  Project ->
    keyEvent projectKey

  Tasks ->
    keyEvent tasksKey

  current ->
    keyEvent (handleGlobalKey current)

handleUiEvent :: BrickEvent Name MainEvent -> MainM ()
handleUiEvent = \case
  AppEvent event -> do
    logMainEvent event
    handleMainEvent event
  VtyEvent event -> do
    current <- use #currentFocus
    logOpDebug ("vty event in " <> showText current <> ": " <> showText event)
    vtyEvent current event
  MouseDown {} -> pure ()
  MouseUp {} -> pure ()

initEvent :: MonadUi m => m ()
initEvent = do
  logOp "Waiting for first session"
  focus StartServer
