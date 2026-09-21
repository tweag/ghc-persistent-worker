module Ghc.Ui.Event.Main where

import Brick (BrickEvent (..), EventM, halt, suspendAndResume', zoom)
import Brick.Forms (formState)
import Control.Lens (Lens', preuse, use, (.=))
import Control.Monad (unless, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (get, gets)
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
import Ghc.Ui.Data.Settings (LocalFlag (ProcessExecute), SettingKind (..), localFlagEnabled)
import Ghc.Ui.Data.WorkerId (WorkerId (..))
import Ghc.Ui.Event.Log (handleLogEvent)
import Ghc.Ui.Event.OpLog (handleOpLogEvent, logMainEvent)
import Ghc.Ui.Event.Popup (focus, handleForm, handleListEventOf, listKeyEvent, openPopup, popupKeyEvent, staticDialog)
import qualified Ghc.Ui.Event.Project as Project
import Ghc.Ui.Event.Sessions (handleSessionsEvent)
import Ghc.Ui.Event.Settings qualified as Settings
import Ghc.Ui.Event.Tasks qualified as Tasks
import Ghc.Ui.GhcDebug (debug)
import Ghc.Ui.Monad (MainM, MonadUi, UiM, server, withApi, withApiSync)
import Ghc.Ui.OpLog (logOp, logOpDebug, logOpLevel)
import qualified Ghc.Ui.Render.Log as Log
import Graphics.Vty (Event (..), Key (..))
import Internal.Debug (debugSocketPathTarget)
import System.OsPath (OsPath)
import System.OsPath.Extra (toOsPath)
import qualified Types.Api as TaskKind
import Types.Api (Target (..), TaskKind, renderTarget)
import Types.Text (showText)

inSession ::
  Lens' SessionState s ->
  (EventM Name s a) ->
  (a -> MainM ()) ->
  MainM ()
inSession lens zoomedAction resultAction =
  lift (getFirst <$> zoom (currentSession . lens) (First . Just <$> zoomedAction)) >>= \case
    Just a -> resultAction a
    Nothing -> logOpDebug "Tried to use the current session with empty state"

inSession_ ::
  Lens' SessionState s ->
  (EventM Name s ()) ->
  MainM ()
inSession_ lens action =
  inSession lens action pure

whenFocused ::
  Name ->
  MainM () ->
  MainM ()
whenFocused name run = do
  current <- use #currentFocus
  when (name == current) run

-- TODO this isn't really used well
withTask ::
  (WorkerId -> Target -> MainM ()) ->
  MainM ()
withTask handler =
  whenFocused Tasks do
    inSession #tasks Tasks.getSelectedTarget \case
      Just (wid, target) -> handler wid target
      Nothing -> logOp "No task selected"

withProjectTargets ::
  (ProjectState -> Maybe a) ->
  (a -> MainM ()) ->
  MainM ()
withProjectTargets select handle =
  inSession #project (gets select) \case
    Just targets ->
      handle targets
    _ ->
      logOp "No project row selected"

inProject :: UiM ProjectState () -> MainM ()
inProject = zoom (currentSession . #project)

writeLogToFile :: MainM ()
writeLogToFile = do
  inSession #log get \ messages -> do
    liftIO $ Text.writeFile "ui.log" (Text.unlines (Log.formatEntry <$> toList messages))
    logOp ("Wrote session log to ui.log")

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
    logOpDebug ("Trigger " <> showText kind <> ": " <> showText target)
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

toggleFeature :: MainM ()
toggleFeature =
  inSession #settings Settings.toggleSelected \case
    Nothing ->
      logOp "No setting selected"
    Just (SettingFeature flag) ->
      withApi \ api -> api.toggleFeature flag
    Just (SettingLocal _) ->
      -- Local settings are not synchronized with the server; 'Ghc.Ui.Event.Settings.toggleSelected' already
      -- flipped the in-memory flag, nothing else to do.
      pure ()

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
    whenFocused StartServer do
      focus Project

  ServerStopped {..} ->
    for_ failedPath \ path ->
      logOp ("Failed to start ghc-server in " <> describeServerRoot path <> ": " <> stderr)

  OpLogMessage {..} -> logOpLevel level message

  CleanCompleted target -> do
    inProject (Project.clearMarks target)
    inSession_ #tasks do
      Tasks.addSeparator ("Cleaned " <> renderTarget target)

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
    logOp "Killing ghc-server"
    server.stop

  KChar 'R' -> do
    logOp "Restarting ghc-server"
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
    withTask \ _ target -> do
      result <- lift $ suspendAndResume' $ debug (debugSocketPathTarget target)
      either (\ err -> logOp ("ghc-debug: " <> Text.pack err)) pure result

  KChar '\t' ->
    case current of
      Tasks -> focus Project
      Project -> focus Settings
      Settings -> focus Tasks
      _ -> pure ()

  _ ->
    lift case current of
      Tasks -> handleListEventOf (currentSession . #tasks) event
      Project -> handleListEventOf (currentSession . #project . #rows) event
      Settings -> handleListEventOf (currentSession . #settings . #rows) event
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
    inSession #settings (gets (localFlagEnabled ProcessExecute)) \ process ->
      triggerTask (fmap Just . Project.selectedExecuteTarget) TaskKind.Execute {process}

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
  KEnter ->
    -- This ensures the cursor is not on a separator
    -- TODO improve
    withTask \ _ _ -> openPopup TaskDetails
  key -> handleGlobalKey Tasks event key

-- | Keys that operate on the feature-flags view specifically: 'Enter' toggles the currently selected flag's
-- checkbox and sends the corresponding API request. Any other key falls through to 'handleGlobalKey'.
settingsKey :: Event -> Key -> MainM ()
settingsKey event = \case
  KEnter -> toggleFeature
  KChar ' ' -> toggleFeature
  key -> handleGlobalKey Settings event key

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

  Settings ->
    keyEvent settingsKey

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
    unless (current == OpLogDebug) do
      logOpDebug ("vty event in " <> showText current <> ": " <> showText event)
    vtyEvent current event
  MouseDown {} -> pure ()
  MouseUp {} -> pure ()

initEvent :: MonadUi m => m ()
initEvent = do
  logOp "Waiting for first session"
  focus StartServer
