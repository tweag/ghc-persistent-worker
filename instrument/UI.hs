{-# LANGUAGE TemplateHaskell #-}

module UI (module UI, customMainWithDefaultVty) where

import Brick.AttrMap (attrMap)
import Brick.Forms (Form, FormFieldState, editTextField, formState, handleFormEvent, newForm, renderForm, (@@=))
import Brick.Main (App (..), customMainWithDefaultVty, getVtyHandle, halt, showFirstCursor, suspendAndResume')
import Brick.Types (BrickEvent (..), EventM, Widget)
import Brick.Util (on)
import Brick.Widgets.Border (borderWithLabel)
import Brick.Widgets.Border.Style (unicodeRounded)
import Brick.Widgets.Center (center)
import Brick.Widgets.Core (joinBorders, modifyDefAttr, str, strWrap, vBox, withBorderStyle, (<+>))
import Brick.Widgets.Edit (editFocusedAttr)
import Brick.Widgets.List (listSelectedAttr, listSelectedElement, listSelectedElementL, listSelectedFocusedAttr)
import Control.Exception (handle)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (for_)
import Data.Monoid (First (..))
import Data.Text qualified as Text
import Data.Time (UTCTime (..), fromGregorian)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Graphics.Vty qualified as V
import Graphics.Vty.Attributes.Color
import Grpc (evictBytecode, sendOptions, triggerExecuteText, triggerRebuild, triggerRebuildText)
import Internal.Debug (debugSocketPath)
import Lens.Micro.Platform (
  Lens',
  Traversal',
  _2,
  each,
  filtered,
  lens,
  makeLenses,
  packed,
  preuse,
  use,
  zoom,
  (%=),
  (.=),
  )
import Network.GRPC.Client (Connection)
import Types.State (Options (..), defaultOptions)
import Types.Target (TargetSpec)
import UI.ActiveTasks qualified as ActiveTasks
import UI.BytecodeBrowser qualified as BytecodeBrowser
import UI.GhcDebug (debug)
import UI.LogViewer qualified as LogViewer
import UI.Session qualified as Session
import UI.SessionSelector qualified as SessionSelector
import UI.TaskTree qualified as TaskTree
import UI.Types (
  Name (..),
  WorkerId,
  canDebugAttr,
  disabledAttr,
  evictedAttr,
  pendingEvictionAttr,
  taskFailedAttr,
  taskRunningAttr,
  taskSucceededAttr,
  )
import UI.Utils (handleListEventOf, popup)

data Event
  = SendOptions (Maybe WorkerId)
  | SetTime UTCTime
  | SessionSelectorEvent SessionSelector.Event
  | TriggerRebuild WorkerId TargetSpec
  | -- | Trigger a build for the given target text (the 'b' key, one event per module when a unit header is
    -- selected), equivalent to @ghc-client $project TARGET@. The target is always @unitName:moduleName@; the
    -- 'm' key dispatches the same event constructor but with an @unitName:metadata@ target instead, as computed
    -- by 'TaskTree.selectedCompileTargets'\/'TaskTree.selectedMetadataTarget'.
    TriggerBuild WorkerId Text.Text
  | -- | Trigger execution of all modules in a unit (the 'x' key). Only fires when a unit header is selected in
  -- the project view, as computed by 'TaskTree.selectedExecuteTarget'. The target text is a bare unit name,
  -- @unitName:moduleName@ for a single module, or the sentinel @"*"@ for the whole project.
    TriggerExecute WorkerId Text.Text
  | BytecodeBrowserEvent BytecodeBrowser.Event
  | RequestEvict Text.Text (Maybe Text.Text)
  | -- | Start (or connect to) a @ghc-server@ instance, as requested via the capital-@S@ key binding's popup.
    -- Carries the user-entered project path (empty means "use the current directory") and extra CLI options to
    -- pass to @ghc-server@.
    RequestServe Text.Text Text.Text
  | -- | A line of stdout\/stderr read from a @ghc-server@ subprocess spawned by this client (see
    -- 'ServeGhcServer.spawnGhcServer'). Carries the stream name (@"stdout"@\/@"stderr"@) and the line text;
    -- dispatched into the current session's server-log viewer via 'logClient'.
    ProcessLog Text.Text Text.Text
  | -- | Dispatched by @Main@'s @startServer@ closure immediately upon being invoked (before forking the background
    -- startup thread), carrying the project path text the user entered (empty means the current directory). Sets
    -- 'serverStatus' so the main window's placeholder message reflects the in-progress startup instead of the
    -- generic "waiting for first session" text.
    ServerStarting Text.Text
  | -- | Dispatched when a @ghc-server@ subprocess spawned by this client terminates with a failing exit code.
    -- Carries the project path text and the process's captured stderr; replaces the startup message in the main
    -- window with an error display.
    ServerFailed Text.Text Text.Text

-- | Status of the most recent server-start request, driving the placeholder message shown in the main window
-- while no session is connected yet (see 'drawUI').
data ServerStatus
  = -- | No server-start request has ever been made in this session.
    ServerIdle
  | -- | A server-start request is in flight for the given project path (empty means "current directory").
    ServerLaunching Text.Text
  | -- | The most recent server-start request's spawned subprocess terminated with a failing exit code. Carries
    -- the project path and the process's captured stderr.
    ServerLaunchFailed Text.Text Text.Text

-- | Input fields for the "start ghc-server" popup (the capital-@S@ key binding).
data ServeInput = ServeInput
  { _serveInputPath :: Text.Text
  , _serveInputOptions :: Text.Text
  }

defaultServeInput :: ServeInput
defaultServeInput = ServeInput{_serveInputPath = Text.empty, _serveInputOptions = Text.empty}

data State = State
  { _sessions :: SessionSelector.State
  , _options :: Form Options Event Name
  , _serveForm :: Form ServeInput Event Name
  , _currentFocus :: Name
  , _currentTime :: UTCTime
  , -- | Starts (or connects to) a @ghc-server@ instance for the given project path (empty for the current
    -- directory) and extra options, dispatched from the capital-@S@ popup. Supplied by @Main@, which owns the
    -- event channel and the socket-listening logic.
    _startServer :: Text.Text -> [String] -> IO ()
  , -- | Kills the @ghc-server@ instance most recently started\/ensured by this session (the capital-@K@ key
    -- binding). Supplied by @Main@, which is the only place that tracks the underlying 'ProcessHandle'. A no-op
    -- (beyond logging) when no such process is tracked, e.g. the client only ever connected to an
    -- already-running server rather than starting one itself.
    _killServer :: IO ()
  , -- | Kills then restarts the @ghc-server@ instance most recently started\/ensured by this session, reusing the
    -- same project path\/extra options (the capital-@R@ key binding). Supplied by @Main@.
    _restartServer :: IO ()
  , -- | Removes the tracked @ghc-server@ project's @cache@ and @output@ directories (the capital-@C@ key binding).
    -- Supplied by @Main@.
    _cleanServer :: IO ()
  , -- | Status of the most recent server-start request; drives the placeholder message shown in the main window
    -- while no session is connected (see 'drawUI'). Updated by the 'ServerStarting'\/'ServerFailed' events.
    _serverStatus :: ServerStatus
  }

makeLenses ''State

servePathLens :: Lens' ServeInput Text.Text
servePathLens = lens (._serveInputPath) (\s v -> s{_serveInputPath = v})

serveOptionsLens :: Lens' ServeInput Text.Text
serveOptionsLens = lens (._serveInputOptions) (\s v -> s{_serveInputOptions = v})

ghcOptionsLens :: Lens' Options Text.Text
ghcOptionsLens =
  lens
    (.extraGhcOptions)
    (\opts s -> opts{extraGhcOptions = s})
    . packed

initialState :: (Text.Text -> [String] -> IO ()) -> IO () -> IO () -> IO () -> State
initialState onStartServer onKill onRestart onClean =
  State
    { _sessions = SessionSelector.initialState
    , _options = newForm optionFields defaultOptions
    , _serveForm = newForm serveFields defaultServeInput
    , _currentFocus = TaskTree
    , _currentTime = UTCTime (fromGregorian 1970 1 1) 0
    , _startServer = onStartServer
    , _killServer = onKill
    , _restartServer = onRestart
    , _cleanServer = onClean
    , _serverStatus = ServerIdle
    }

optionFields :: [Options -> FormFieldState Options Event Name]
optionFields =
  [ (str "Extra GHC Options: " <+>) @@= editTextField ghcOptionsLens OEExtraGhcOptions (Just 1)
  ]

serveFields :: [ServeInput -> FormFieldState ServeInput Event Name]
serveFields =
  [ (str "Project path: " <+>) @@= editTextField servePathLens SOPath (Just 1)
  , (str "Extra options: " <+>) @@= editTextField serveOptionsLens SOExtraOptions (Just 1)
  ]

drawUI :: State -> [Widget Name]
drawUI State{..} =
  ( case _currentFocus of
      SessionSelector -> [SessionSelector.draw _sessions]
      OptionsEditor -> [drawOptionsEditor _options]
      ServeOptions -> [drawServeEditor _serveForm]
      TaskDetails -> let task = session >>= listSelectedElement . Session._activeTasks in maybe [] (pure . ActiveTasks.drawTaskDetails . snd) task
      LogViewer -> maybe [] (pure . drawLogViewer . Session._logViewer) session
      _ -> []
  )
    ++ [ vBox $
          [ joinBorders $
              withBorderStyle unicodeRounded $
                maybe
                  (borderWithLabel (str " GHC Persistent Worker ") $ center $ strWrap (statusText _serverStatus))
                  (Session.draw _currentFocus _currentTime)
                  session
          , modifyDefAttr (`V.withStyle` V.italic) $
              str
          " q:quit   Tab:switch pane   Enter:expand/details   b:build   m:metadata   x:execute   r:trigger rebuild   d:debug   o:options   s:sessions   S:start server   K:kill server   R:restart server   C:clean cache   t:sort bytecode   e:evict bytecode   L:log"
          ]
       ]
 where
  session = snd . snd <$> listSelectedElement _sessions

-- | Placeholder text shown in the main window while no session is connected, reflecting the most recent
-- server-start request's status (see 'ServerStatus').
statusText :: ServerStatus -> String
statusText ServerIdle = "Waiting for first session"
statusText (ServerLaunching path) = "Starting ghc-server in " ++ describePath path
statusText (ServerLaunchFailed path stderrText) =
  "Failed to start ghc-server in " ++ describePath path ++ ":\n\n" ++ Text.unpack stderrText

describePath :: Text.Text -> String
describePath path
  | Text.null path = "the current directory"
  | otherwise = Text.unpack path

drawOptionsEditor :: Form Options Event Name -> Widget Name
drawOptionsEditor form = popup 50 "Session Options" $ renderForm form

drawServeEditor :: Form ServeInput Event Name -> Widget Name
drawServeEditor form =
  popup 50 "Start ghc-server" $
    vBox
      [ renderForm form
      , str " "
      , str "Leaving the path empty starts ghc-server in the current directory."
      ]

drawLogViewer :: LogViewer.State -> Widget Name
drawLogViewer = popup 80 "Server Log" . LogViewer.draw LogViewer

currentSession :: Traversal' State Session.State
currentSession = sessions . listSelectedElementL . _2

-- | Append an entry to the current session's server-log viewer, tagged as coming from the client (as opposed to
-- entries pushed by the server via 'Instr.LogMessage'). Used to replace ad hoc @hPutStrLn stderr@ debug prints,
-- which corrupt a running Brick app's terminal rendering.
logClient :: Text.Text -> Text.Text -> EventM Name State ()
logClient level message = do
  ms <- liftIO $ round . (* 1000) <$> getPOSIXTime
  currentSession . Session.logViewer %=
    LogViewer.addEntry LogViewer.Entry {target = Text.pack "client", level, message, timestampMs = ms}

beep :: EventM Name State ()
beep = do
  vty <- getVtyHandle
  liftIO $ V.ringTerminalBell $ V.outputIface vty

-- | Runs 'handler' against the currently selected active-task target, or beeps if none is selected. Shared by the
-- 'd' (debug) and 'r' (rebuild) key handlers.
withTarget :: (WorkerId -> TargetSpec -> EventM Name State ()) -> EventM Name State ()
withTarget handler = do
  current <- use currentFocus
  First mtarget <- case current of
    ActiveTasks -> zoom (currentSession . Session.activeTasks) (First <$> ActiveTasks.getSelectedTarget)
    _ -> pure (First Nothing)
  case mtarget of
    Nothing -> beep
    Just (wid, target) -> handler wid target

-- | The connection used for RPCs targeting the first worker of the currently focused session: bytecode cache
-- queries/evictions and the 'b' build (metadata) action.
firstWorker :: EventM Name State (Maybe (WorkerId, Connection))
firstWorker = do
  workers <- use (currentSession . Session.workers)
  pure $ case workers of
    (w : _) -> Just (w._workerId, w._connection)
    [] -> Nothing

bcoConnection :: EventM Name State (Maybe Connection)
bcoConnection = fmap snd <$> firstWorker

handleEvent :: BrickEvent Name Event -> EventM Name State ()
handleEvent (AppEvent (SetTime t)) = currentTime .= t
handleEvent (AppEvent (SendOptions mwid)) = do
  opts <- use options
  workers <- use (currentSession . Session.workers)
  let workers' = case mwid of
        Nothing -> workers
        Just wid -> filter (\w -> w._workerId == wid) workers
  for_ workers' $ \worker -> do
    liftIO $
      handle @IOError (\_ -> pure ()) $
        sendOptions (Session._connection worker) (formState opts)
handleEvent (AppEvent (TriggerRebuild wid target)) = do
  mworker <- preuse (currentSession . Session.workers . each . filtered (\w -> Session._workerId w == wid))
  for_ mworker $ \worker -> do
    liftIO $ triggerRebuild (Session._connection worker) target
handleEvent (AppEvent (TriggerBuild wid target)) = do
  mworker <- preuse (currentSession . Session.workers . each . filtered (\w -> Session._workerId w == wid))
  for_ mworker $ \worker -> do
    liftIO $ triggerRebuildText (Session._connection worker) target
handleEvent (AppEvent (TriggerExecute wid target)) = do
  mworker <- preuse (currentSession . Session.workers . each . filtered (\w -> Session._workerId w == wid))
  case mworker of
    Nothing -> logClient (Text.pack "debug") (Text.pack ("TriggerExecute: no worker found for " ++ show wid ++ " (target=" ++ Text.unpack target ++ ")"))
    Just worker -> do
      logClient (Text.pack "debug") (Text.pack ("TriggerExecute: dispatching target=" ++ Text.unpack target ++ " to worker=" ++ show wid))
      liftIO $ triggerExecuteText (Session._connection worker) target
handleEvent (AppEvent (BytecodeBrowserEvent evt)) =
  zoom (currentSession . Session.bcoBrowser) (BytecodeBrowser.handleEvent evt)
handleEvent (AppEvent (RequestEvict unitId modName)) = do
  mconn <- bcoConnection
  for_ mconn $ \conn -> liftIO $ evictBytecode conn unitId (maybe Text.empty id modName)
  zoom (currentSession . Session.bcoBrowser) (BytecodeBrowser.handleEvent (BytecodeBrowser.Evicted unitId modName))
handleEvent (AppEvent (SessionSelectorEvent evt)) =
  zoom sessions (SessionSelector.handleEvent evt)
handleEvent (AppEvent (RequestServe path opts)) = do
  action <- use startServer
  liftIO $ action path (words (Text.unpack opts))
handleEvent (AppEvent (ProcessLog stream line)) =
  logClient (Text.pack "info") (stream <> Text.pack ": " <> line)
handleEvent (AppEvent (ServerStarting path)) = serverStatus .= ServerLaunching path
handleEvent (AppEvent (ServerFailed path stderrText)) = serverStatus .= ServerLaunchFailed path stderrText
handleEvent (VtyEvent evt) = do
  current <- use currentFocus
  case current of
    SessionSelector -> do
      let hide = currentFocus .= TaskTree
      case evt of
        V.EvKey V.KEsc [] -> hide
        V.EvKey V.KEnter [] -> hide
        V.EvKey (V.KChar 's') [] -> hide
        _ -> handleListEventOf sessions evt
    OptionsEditor -> do
      let hide = do
            currentFocus .= TaskTree
            handleEvent (AppEvent (SendOptions Nothing))
      case evt of
        V.EvKey V.KEsc [] -> hide
        V.EvKey V.KEnter [] -> hide
        _ -> zoom options (handleFormEvent (VtyEvent evt))
    ServeOptions -> do
      let hide = currentFocus .= TaskTree
      case evt of
        V.EvKey V.KEsc [] -> hide
        V.EvKey V.KEnter [] -> do
          input <- formState <$> use serveForm
          hide
          handleEvent (AppEvent (RequestServe input._serveInputPath input._serveInputOptions))
        _ -> zoom serveForm (handleFormEvent (VtyEvent evt))
    TaskDetails -> do
      let hide = currentFocus .= ActiveTasks
      case evt of
        V.EvKey V.KEsc [] -> hide
        V.EvKey V.KEnter [] -> hide
        _ -> handleListEventOf (currentSession . Session.activeTasks) evt
    LogViewer -> do
      let hide = currentFocus .= TaskTree
      case evt of
        V.EvKey V.KEsc [] -> hide
        V.EvKey (V.KChar 'q') [] -> hide
        V.EvKey (V.KChar 'L') [] -> hide
        _ -> handleListEventOf (currentSession . Session.logViewer . LogViewer.rowsLens) evt
    _ -> case evt of
      V.EvKey V.KEsc [] -> halt
      V.EvKey (V.KChar 'q') [] -> halt
      V.EvKey (V.KChar 's') [] -> do
        currentFocus .= SessionSelector
      V.EvKey (V.KChar 'o') [] -> do
        currentFocus .= OptionsEditor
      V.EvKey (V.KChar 'S') [] -> do
        currentFocus .= ServeOptions
      V.EvKey (V.KChar 'K') [] -> do
        logClient (Text.pack "info") (Text.pack "Killing ghc-server")
        action <- use killServer
        liftIO action
      V.EvKey (V.KChar 'R') [] -> do
        logClient (Text.pack "info") (Text.pack "Restarting ghc-server")
        action <- use restartServer
        liftIO action
      V.EvKey (V.KChar 'C') [] -> do
        logClient (Text.pack "info") (Text.pack "Cleaning ghc-server cache/output directories")
        action <- use cleanServer
        liftIO action
      V.EvKey (V.KChar 'L') [] -> do
        currentFocus .= LogViewer
      V.EvKey (V.KChar 'd') [] -> do
        withTarget $ \_wid target ->
          suspendAndResume' $
            debug (debugSocketPath target)
      V.EvKey (V.KChar 'r') [] -> do
        withTarget $ \wid target ->
          handleEvent (AppEvent (TriggerRebuild wid target))
      V.EvKey (V.KChar 'b') [] -> do
        mtree <- preuse (currentSession . Session.taskTree)
        mfirst <- firstWorker
        case (mtree >>= TaskTree.selectedCompileTargets, mfirst) of
          (Just targets, Just (wid, _)) -> for_ targets \target -> handleEvent (AppEvent (TriggerBuild wid target))
          _ -> beep
      V.EvKey (V.KChar 'm') [] -> do
        mtree <- preuse (currentSession . Session.taskTree)
        mfirst <- firstWorker
        case (mtree >>= TaskTree.selectedMetadataTargets, mfirst) of
          (Just targets, Just (wid, _)) -> for_ targets \target -> handleEvent (AppEvent (TriggerBuild wid target))
          _ -> beep
      V.EvKey (V.KChar 'x') [] -> do
        mtree <- preuse (currentSession . Session.taskTree)
        mfirst <- firstWorker
        case (mtree >>= TaskTree.selectedExecuteTarget, mfirst) of
          (Just target, Just (wid, _)) -> do
            logClient (Text.pack "debug") (Text.pack ("x key: selected target=" ++ Text.unpack target ++ ", worker=" ++ show wid))
            handleEvent (AppEvent (TriggerExecute wid target))
          (Nothing, _) -> do
            logClient (Text.pack "debug") (Text.pack "x key: no row selected")
            beep
          (_, Nothing) -> do
            logClient (Text.pack "debug") (Text.pack "x key: no worker available")
            beep
      V.EvKey (V.KChar 't') [] | current == BytecodeBrowser ->
        handleEvent (AppEvent (BytecodeBrowserEvent BytecodeBrowser.ToggleSort))
      V.EvKey (V.KChar 'e') [] | current == BytecodeBrowser -> do
        mbco <- preuse (currentSession . Session.bcoBrowser)
        case mbco >>= BytecodeBrowser.selectedTarget of
          Nothing -> beep
          Just (unitId, modName) -> handleEvent (AppEvent (RequestEvict unitId modName))
      V.EvKey (V.KChar '\t') [] -> do
        let next = case current of
              ActiveTasks -> TaskTree
              TaskTree -> BytecodeBrowser
              BytecodeBrowser -> ActiveTasks
              _ -> current
        currentFocus .= next
      V.EvKey V.KEnter [] -> case current of
        ActiveTasks ->
          withTarget \_ _ -> currentFocus .= TaskDetails
        TaskTree ->
          zoom (currentSession . Session.taskTree) (TaskTree.handleEvent TaskTree.ToggleExpand)
        BytecodeBrowser ->
          zoom (currentSession . Session.bcoBrowser) (BytecodeBrowser.handleEvent BytecodeBrowser.ToggleExpand)
        _ -> pure ()
      _ -> case current of
        ActiveTasks -> handleListEventOf (currentSession . Session.activeTasks) evt
        TaskTree -> handleListEventOf (currentSession . Session.taskTree . TaskTree.rowsLens) evt
        BytecodeBrowser -> handleListEventOf (currentSession . Session.bcoBrowser . BytecodeBrowser.rowsLens) evt
        _ -> pure ()
handleEvent MouseDown{} = pure ()
handleEvent MouseUp{} = pure ()

app :: App State Event Name
app =
  App
    { appDraw = drawUI
    , appStartEvent = pure ()
    , appHandleEvent = handleEvent
    , appAttrMap =
        const $
          attrMap
            V.defAttr
            [ (editFocusedAttr, brightWhite `on` blue)
            , (listSelectedAttr, brightWhite `on` brightBlack)
            , (listSelectedFocusedAttr, brightWhite `on` blue)
            , (disabledAttr, V.withStyle V.defAttr V.dim)
            , (canDebugAttr, V.withStyle V.defAttr V.bold)
            , (evictedAttr, V.withStyle (V.defAttr `V.withForeColor` brightBlack) V.dim)
            , (pendingEvictionAttr, V.withStyle V.defAttr V.italic)
            , (taskRunningAttr, V.defAttr `V.withForeColor` yellow)
            , (taskSucceededAttr, V.defAttr `V.withForeColor` green)
            , (taskFailedAttr, V.defAttr `V.withForeColor` red)
            ]
    , appChooseCursor = showFirstCursor
    }
