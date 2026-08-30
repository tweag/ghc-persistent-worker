{-# LANGUAGE TemplateHaskell #-}

module UI (module UI, customMainWithDefaultVty) where

import Brick.AttrMap (AttrName, attrMap)
import Brick.Forms (Form, FormFieldState, editTextField, formState, handleFormEvent, newForm, renderForm, (@@=))
import Brick.Main (App (..), customMainWithDefaultVty, getVtyHandle, halt, showFirstCursor, suspendAndResume')
import Brick.Types (
  BrickEvent (..),
  Context (availHeight, availWidth),
  EventM,
  Location (..),
  Result (image),
  Size (Greedy),
  Widget (..),
  getContext,
  )
import Brick.Util (on)
import Brick.Widgets.Border (hBorder)
import Brick.Widgets.Border.Style (unicodeRounded)
import Brick.Widgets.Center (centerLayer, hCenterLayer)
import Brick.Widgets.Core (
  Padding (Pad),
  addResultOffset,
  fill,
  hBox,
  hLimitPercent,
  joinBorders,
  modifyDefAttr,
  padLeft,
  padTop,
  str,
  vBox,
  withAttr,
  withBorderStyle,
  (<+>),
  )
import Brick.Widgets.Edit (editFocusedAttr)
import Brick.Widgets.List (listSelectedAttr, listSelectedElement, listSelectedElementL, listSelectedFocusedAttr)
import Control.Exception (handle)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (for_)
import Data.Monoid (First (..))
import Data.Text qualified as Text
import Data.Time (UTCTime (..), fromGregorian)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Graphics.Vty qualified as V
import Graphics.Vty.Attributes.Color
import Grpc (evictBytecode, sendOptions, triggerExecuteText, triggerRebuildText)
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
import UI.OpLog qualified as OpLog
import UI.Session qualified as Session
import UI.SessionSelector qualified as SessionSelector
import UI.TaskTree qualified as TaskTree
import UI.Types (
  Name (..),
  WorkerId,
  canDebugAttr,
  disabledAttr,
  evictedAttr,
  haskellLogoArrowAttr,
  haskellLogoEqualsAttr,
  haskellLogoLambdaAttr,
  opLogIndicatorAttr,
  opLogTextAttr,
  pendingEvictionAttr,
  startServerLabelAttr,
  taskFailedAttr,
  taskRunningAttr,
  taskSucceededAttr,
  )
import UI.Utils (handleListEventOf, popup)

data Event
  = SendOptions (Maybe WorkerId)
  | SetTime UTCTime
  | SessionSelectorEvent SessionSelector.Event
  | -- | Trigger a build for the given target text ('b' forces an incremental recompile of the target, 'm'
    -- targets the owning unit's metadata, 'r' forces a full rebuild of the target that discards the server's
    -- stored source-digest record first -- see 'GhcServer.Build.Diff'). One event per module when a unit
    -- header\/the project root is selected, equivalent to @ghc-client $project TARGET@\/@ghc-client --rebuild
    -- $project TARGET@. The 'Bool' is forwarded verbatim as the RPC's @rebuild@ field. Targets are computed by
    -- 'TaskTree.selectedCompileTargets'\/'TaskTree.selectedMetadataTargets'.
    TriggerBuild WorkerId Text.Text Bool
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
    -- startup thread), carrying the project path text the user entered (empty means the current directory).
    -- Recorded via 'logOp' so the operational log reflects the in-progress startup.
    ServerStarting Text.Text
  | -- | Dispatched when a @ghc-server@ subprocess spawned by this client terminates with a failing exit code.
    -- Carries the project path text and the process's captured stderr; recorded via 'logOp'.
    ServerFailed Text.Text Text.Text
  | -- | A generic operational message dispatched from outside 'EventM' (e.g. Main's background shutdown
    -- sequence, see 'ShutdownComplete'), recorded via 'logOp'.
    OpLogMessage Text.Text
  | -- | Dispatched once the background shutdown sequence started by the quit key has finished (see Main's
    -- 'requestShutdown'). Triggers the actual 'halt'.
    ShutdownComplete

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
  , -- | Runs the background shutdown sequence (clean\/kill the tracked @ghc-server@ instance, if any) and, once
    -- finished, dispatches 'ShutdownComplete' to actually 'halt' the app. Non-blocking: forks its own background
    -- thread and returns immediately, so the quit key ('requestQuit') can clear the session list and log the
    -- "Shutting down" message without freezing the UI while shutdown proceeds. Supplied by @Main@.
    _shutdown :: IO ()
  , -- | Operational message log (see 'UI.OpLog'): high-visibility status\/lifecycle messages, shown centered while
    -- no session is connected (see 'drawUI') and in a pane below the stats footer once one is (see
    -- 'UI.Session.draw'). Updated via 'logOp'.
    _opLog :: OpLog.State
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

initialState :: (Text.Text -> [String] -> IO ()) -> IO () -> IO () -> IO () -> IO () -> State
initialState onStartServer onKill onRestart onClean onShutdown =
  State
    { _sessions = SessionSelector.initialState
    , _options = newForm optionFields defaultOptions
    , _serveForm = newForm serveFields defaultServeInput
    , _currentFocus = ServeOptions
    , _currentTime = UTCTime (fromGregorian 1970 1 1) 0
    , _startServer = onStartServer
    , _killServer = onKill
    , _restartServer = onRestart
    , _cleanServer = onClean
    , _shutdown = onShutdown
    , _opLog = OpLog.addEntry (Text.pack "Waiting for first session") OpLog.initialState
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
      -- The start-server form is already embedded directly in the idle\/start screen (see 'idleLayers'); this
      -- overlay only applies when a session is connected, so 'S' still has a visible effect in that case.
      ServeOptions -> maybe [] (const [drawServeOverlay _serveForm]) session
      TaskDetails -> let task = session >>= listSelectedElement . Session._activeTasks in maybe [] (pure . ActiveTasks.drawTaskDetails . snd) task
      LogViewer -> maybe [] (pure . drawLogViewer . Session._logViewer) session
      _ -> []
  )
    -- On the idle\/start screen, the "GHC" banner, the Haskell-logo banner, the operational log and the
    -- start-server form are rendered as independent layers (see 'drawGhcArt', 'drawHaskellArt', 'drawIdleLog',
    -- 'drawIdleStartServer') instead of being stacked in a single 'vBox', so that the log and the form can each
    -- be positioned relative to the *whole* main view rather than relative to whatever space the banners happen
    -- to leave behind.
    ++ maybe [drawGhcArt, drawHaskellArt, drawIdleLog _opLog, drawIdleStartServer _serveForm] (const []) session
    ++ [ vBox
          [ joinBorders $
              withBorderStyle unicodeRounded $
                maybe (fill ' ') (Session.draw _currentFocus _currentTime _opLog) session
          , hBorder
          , modifyDefAttr (`V.withStyle` V.italic) $
              str
          " q:quit   Tab:switch focus   Enter:expand/details   b:build   m:metadata   x:execute   r:trigger rebuild   d:debug   o:options   s:sessions   S:start server   K:kill server   R:restart server   C:clean cache   t:sort bytecode   e:evict bytecode   L:log"
          ]
       ]
 where
  session = snd . snd <$> listSelectedElement _sessions

-- | Renders a project path as it should appear in an operational-log message, substituting a description for an
-- empty path (meaning "use the current directory").
describePath :: Text.Text -> String
describePath path
  | Text.null path = "the current directory"
  | otherwise = Text.unpack path

drawOptionsEditor :: Form Options Event Name -> Widget Name
drawOptionsEditor form = popup 50 "Session Options" $ renderForm form

-- | Block-letter rendering of "GHC", 5 rows high, drawn in the top-left corner of the idle\/start screen (see
-- 'drawGhcArt') with a two-cell margin from the top and left edges.
ghcArt :: [String]
ghcArt =
  [ "█████ █   █ █████"
  , "█     █   █ █    "
  , "█  ██ █████ █    "
  , "█   █ █   █ █    "
  , "█████ █   █ █████"
  ]

-- | Position a widget as a transparent, non-space-filling layer -- like 'Brick.Widgets.Center.vCenterLayer',
-- which this generalizes (a fraction of 0.5 reproduces it exactly) -- so that its vertical center sits at the
-- given fraction of the available height, measured from the top of the rendering context. Only usable as a
-- top-level layer (see 'Brick.Main.App' 'appDraw'\/'drawUI'): unlike 'vCenterLayer' it isn't meant to be nested
-- inside another layout, since its positioning is computed against whatever the ambient context's available
-- height happens to be at the point it renders.
vAnchorLayer :: Double -> Widget n -> Widget n
vAnchorLayer frac p =
  Widget (hSize p) Greedy $ do
    result <- render p
    ctx <- getContext
    let rHeight = V.imageHeight (image result)
        targetCenter = round (frac * fromIntegral (availHeight ctx))
        topOffset = max 0 (targetCenter - rHeight `div` 2)
    if topOffset == 0
      then pure result
      else
        pure $
          addResultOffset (Location (0, topOffset)) $
            result {image = V.translate 0 topOffset (image result)}

-- | The "GHC" banner layer, anchored to the top-left corner of the whole screen (see 'ghcArt'). A plain
-- 'Fixed'-sized widget doesn't fill its context, so as a layer it leaves the rest of the idle screen -- and
-- whatever other layers sit underneath it -- untouched.
drawGhcArt :: Widget Name
drawGhcArt = padTop (Pad 2) $ padLeft (Pad 2) $ vBox (str <$> ghcArt)

-- | A contiguous run of identical cells in one row of 'haskellArt': a repeated glyph character, optionally
-- colored via 'haskellLogoArrowAttr'\/'haskellLogoLambdaAttr'\/'haskellLogoEqualsAttr'. 'Nothing' renders in
-- the ambient default attribute, used for the blank cells surrounding and separating the glyph's strokes.
data ArtCell = ArtCell
  { artCount :: Int
  , artChar :: Char
  , artColor :: Maybe AttrName
  }

haskellLogo :: [[ArtCell]]
haskellLogo =
  [
    line1 0,
    line1 1,
    line1 2,
    line1 3,
    line2 4 10,
    line2 5 9,
    line [
      (6, segment 5 chevron otri itri),
      (1, segment 5 lambda otri ur)
    ],
    line3 5 7 7,
    line3 4 9 6,
    line [
      (3, segment 5 chevron ul lr),
      (1, segment 5 lambda ul utri),
      (0, segment 4 lambda full ur)
    ],
    line4 2 1,
    line4 1 3,
    line4 0 5
  ]
 where
  chevron = haskellLogoArrowAttr
  lambda = haskellLogoLambdaAttr
  equals = haskellLogoEqualsAttr

  line1 pre =
    line [
      (pre, chevron2),
      (1, lambda2)
    ]

  line2 pre eqWidth =
    line [
      (pre, chevron2),
      (1, segment 5 lambda ll ur),
      (1, segment eqWidth equals ll full)
    ]

  line3 pre lamWidth eqWidth =
    line [
      (pre, segment 5 chevron ul lr),
      (1, segment lamWidth lambda ul ur),
      (1, segment eqWidth equals ll full)
    ]

  line4 pre gap =
    line [
      (pre, chevron1),
      (1, lambda1),
      (gap, lambda2)
    ]

  chevron1 = segment 5 chevron ul lr

  chevron2 = segment 5 chevron ll ur

  lambda1 = segment 5 lambda ul lr

  lambda2 = segment 5 lambda ll ur

  line cells =
    mconcat [ws w ++ c | (w, c) <- cells]

  ws artCount =
    [
      ArtCell {
        artCount,
        artChar = ' ',
        artColor = Nothing
      }
    ]

  segment width color l r =
    [
      ArtCell 1 l (Just color),
      ArtCell width full (Just color),
      ArtCell 1 r (Just color)
    ]

  ul = '◢'
  ur = '◣'
  ll = '◥'
  lr = '◤'
  full = '█'
  itri = '🭬'
  otri = '🭨'
  utri = '🭫'

renderArtCell :: ArtCell -> Widget Name
renderArtCell (ArtCell n c color) = maybe id withAttr color (str (replicate n c))

-- | The Haskell-logo banner layer (see 'haskellArt'), anchored to the top-right corner of the whole screen
-- with a two-cell margin from both edges, mirroring 'drawGhcArt' on the opposite side.
drawHaskellArt :: Widget Name
drawHaskellArt = hAnchorRightLayer 2 $ padTop (Pad 2) $ vBox (hBox . fmap renderArtCell <$> haskellLogo)

-- | Position a widget as a transparent, non-space-filling layer -- the horizontal, right-anchored analogue of
-- 'vAnchorLayer' (and of 'Brick.Widgets.Center.hCenterLayer', which this would reproduce if the margin were
-- chosen to center rather than right-align) -- so that its right edge sits the given number of columns from
-- the right edge of the whole screen. Only usable as a top-level layer, for the same reason as 'vAnchorLayer'.
hAnchorRightLayer :: Int -> Widget n -> Widget n
hAnchorRightLayer marginRight p =
  Widget Greedy (vSize p) $ do
    result <- render p
    ctx <- getContext
    let rWidth = V.imageWidth (image result)
        leftOffset = max 0 (availWidth ctx - rWidth - marginRight)
    if leftOffset == 0
      then pure result
      else
        pure $
          addResultOffset (Location (leftOffset, 0)) $
            result {image = V.translate leftOffset 0 (image result)}

-- | The operational message log layer, centered relative to the whole main view (i.e. the entire screen above
-- the footer legend, ignoring how much space the "GHC" banner and the start-server form separately occupy --
-- see 'drawGhcArt', 'drawIdleStartServer').
drawIdleLog :: OpLog.State -> Widget Name
drawIdleLog opLogState = centerLayer (hLimitPercent 50 (OpLog.draw 5 opLogState))

-- | The "start ghc-server" form: a "Start server" label (blue) above the project-path\/extra-options input
-- fields. Unbordered; callers are responsible for placement (see 'drawIdleStartServer', 'drawServeOverlay').
drawStartServer :: Form ServeInput Event Name -> Widget Name
drawStartServer form =
  hLimitPercent 50 $
    vBox
      [ withAttr startServerLabelAttr (str "Start server")
      , str " "
      , renderForm form
      , str " "
      , str "Leaving the path empty starts ghc-server in the current directory."
      ]

-- | The start-server form layer, positioned so that it sits a quarter of the main view's height above the
-- footer legend (i.e. its vertical center is at 3\/4 of the main view's height, measured from the top).
drawIdleStartServer :: Form ServeInput Event Name -> Widget Name
drawIdleStartServer form = hCenterLayer (vAnchorLayer 0.75 (drawStartServer form))

-- | Fallback rendering of the "start ghc-server" form as a borderless overlay, used when the 'S' key is pressed
-- while a session is already connected (so 'drawIdleStartServer', which normally hosts the form, is not on
-- screen).
drawServeOverlay :: Form ServeInput Event Name -> Widget Name
drawServeOverlay form = centerLayer (drawStartServer form)

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

-- | Records a high-visibility operational message (see 'UI.OpLog'): appended to the top-level operational log
-- (always) and, when a session is currently connected, mirrored into that session's regular server-log viewer
-- (tagged @"operational"@) as well, so it remains visible via the 'L' key too.
logOp :: Text.Text -> EventM Name State ()
logOp message = do
  opLog %= OpLog.addEntry message
  ms <- liftIO $ round . (* 1000) <$> getPOSIXTime
  currentSession . Session.logViewer %=
    LogViewer.addEntry LogViewer.Entry {target = Text.pack "operational", level = Text.pack "info", message, timestampMs = ms}

-- | Write every log entry captured so far for the current session (server-forwarded scheduler\/build
-- diagnostics as well as client\/operational entries, see 'UI.LogViewer.Entry') to a hardcoded @ui.log@ file in
-- the current working directory, oldest first. Triggered by the 'W' key, primarily to capture the full
-- scheduler decision trace (@GhcServer.Handler@ forwards every 'GhcServer.Scheduler.SchedulerDecision' as a
-- @\"scheduler\"@-tagged debug entry) for offline bug reports.
writeLogToFile :: EventM Name State ()
writeLogToFile = do
  mentries <- preuse (currentSession . Session.logViewer)
  case mentries of
    Nothing -> logOp (Text.pack "W key: no session connected, nothing to write")
    Just lv -> do
      let rendered = LogViewer.formatEntry <$> LogViewer.visibleEntries LogViewer.noFilter lv.rawEntries
      liftIO $ writeFile "ui.log" (unlines rendered)
      logOp (Text.pack ("Wrote " ++ show (length rendered) ++ " log entries to ui.log"))

beep :: EventM Name State ()
beep = do
  vty <- getVtyHandle
  liftIO $ V.ringTerminalBell $ V.outputIface vty

-- | Backs the quit keys ('q'\/Esc). Per the task's requirement, switches back to the view shown on start --
-- clearing the session list, which makes 'drawUI' fall back to the operational-log placeholder -- \'before
-- anything else\', then logs and kicks off the background shutdown sequence (see the '_shutdown' field\/Main's
-- @requestShutdown@) rather than halting immediately: that sequence dispatches 'OpLogMessage'\/'ShutdownComplete'
-- events while this app's event loop is still running, so its progress remains visible (and, if it hangs,
-- diagnosable) instead of freezing on whatever was on screen when 'q' was pressed.
requestQuit :: EventM Name State ()
requestQuit = do
  sessions .= SessionSelector.initialState
  logOp (Text.pack "Shutting down")
  action <- use shutdown
  liftIO action

-- | Runs 'handler' against the currently selected active-task target, or beeps if none is selected. Used by the
-- 'd' (debug) key handler.
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
handleEvent (AppEvent (TriggerBuild wid target rebuild)) = do
  mworker <- preuse (currentSession . Session.workers . each . filtered (\w -> Session._workerId w == wid))
  for_ mworker $ \worker -> do
    liftIO $ triggerRebuildText (Session._connection worker) target rebuild
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
handleEvent (AppEvent (SessionSelectorEvent evt)) = do
  zoom sessions (SessionSelector.handleEvent evt)
  -- The first session to appear is auto-selected (see 'SessionSelector.handleEvent's 'StartSession' case)
  -- without the user dismissing any modal, so the idle screen's initial focus (the start-server form) has to be
  -- moved off explicitly here once that happens, mirroring what the other modals' "hide" logic does on Esc\/Enter.
  current <- use currentFocus
  when (current == ServeOptions) $ currentFocus .= TaskTree
handleEvent (AppEvent (RequestServe path opts)) = do
  action <- use startServer
  liftIO $ action path (words (Text.unpack opts))
handleEvent (AppEvent (ProcessLog stream line)) =
  logClient (Text.pack "info") (stream <> Text.pack ": " <> line)
handleEvent (AppEvent (ServerStarting path)) =
  logOp (Text.pack "Starting ghc-server in " <> Text.pack (describePath path))
handleEvent (AppEvent (ServerFailed path stderrText)) =
  logOp (Text.pack "Failed to start ghc-server in " <> Text.pack (describePath path) <> Text.pack ": " <> stderrText)
handleEvent (AppEvent (OpLogMessage message)) = logOp message
handleEvent (AppEvent ShutdownComplete) = halt
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
      -- Esc\/Enter always fall back to 'TaskTree', not back into the form (even though the form is the initial
      -- focus on the idle screen -- see 'initialState'): otherwise there would be no way to reach any other
      -- keybinding (including quit) while no session is connected, since every keystroke would keep being routed
      -- right back into the form.
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
        _ -> zoom (currentSession . Session.logViewer) (LogViewer.handleEvent evt)
    _ -> case evt of
      V.EvKey V.KEsc [] -> requestQuit
      V.EvKey (V.KChar 'q') [] -> requestQuit
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
      V.EvKey (V.KChar 'W') [] -> writeLogToFile
      V.EvKey (V.KChar 'd') [] -> do
        withTarget $ \_wid target ->
          suspendAndResume' $
            debug (debugSocketPath target)
      V.EvKey (V.KChar 'r') [] -> do
        mtree <- preuse (currentSession . Session.taskTree)
        mfirst <- firstWorker
        case (mtree >>= TaskTree.selectedCompileTargets, mfirst) of
          (Just targets, Just (wid, _)) -> for_ targets \target -> handleEvent (AppEvent (TriggerBuild wid target True))
          _ -> beep
      V.EvKey (V.KChar 'b') [] -> do
        mtree <- preuse (currentSession . Session.taskTree)
        mfirst <- firstWorker
        case (mtree >>= TaskTree.selectedCompileTargets, mfirst) of
          (Just targets, Just (wid, _)) -> for_ targets \target -> handleEvent (AppEvent (TriggerBuild wid target False))
          _ -> beep
      V.EvKey (V.KChar 'm') [] -> do
        mtree <- preuse (currentSession . Session.taskTree)
        mfirst <- firstWorker
        case (mtree >>= TaskTree.selectedMetadataTargets, mfirst) of
          (Just targets, Just (wid, _)) -> for_ targets \target -> handleEvent (AppEvent (TriggerBuild wid target False))
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
            , (opLogIndicatorAttr, V.withStyle (V.defAttr `V.withForeColor` green) V.bold)
            , (opLogTextAttr, V.defAttr `V.withForeColor` brightWhite)
            , (startServerLabelAttr, V.defAttr `V.withForeColor` blue)
            , (haskellLogoArrowAttr, V.defAttr `V.withForeColor` RGBColor 0x45 0x3a 0x62)
            , (haskellLogoLambdaAttr, V.defAttr `V.withForeColor` RGBColor 0x5e 0x50 0x86)
            , (haskellLogoEqualsAttr, V.defAttr `V.withForeColor` RGBColor 0x8f 0x4e 0x8b)
            ]
    , appChooseCursor = showFirstCursor
    }
