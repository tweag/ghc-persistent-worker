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
import Brick.Widgets.Core (joinBorders, modifyDefAttr, str, vBox, withBorderStyle, (<+>))
import Brick.Widgets.Edit (editFocusedAttr)
import Brick.Widgets.List (listSelectedAttr, listSelectedElement, listSelectedElementL, listSelectedFocusedAttr)
import Control.Exception (handle)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (for_)
import Data.Monoid (First (..))
import Data.Text qualified as Text
import Data.Time (UTCTime (..), fromGregorian)
import Graphics.Vty qualified as V
import Graphics.Vty.Attributes.Color
import Grpc (evictBytecode, getBytecodeState, sendOptions, triggerExecuteText, triggerRebuild, triggerRebuildText)
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
  (.=),
  (^.),
  )
import Network.GRPC.Client (Connection)
import Network.GRPC.Common.Protobuf (Proto)
import Proto.Instrument qualified as Instr
import Proto.Instrument_Fields qualified as InstrF
import Types.State (Options (..), defaultOptions)
import Types.Target (TargetSpec)
import UI.ActiveTasks qualified as ActiveTasks
import UI.BytecodeBrowser qualified as BytecodeBrowser
import UI.GhcDebug (debug)
import UI.Session qualified as Session
import UI.SessionSelector qualified as SessionSelector
import UI.TaskTree qualified as TaskTree
import UI.Types (Name (..), WorkerId, canDebugAttr, disabledAttr, evictedAttr, pendingEvictionAttr, taskFailedAttr, taskRunningAttr, taskSucceededAttr)
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
    -- the project view, as computed by 'TaskTree.selectedUnitForExecute'. The target text is a bare unit name.
    TriggerExecute WorkerId Text.Text
  | BytecodeBrowserEvent BytecodeBrowser.Event
  | FetchBytecodeState
  | RequestEvict Text.Text (Maybe Text.Text)
  | -- | Start (or connect to) a @ghc-server@ instance, as requested via the capital-@S@ key binding's popup.
    -- Carries the user-entered project path (empty means "use the current directory") and extra CLI options to
    -- pass to @ghc-server@.
    RequestServe Text.Text Text.Text

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
  , _bcoBrowser :: BytecodeBrowser.State
  , -- | Starts (or connects to) a @ghc-server@ instance for the given project path (empty for the current
    -- directory) and extra options, dispatched from the capital-@S@ popup. Supplied by @Main@, which owns the
    -- event channel and the socket-listening logic.
    _startServer :: Text.Text -> [String] -> IO ()
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

initialState :: (Text.Text -> [String] -> IO ()) -> State
initialState onStartServer =
  State
    { _sessions = SessionSelector.initialState
    , _options = newForm optionFields defaultOptions
    , _serveForm = newForm serveFields defaultServeInput
    , _currentFocus = TaskTree
    , _currentTime = UTCTime (fromGregorian 1970 1 1) 0
    , _bcoBrowser = BytecodeBrowser.initialState
    , _startServer = onStartServer
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
      _ -> []
  )
    ++ [ vBox $
          [ joinBorders $
              withBorderStyle unicodeRounded $
                maybe
                  (borderWithLabel (str " GHC Persistent Worker ") $ center $ str "Waiting for first session")
                  (Session.draw _currentFocus _currentTime _bcoBrowser)
                  session
          , modifyDefAttr (`V.withStyle` V.italic) $
              str
          " q:quit   Tab:switch pane   Enter:expand/details   b:build   m:metadata   x:execute   r:trigger rebuild   d:debug   o:options   s:sessions   S:start server   t:sort bytecode   e:evict bytecode"
          ]
       ]
 where
  session = snd . snd <$> listSelectedElement _sessions

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

currentSession :: Traversal' State Session.State
currentSession = sessions . listSelectedElementL . _2

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

toEntries :: Proto Instr.BytecodeState -> [BytecodeBrowser.Entry]
toEntries resp =
  [ BytecodeBrowser.Entry
      (e ^. InstrF.unitId)
      (e ^. InstrF.moduleName)
      (fromIntegral (e ^. InstrF.size))
      (fromIntegral (e ^. InstrF.lastAccess))
      (e ^. InstrF.resident)
      (e ^. InstrF.pendingEviction)
  | e <- resp ^. InstrF.entries
  ]

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
  for_ mworker $ \worker -> do
    liftIO $ triggerExecuteText (Session._connection worker) target
handleEvent (AppEvent (BytecodeBrowserEvent evt)) =
  zoom bcoBrowser (BytecodeBrowser.handleEvent evt)
handleEvent (AppEvent FetchBytecodeState) = do
  mconn <- bcoConnection
  case mconn of
    Nothing -> beep
    Just conn -> do
      resp <- liftIO $ getBytecodeState conn
      zoom bcoBrowser (BytecodeBrowser.handleEvent (BytecodeBrowser.Load (toEntries resp)))
handleEvent (AppEvent (RequestEvict unitId modName)) = do
  mconn <- bcoConnection
  for_ mconn $ \conn -> liftIO $ evictBytecode conn unitId (maybe Text.empty id modName)
  zoom bcoBrowser (BytecodeBrowser.handleEvent (BytecodeBrowser.Evicted unitId modName))
handleEvent (AppEvent (SessionSelectorEvent evt)) =
  zoom sessions (SessionSelector.handleEvent evt)
handleEvent (AppEvent (RequestServe path opts)) = do
  action <- use startServer
  liftIO $ action path (words (Text.unpack opts))
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
    _ -> case evt of
      V.EvKey V.KEsc [] -> halt
      V.EvKey (V.KChar 'q') [] -> halt
      V.EvKey (V.KChar 's') [] -> do
        currentFocus .= SessionSelector
      V.EvKey (V.KChar 'o') [] -> do
        currentFocus .= OptionsEditor
      V.EvKey (V.KChar 'S') [] -> do
        currentFocus .= ServeOptions
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
        case (mtree >>= TaskTree.selectedMetadataTarget, mfirst) of
          (Just target, Just (wid, _)) -> handleEvent (AppEvent (TriggerBuild wid target))
          _ -> beep
      V.EvKey (V.KChar 'x') [] -> do
        mtree <- preuse (currentSession . Session.taskTree)
        mfirst <- firstWorker
        case (mtree >>= TaskTree.selectedUnitForExecute, mfirst) of
          (Just target, Just (wid, _)) -> handleEvent (AppEvent (TriggerExecute wid target))
          _ -> beep
      V.EvKey (V.KChar 't') [] | current == BytecodeBrowser ->
        handleEvent (AppEvent (BytecodeBrowserEvent BytecodeBrowser.ToggleSort))
      V.EvKey (V.KChar 'e') [] | current == BytecodeBrowser -> do
        bco <- use bcoBrowser
        case BytecodeBrowser.selectedTarget bco of
          Nothing -> beep
          Just (unitId, modName) -> handleEvent (AppEvent (RequestEvict unitId modName))
      V.EvKey (V.KChar '\t') [] -> do
        let next = case current of
              ActiveTasks -> TaskTree
              TaskTree -> BytecodeBrowser
              BytecodeBrowser -> ActiveTasks
              _ -> current
        currentFocus .= next
        when (next == BytecodeBrowser) (handleEvent (AppEvent FetchBytecodeState))
      V.EvKey V.KEnter [] -> case current of
        ActiveTasks ->
          withTarget \_ _ -> currentFocus .= TaskDetails
        TaskTree ->
          zoom (currentSession . Session.taskTree) (TaskTree.handleEvent TaskTree.ToggleExpand)
        BytecodeBrowser ->
          zoom bcoBrowser (BytecodeBrowser.handleEvent BytecodeBrowser.ToggleExpand)
        _ -> pure ()
      _ -> case current of
        ActiveTasks -> handleListEventOf (currentSession . Session.activeTasks) evt
        TaskTree -> handleListEventOf (currentSession . Session.taskTree . TaskTree.rowsLens) evt
        BytecodeBrowser -> handleListEventOf (bcoBrowser . BytecodeBrowser.rowsLens) evt
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
