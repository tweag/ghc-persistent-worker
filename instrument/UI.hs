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
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (for_)
import Data.Monoid (First (..))
import Data.Text qualified as Text
import Data.Time (UTCTime (..), fromGregorian)
import Graphics.Vty qualified as V
import Graphics.Vty.Attributes.Color
import Grpc (evictBytecode, getBytecodeState, sendOptions, triggerRebuild)
import Internal.Debug (debugSocketPath)
import Network.GRPC.Client (Connection)
import Lens.Micro.Platform (Lens', Traversal', each, filtered, lens, makeLenses, packed, preuse, use, zoom, (.=), (^.), _2)
import Network.GRPC.Common.Protobuf (Proto)
import Proto.Instrument qualified as Instr
import Proto.Instrument_Fields qualified as InstrF
import Types.State (Options (..), defaultOptions)
import Types.Target (TargetSpec)
import UI.ActiveTasks qualified as ActiveTasks
import UI.BytecodeBrowser qualified as BytecodeBrowser
import UI.GhcDebug (debug)
import UI.ModuleSelector qualified as ModuleSelector
import UI.Session qualified as Session
import UI.SessionSelector qualified as SessionSelector
import UI.Types (Name (..), WorkerId, canDebugAttr, disabledAttr)
import UI.Utils (handleListEventOf, popup)

data Event
  = SendOptions (Maybe WorkerId)
  | SetTime UTCTime
  | SessionSelectorEvent SessionSelector.Event
  | TriggerRebuild WorkerId TargetSpec
  | BytecodeBrowserEvent BytecodeBrowser.Event
  | FetchBytecodeState
  | RequestEvict Text.Text (Maybe Text.Text)

data State = State
  { _sessions :: SessionSelector.State
  , _options :: Form Options Event Name
  , _currentFocus :: Name
  , _currentTime :: UTCTime
  , _bcoBrowser :: BytecodeBrowser.State
  }

makeLenses ''State

ghcOptionsLens :: Lens' Options Text.Text
ghcOptionsLens =
  lens
    (.extraGhcOptions)
    (\opts s -> opts{extraGhcOptions = s})
    . packed

initialState :: State
initialState =
  State
    { _sessions = SessionSelector.initialState
    , _options = newForm optionFields defaultOptions
    , _currentFocus = ModuleSelector
    , _currentTime = UTCTime (fromGregorian 1970 1 1) 0
    , _bcoBrowser = BytecodeBrowser.initialState
    }

optionFields :: [Options -> FormFieldState Options Event Name]
optionFields =
  [ (str "Extra GHC Options: " <+>) @@= editTextField ghcOptionsLens OEExtraGhcOptions (Just 1)
  ]

drawUI :: State -> [Widget Name]
drawUI State{..} =
  ( case _currentFocus of
      SessionSelector -> [SessionSelector.draw _sessions]
      OptionsEditor -> [drawOptionsEditor _options]
      BytecodeBrowser -> [BytecodeBrowser.draw _bcoBrowser]
      TaskDetails -> let task = session >>= listSelectedElement . Session._activeTasks in maybe [] (pure . ActiveTasks.drawTaskDetails . snd) task
      ModuleDetails -> let mdl = session >>= listSelectedElement . Session._modules in maybe [] (pure . ModuleSelector.drawModuleDetails . snd) mdl
      _ -> []
  )
    ++ [ vBox $
          [ joinBorders $
              withBorderStyle unicodeRounded $
                maybe
                  (borderWithLabel (str " GHC Persistent Worker ") $ center $ str "Waiting for first session")
                  (Session.draw _currentFocus _currentTime)
                  session
          , modifyDefAttr (`V.withStyle` V.italic) $ str " q:quit   Enter:show details   r:trigger rebuild   d:debug   o:toggle options editor   s:toggle session selector   b:bytecode cache"
          ]
       ]
 where
  session = snd . snd <$> listSelectedElement _sessions

drawOptionsEditor :: Form Options Event Name -> Widget Name
drawOptionsEditor form = popup 50 "Session Options" $ renderForm form

currentSession :: Traversal' State Session.State
currentSession = sessions . listSelectedElementL . _2

beep :: EventM Name State ()
beep = do
  vty <- getVtyHandle
  liftIO $ V.ringTerminalBell $ V.outputIface vty

withTarget' :: Bool -> (WorkerId -> TargetSpec -> EventM Name State ()) -> EventM Name State ()
withTarget' forRebuild handler = do
  current <- use currentFocus
  First mtarget <- case current of
    ActiveTasks -> zoom (currentSession . Session.activeTasks) (First <$> ActiveTasks.getSelectedTarget)
    ModuleSelector -> zoom (currentSession . Session.modules) (First <$> ModuleSelector.getSelectedTarget forRebuild)
    _ -> pure (First Nothing)
  case mtarget of
    Nothing -> beep
    Just (wid, target) -> handler wid target

withTarget :: (WorkerId -> TargetSpec -> EventM Name State ()) -> EventM Name State ()
withTarget = withTarget' False

withTargetForRebuild :: (WorkerId -> TargetSpec -> EventM Name State ()) -> EventM Name State ()
withTargetForRebuild = withTarget' True

toEntries :: Proto Instr.BytecodeState -> [BytecodeBrowser.Entry]
toEntries resp =
  [ BytecodeBrowser.Entry (e ^. InstrF.unitId) (e ^. InstrF.moduleName) (fromIntegral (e ^. InstrF.size)) (fromIntegral (e ^. InstrF.lastAccess))
  | e <- resp ^. InstrF.entries
  ]

-- | The connection used to query/evict bytecode cache state: the first worker of the currently focused session.
bcoConnection :: EventM Name State (Maybe Connection)
bcoConnection = do
  workers <- use (currentSession . Session.workers)
  pure $ case workers of
    (w : _) -> Just w._connection
    [] -> Nothing

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
handleEvent (VtyEvent evt) = do
  current <- use currentFocus
  case current of
    SessionSelector -> do
      let hide = currentFocus .= ModuleSelector
      case evt of
        V.EvKey V.KEsc [] -> hide
        V.EvKey V.KEnter [] -> hide
        V.EvKey (V.KChar 's') [] -> hide
        _ -> handleListEventOf sessions evt
    OptionsEditor -> do
      let hide = do
            currentFocus .= ModuleSelector
            handleEvent (AppEvent (SendOptions Nothing))
      case evt of
        V.EvKey V.KEsc [] -> hide
        V.EvKey V.KEnter [] -> hide
        _ -> zoom options (handleFormEvent (VtyEvent evt))
    BytecodeBrowser -> do
      let hide = currentFocus .= ModuleSelector
      case evt of
        V.EvKey V.KEsc [] -> hide
        V.EvKey V.KEnter [] -> hide
        V.EvKey (V.KChar 'b') [] -> hide
        V.EvKey (V.KChar 't') [] -> handleEvent (AppEvent (BytecodeBrowserEvent BytecodeBrowser.ToggleSort))
        V.EvKey (V.KChar 'x') [] -> do
          bco <- use bcoBrowser
          case BytecodeBrowser.selectedTarget bco of
            Nothing -> beep
            Just (unitId, modName) -> handleEvent (AppEvent (RequestEvict unitId modName))
        _ -> handleListEventOf (bcoBrowser . BytecodeBrowser.rowsLens) evt
    TaskDetails -> do
      let hide = currentFocus .= ActiveTasks
      case evt of
        V.EvKey V.KEsc [] -> hide
        V.EvKey V.KEnter [] -> hide
        _ -> handleListEventOf (currentSession . Session.activeTasks) evt
    ModuleDetails -> do
      let hide = currentFocus .= ModuleSelector
      case evt of
        V.EvKey V.KEsc [] -> hide
        V.EvKey V.KEnter [] -> hide
        _ -> handleListEventOf (currentSession . Session.modules) evt
    _ -> case evt of
      V.EvKey V.KEsc [] -> halt
      V.EvKey (V.KChar 'q') [] -> halt
      V.EvKey (V.KChar 's') [] -> do
        currentFocus .= SessionSelector
      V.EvKey (V.KChar 'o') [] -> do
        currentFocus .= OptionsEditor
      V.EvKey (V.KChar 'b') [] -> do
        currentFocus .= BytecodeBrowser
        handleEvent (AppEvent FetchBytecodeState)
      V.EvKey (V.KChar 'd') [] -> do
        withTarget $ \_wid target ->
          suspendAndResume' $
            debug (debugSocketPath target)
      V.EvKey (V.KChar 'r') [] -> do
        withTargetForRebuild $ \wid target ->
          handleEvent (AppEvent (TriggerRebuild wid target))
      V.EvKey (V.KChar '\t') [] -> do
        currentFocus .= case current of
          ActiveTasks -> ModuleSelector
          ModuleSelector -> ActiveTasks
          _ -> current
      V.EvKey V.KEnter [] -> do
        withTarget \_ _ ->
          currentFocus .= case current of
            ActiveTasks -> TaskDetails
            ModuleSelector -> ModuleDetails
            _ -> current
      _ -> case current of
        ActiveTasks -> handleListEventOf (currentSession . Session.activeTasks) evt
        ModuleSelector -> handleListEventOf (currentSession . Session.modules) evt
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
            ]
    , appChooseCursor = showFirstCursor
    }
