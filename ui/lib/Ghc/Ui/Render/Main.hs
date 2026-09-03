module Ghc.Ui.Render.Main where

import Brick (Widget (..), fill, hLimitPercent, joinBorders, modifyDefAttr, txt, vBox, withAttr, withBorderStyle)
import Brick.Forms (Form, renderForm)
import Brick.Widgets.Border (hBorder)
import Brick.Widgets.Border.Style (unicodeRounded)
import Brick.Widgets.Center (centerLayer, hCenterLayer)
import Brick.Widgets.List (listSelectedElement)
import Ghc.Ui.Attr qualified as Attr
import Ghc.Ui.Data.Main (MainEvent, MainState (..))
import Ghc.Ui.Data.Name (Name (..))
import Ghc.Ui.Data.ServerProcess (ServerConfig)
import Ghc.Ui.Data.Session qualified as Session
import Ghc.Ui.Data.Session (SessionState)
import Ghc.Ui.Data.Tasks (rowTask)
import Ghc.Ui.Render.Layer (vAnchorLayer)
import Ghc.Ui.Render.Log (renderLogPopup)
import Ghc.Ui.Render.Logo (renderLogo)
import Ghc.Ui.Render.OpLog (renderOpLogIdle, renderOpLogPopup)
import Ghc.Ui.Render.Session (renderSession)
import Ghc.Ui.Render.Sessions (renderSessions)
import Ghc.Ui.Render.Tasks qualified as Tasks
import Graphics.Vty (italic, withStyle)

-- | The "start ghc-server" form: a "Start server" label (blue) above the project-path\/extra-options input
-- fields. Unbordered; callers are responsible for placement (see 'drawIdleStartServer', 'drawServeOverlay').
renderStartServer :: Form ServerConfig MainEvent Name -> Widget Name
renderStartServer form =
  hLimitPercent 50 $ vBox [
    withAttr Attr.startServerLabel (txt "Start server"),
    txt " ",
    renderForm form,
    txt " ",
    txt "Leaving the path empty uses the current directory."
  ]

-- | The start-server form layer, positioned so that it sits a quarter of the main view's height above the
-- footer legend (i.e. its vertical center is at 3\/4 of the main view's height, measured from the top).
renderStartServerIdle :: Form ServerConfig MainEvent Name -> Widget Name
renderStartServerIdle form = hCenterLayer (vAnchorLayer 0.75 (renderStartServer form))

-- | Fallback rendering of the "start ghc-server" form as a borderless overlay, used when the 'S' key is pressed
-- while a session is already connected (so 'drawIdleStartServer', which normally hosts the form, is not on
-- screen).
renderStartServerOverlay :: Form ServerConfig MainEvent Name -> Widget Name
renderStartServerOverlay form = centerLayer (renderStartServer form)

-- TODO is the border stuff a no-op that was forgotten to be removed when we disabled borders?
-- Or is it pointless because there are no borders around the entire UI?
renderInfo :: MainState -> Maybe SessionState -> Widget Name
renderInfo MainState {currentFocus, currentTime, opLog} session =
  vBox [
    joinBorders $ withBorderStyle unicodeRounded $ maybe (fill ' ') (renderSession currentFocus currentTime opLog) session,
    hBorder,
    modifyDefAttr (`withStyle` italic) $ txt " q:quit   Tab:switch focus   Enter:expand/details   p:phases   b:build   m:metadata   x:execute   r:trigger rebuild   d:debug   o:options   s:sessions   S:start server   K:kill server   R:restart server   c:clean cache   e:evict bytecode   L:log"
  ]

renderPopup :: MainState -> Maybe SessionState -> Name -> [Widget Name]
renderPopup MainState {sessions, serverForm, opLog} session = \case
  Sessions -> [renderSessions sessions]
  StartServer -> maybe [] (const [renderStartServerOverlay serverForm]) session
  TaskDetails -> let mrow = session >>= listSelectedElement . (.tasks) in maybe [] (pure . Tasks.renderTaskDetails) (mrow >>= rowTask . snd)
  Log -> maybe [] (pure . renderLogPopup . (.log)) session
  OpLogDebug -> pure (renderOpLogPopup opLog)
  _ -> []

renderMain :: MainState -> [Widget Name]
renderMain state@MainState {sessions, currentFocus, serverForm, opLog} =
  renderPopup state session currentFocus
  ++
  maybe [renderOpLogIdle opLog, renderStartServerIdle serverForm] (const []) session
  ++
  [renderLogo, renderInfo state session]
 where
  session = snd . snd <$> listSelectedElement sessions
