module Ghc.Ui.App where

import Brick (App (..), AttrMap, attrMap, on, showFirstCursor)
import Brick.Widgets.Edit (editFocusedAttr)
import Brick.Widgets.List (listSelectedAttr, listSelectedFocusedAttr)
import Control.Monad.Reader (runReaderT)
import Ghc.Ui.Attr qualified as Attr
import Ghc.Ui.Data.Main (MainEvent, MainState)
import Ghc.Ui.Data.Name (Name (..))
import Ghc.Ui.Data.ServerHandlers (ServerHandlers)
import Ghc.Ui.Event.Main (handleUiEvent, initEvent)
import Ghc.Ui.Render.Main (renderMain)
import Graphics.Vty (
  Color (..),
  black,
  blue,
  bold,
  brightBlack,
  brightWhite,
  brightYellow,
  cyan,
  defAttr,
  dim,
  green,
  italic,
  magenta,
  red,
  withBackColor,
  withForeColor,
  withStyle,
  yellow,
  )

attrMapMain :: a -> AttrMap
attrMapMain _ =
  attrMap defAttr [
    (editFocusedAttr, brightWhite `on` blue),
    (listSelectedAttr, withBackColor defAttr black),
    (listSelectedFocusedAttr, brightWhite `on` brightBlack),
    (Attr.disabled, style dim),
    (Attr.debuggable, bold' defAttr),
    (Attr.evicted, italic' (style dim)),
    (Attr.pendingEviction, italic' (style dim)),
    (Attr.taskRunning, fg yellow),
    (Attr.taskPhase, italic' (fg yellow)),
    (Attr.taskSucceeded, fg green),
    (Attr.taskFailed, fg red),
    (Attr.taskName, bold' defAttr),
    (Attr.taskTime, style dim),
    (Attr.taskResult, fg brightYellow),
    (Attr.sectionActiveTasks, bold' (fg yellow)),
    (Attr.sectionProject, bold' (fg cyan)),
    (Attr.sectionSettings, bold' (fg magenta)),
    (Attr.opLogIndicator, bold' (fg green)),
    (Attr.opLogText, fg brightWhite),
    (Attr.startServerLabel, fg blue),
    (Attr.haskellLogoArrow, bold' (fg (RGBColor 0x45 0x3a 0x62))),
    (Attr.haskellLogoLambda, bold' (fg (RGBColor 0x5e 0x50 0x86))),
    (Attr.haskellLogoEquals, fg (RGBColor 0x8f 0x4e 0x8b)),
    (Attr.moduleName, bold' (fg blue)),
    (Attr.metadata, bold' (fg magenta)),
    (Attr.execute, bold' (fg green)),
    (Attr.nodeLabel, bold' defAttr)
  ]
  where
    fg = withForeColor defAttr

    bold' a = withStyle a bold

    italic' a = withStyle a italic

    style = withStyle defAttr

app :: ServerHandlers -> App MainState MainEvent Name
app server =
  App {
    appDraw = renderMain,
    appStartEvent = initEvent,
    appHandleEvent = flip runReaderT server . handleUiEvent,
    appAttrMap = attrMapMain,
    appChooseCursor = showFirstCursor
  }
