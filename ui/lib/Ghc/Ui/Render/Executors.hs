module Ghc.Ui.Render.Executors where

import Brick (Widget, hBox, txt, vLimit, withAttr)
import Brick.Widgets.List (renderList)
import Ghc.Ui.Attr qualified as Attr
import Ghc.Ui.Data.Executors (ExecutorRow (..), ExecutorsState (..))
import Ghc.Ui.Data.Name (Name (..))
import Ghc.Ui.Render.Format (formatBytes)
import Ghc.Ui.Render.Section (drawSection)
import Types.Api (ExecutorId (..), ExecutorStatus (..))
import Types.Text (showText)

renderExecutor :: ExecutorRow -> Widget n
renderExecutor ExecutorRow {executor, pid, status, killing} =
  hBox [
    withAttr Attr.nodeLabel (txt executor.text),
    txt (foldMap (\ p -> " [" <> showText p <> "]") pid),
    txt (" mem " <> formatBytes status.memInUseBytes),
    txt (", live " <> formatBytes status.liveBytes),
    txt (", " <> showText status.activeTasks <> " active"),
    if killing then withAttr Attr.taskFailed (txt " (killing)") else txt ""
  ]

renderExecutors :: Name -> ExecutorsState -> Widget Name
renderExecutors current ExecutorsState {rows} =
  drawSection Attr.sectionExecutors (withAttr Attr.sectionExecutors (txt "Executors")) $
  vLimit 6 $
  if null rows
  then txt "No executors running"
  else renderList (const renderExecutor) (current == Executors) rows
