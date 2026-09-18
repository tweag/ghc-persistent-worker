module Ghc.Ui.Render.Session where

import Brick (Padding (..), Widget (..), fill, hBox, hLimitPercent, padAll, padRight, txt, vBox, vLimit)
import Brick.Widgets.Border (hBorder)
import Data.Map qualified as Map
import qualified Data.Text as Text
import Data.Time (UTCTime)
import Ghc.Ui.Data.Name (Name)
import Ghc.Ui.Data.OpLog (OpLogState)
import Ghc.Ui.Data.Session (SessionState (..), Stats (..), Worker (..))
import Ghc.Ui.Render.Format (formatBytes, formatPs)
import Ghc.Ui.Render.OpLog (renderOpLogEmbed)
import Ghc.Ui.Render.Project (renderProject)
import Ghc.Ui.Render.Settings (renderSettings)
import Ghc.Ui.Render.Tasks (renderTasks)
import Types.Text (showText)

renderStats :: Int -> Stats -> Widget Name
renderStats workerCount Stats {..} =
  vBox [txt line1, txt line2]
  where
    line1 =
      " Worker count: " <> showText workerCount
      <>
      " | Memory:" <> memoryStats

    line2 =
      " CPU Time: " <> formatPs (1000 * cpu_ns)
      <>
      " | GC Time: " <> formatPs (1000 * gc_cpu_ns)

    memoryStats = Text.concat [" " <> k <> "=" <> formatBytes v | (k, v) <- Map.toList memory]

renderSession :: Name -> UTCTime -> OpLogState -> SessionState -> Widget Name
renderSession current now opLog SessionState {project, tasks, workers, finishedWorkerStats, settings} =
  vBox [
    padAll 2 $ hBox [
      hLimitPercent 50 $ padRight (Pad 3) $ vBox [renderProject current project, fill ' '],
      renderTasks current now tasks
    ],
    padAll 2 $ renderSettings current settings,
    hBorder,
    renderStats (length workers) (foldMap (.stats) workers <> finishedWorkerStats),
    hBorder,
    vLimit 6 (renderOpLogEmbed opLog)
  ]
