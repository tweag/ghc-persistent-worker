module Ghc.Ui.Render.Tasks where

import Brick (AttrName, Padding (..), Widget, padLeft, str, strWrap, txt, vBox, vLimit, withAttr, (<+>))
import Brick.Widgets.List (renderList)
import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Time (UTCTime, defaultTimeLocale, diffUTCTime, formatTime, nominalDiffTimeToSeconds)
import Ghc.Ui.Attr qualified as Attr
import Ghc.Ui.Data.Name (Name (Tasks))
import Ghc.Ui.Data.Tasks (Outcome (..), PhaseInfo (..), Task (..), TasksRow (..), TasksState)
import Ghc.Ui.Render.Format (formatPico)
import qualified Ghc.Ui.Render.OpLog as OpLog
import Ghc.Ui.Render.Popup (popup)
import Ghc.Ui.Render.Section (drawSection)
import Ghc.Ui.Render.Target (styledTarget)
import Types.Api (renderTarget)

-- | The marker string and attribute used to indicate a task's current state.
stateMarker :: Task -> (String, AttrName)
stateMarker Task {outcome, phase} =
  case (outcome, phase) of
    (Nothing, Just p) -> (p, Attr.taskPhase)
    (Nothing, Nothing) -> ("...", Attr.taskRunning)
    (Just (Succeeded _), _) -> ("\10004", Attr.taskSucceeded) -- \x2714 heavy check mark
    (Just (Failed _), _) -> ("\10008", Attr.taskFailed) -- \x2718 heavy ballot X

renderTaskDetails :: Task -> Widget Name
renderTaskDetails Task {target, phases, outcome} =
  popup 30 (renderTarget target) $
    vBox $
      withAttr Attr.taskName (styledTarget (renderTarget target))
        : outcomeLines
        ++ phaseLines
 where
  outcomeLines = case outcome of
    Just (Failed content) -> [strWrap content]
    Just (Succeeded (Just result)) -> [strWrap ("Result: " ++ result)]
    _ -> []
  phaseLines
    | null phases = []
    | otherwise = str " " : (drawPhase <$> (sortOn ((.order) . snd) (Map.toList phases)))
  drawPhase (p, PhaseInfo {durationMs}) =
    str p <+> str (replicate 2 ' ') <+> withAttr Attr.taskTime (str (show durationMs ++ "ms"))

-- | Header line replacing the border that used to delimit this panel; see 'Ghc.Ui.Attr.sectionActiveTasks'.
-- Uses 'UI.Utils.drawSection's permanent placeholder rectangle for visual structure.
renderTasks :: Name -> UTCTime -> TasksState -> Widget Name
renderTasks current now state =
  drawSection Attr.sectionActiveTasks (withAttr Attr.sectionActiveTasks (str "Tasks")) $
    renderList drawRow (current == Tasks) state
 where
  drawRow _ (Separator msg) =
    withAttr Attr.disabled $ txt ("\9472\9472 " <> msg <> " \9472\9472")
  drawRow _ (TaskRow task@Task {target, ..}) =
    let (status, attr) = stateMarker task
        elapsed = nominalDiffTimeToSeconds (max 0 (diffUTCTime (fromMaybe now endTime) startTime))
        progress = case outcome of
          Just (Failed _) -> "Failure"
          _ -> formatPico elapsed
        timestamp = withAttr Attr.taskTime (str (formatTime defaultTimeLocale "%H:%M:%S" startTime ++ " "))
        header =
          (if debuggable then withAttr Attr.debuggable else id) $
            -- The timestamp (subdued\/dim, mirroring 'progressLine' below) leads the label; the marker
            -- ('stateMarker') is a plain single-width character, moved to the end of the row instead of
            -- leading it. The target name itself is rendered via 'UI.Utils.styledTarget' for the
            -- module\/metadata syntax highlighting, with 'Attr.taskName' as its default for the unrecognized
            -- (unit-name) part.
            timestamp
              <+> withAttr Attr.taskName (styledTarget (renderTarget target))
              <+> str " "
              <+> withAttr attr (str status)
        -- Status (elapsed time or "Failure") is rendered on its own indented line below the target name,
        -- rather than right-aligned on the same line: right-aligning it made it hard to visually associate
        -- with the target it belongs to, especially once lines wrap or targets vary in length, and there is
        -- no need for rows to stretch to the panel's full width just to right-align one word. This mirrors
        -- how an execute task's result is already shown on its own line below ('drawResult').
        progressLine = padLeft (Pad 2) (withAttr Attr.taskTime (txt progress))
        result = case outcome of
          Just (Succeeded (Just r)) -> Just r
          _ -> Nothing
     in vBox ([header, progressLine] ++ maybe [] (pure . drawResult) result)

  -- A successful execute task's exfiltrated result, rendered on the lines following its row: wrapped to the
  -- available width, truncated to 4 lines, indented by two cells, and left uncolored (unlike the marker/status
  -- above it).
  drawResult r = padLeft (Pad 2) (vLimit 4 (withAttr Attr.opLogIndicator (txt OpLog.indicator) <+> withAttr Attr.taskResult (strWrap r)))
