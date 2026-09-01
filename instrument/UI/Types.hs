module UI.Types where

import Brick.AttrMap (AttrName, attrName)
import Data.Text (Text)

data Name
  = ActiveTasks
  | TaskDetails
  | TaskStats
  | TaskTree
  | SessionSelector
  | OptionsEditor
  | OEExtraGhcOptions
  | ServeOptions
  | SOPath
  | SOExtraOptions
  | LogViewer
  deriving stock (Eq, Ord, Show)

newtype WorkerId = WorkerId { unWorkerId :: Text }
  deriving stock (Eq, Ord, Show)

disabledAttr :: AttrName
disabledAttr = attrName "disabled"

canDebugAttr :: AttrName
canDebugAttr = attrName "canDebug"

-- | Applied to bytecode-cache browser rows for modules no longer resident in the worker's loader state (i.e.
-- evicted), to visually distinguish them from currently-loaded modules.
evictedAttr :: AttrName
evictedAttr = attrName "evicted"

-- | Applied (in addition to, or on top of, 'evictedAttr') to bytecode-cache browser rows with a pending eviction
-- request that hasn't been applied by the worker yet.
pendingEvictionAttr :: AttrName
pendingEvictionAttr = attrName "pendingEviction"

-- | Applied to active-task rows for tasks that are still running.
taskRunningAttr :: AttrName
taskRunningAttr = attrName "taskRunning"

-- | Applied to active-task rows for tasks that are running and have an active phase.
taskPhaseAttr :: AttrName
taskPhaseAttr = attrName "taskPhase"

-- | Applied to active-task rows for tasks that finished successfully.
taskSucceededAttr :: AttrName
taskSucceededAttr = attrName "taskSucceeded"

-- | Applied to active-task rows for tasks that finished with an error.
taskFailedAttr :: AttrName
taskFailedAttr = attrName "taskFailed"

-- | Applied to the indicator prefix of an operational-log line (see 'UI.OpLog').
opLogIndicatorAttr :: AttrName
opLogIndicatorAttr = attrName "opLogIndicator"

-- | Applied to the message text of an operational-log line (see 'UI.OpLog').
opLogTextAttr :: AttrName
opLogTextAttr = attrName "opLogText"

-- | Applied to the "Start server" label rendered above the server-start form's input fields (see
-- 'UI.drawStartServer').
startServerLabelAttr :: AttrName
startServerLabelAttr = attrName "startServerLabel"

-- | Foreground color for the "arrow" component of the Haskell logo (see 'UI.haskellArt'\/'UI.drawHaskellArt'):
-- the leftmost double-chevron bracket shape, the darkest of the logo's three purple tones
-- (@#453a62@, matching the official @purple0@ from the source in @georgefst\/haskell-logo@).
haskellLogoArrowAttr :: AttrName
haskellLogoArrowAttr = attrName "haskellLogoArrow"

-- | Foreground color for the "lambda" component of the Haskell logo: the stylized "\955" glyph, the medium
-- purple tone (@#5e5086@, @purple1@).
haskellLogoLambdaAttr :: AttrName
haskellLogoLambdaAttr = attrName "haskellLogoLambda"

-- | Foreground color for the "equals" component of the Haskell logo: the two-bar "=" to the right of the
-- lambda, the lightest\/most pink-toned purple (@#8f4e8b@, @purple2@).
haskellLogoEqualsAttr :: AttrName
haskellLogoEqualsAttr = attrName "haskellLogoEquals"

-- | Applied to an active-task row's target name (see 'UI.ActiveTasks.draw') and, analogously, to a bytecode
-- cache stats line's primary figure (the BCO count, see 'UI.TaskTree.draw'\'s @bcoLine@) -- the "headline"
-- part of a two-part row that 'taskTimeAttr' styles the secondary part of.
taskNameAttr :: AttrName
taskNameAttr = attrName "taskName"

-- | Applied to an active-task row's elapsed-time\/status label and, analogously, to a bytecode cache stats
-- line's last-access figure -- the secondary, de-emphasized part of a row 'taskNameAttr' styles the primary
-- part of.
taskTimeAttr :: AttrName
taskTimeAttr = attrName "taskTime"

taskResultAttr :: AttrName
taskResultAttr = attrName "taskResult"

-- | Foreground accent for the "Active Tasks" panel header. Panel headers replace the borders that used to
-- delimit the main view's panels (active tasks\/project) -- see 'UI.ActiveTasks.draw', 'UI.TaskTree.draw' --
-- with distinct color accents instead, so the panels remain visually distinguishable without drawing a border
-- around each of them.
sectionActiveTasksAttr :: AttrName
sectionActiveTasksAttr = attrName "sectionActiveTasks"

-- | Foreground accent for the "Project" panel header (see 'sectionActiveTasksAttr'). Also used for the
-- project view's bytecode-cache child rows, since that panel was merged into this one.
sectionProjectAttr :: AttrName
sectionProjectAttr = attrName "sectionProject"

-- | Applied to a module name within a task\/tree label (see 'UI.Utils.styledTarget', 'UI.TaskTree.draw'):
-- blue, bold. Shared between the task view's @unit  module@ labels and the project view's module rows so
-- both use the same color coding.
moduleNameAttr :: AttrName
moduleNameAttr = attrName "moduleName"

-- | Applied to the literal @"metadata"@ keyword within a task label (see 'UI.Utils.styledTarget'): magenta,
-- bold.
metadataAttr :: AttrName
metadataAttr = attrName "metadata"

-- | Applied to the literal @"execute"@ keyword within a task label (see 'UI.Utils.styledTarget'): green,
-- bold.
executeAttr :: AttrName
executeAttr = attrName "execute"

-- | Applied to a project-view node's own label (unit header\/module name), bold -- so node labels share the
-- same weight as the task view's colored labels ('moduleNameAttr'\/'metadataAttr').
nodeLabelAttr :: AttrName
nodeLabelAttr = attrName "nodeLabel"

-- | Suffix appended to a 'UI.TaskTree.ModuleRow' label once that module has been built successfully.
--
-- A plain check mark ('\10004', single-width in virtually every terminal font) rather than an emoji-presentation
-- glyph, so it doesn't need 'UI.Utils.wideStr'\'s explicit-width workaround.
builtMarker :: Text
builtMarker = " \10004"

-- | Suffix appended to a 'UI.TaskTree.ModuleRow'\/'Header' label once that module\/unit has failed to build.
--
-- A plain ballot X ('\10008'), matching 'builtMarker'\'s use of a plain check mark rather than an
-- emoji-presentation glyph.
failedMarker :: Text
failedMarker = " \10008"
