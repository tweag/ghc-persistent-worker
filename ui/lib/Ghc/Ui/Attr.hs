module Ghc.Ui.Attr where

import Brick (AttrName, attrName)
import Data.Text (Text)

disabled :: AttrName
disabled = attrName "disabled"

debuggable :: AttrName
debuggable = attrName "debuggable"

-- | Applied to bytecode-cache browser rows for modules no longer resident in the worker's loader state (i.e.
-- evicted), to visually distinguish them from currently-loaded modules.
evicted :: AttrName
evicted = attrName "evicted"

-- | Applied (in addition to, or on top of, 'evicted') to bytecode-cache browser rows with a pending eviction
-- request that hasn't been applied by the worker yet.
pendingEviction :: AttrName
pendingEviction = attrName "pendingEviction"

-- | Applied to active-task rows for tasks that are still running.
taskRunning :: AttrName
taskRunning = attrName "taskRunning"

-- | Applied to active-task rows for tasks that are running and have an active phase.
taskPhase :: AttrName
taskPhase = attrName "taskPhase"

-- | Applied to active-task rows for tasks that finished successfully.
taskSucceeded :: AttrName
taskSucceeded = attrName "taskSucceeded"

-- | Applied to active-task rows for tasks that finished with an error.
taskFailed :: AttrName
taskFailed = attrName "taskFailed"

-- | Applied to the indicator prefix of an operational-log line (see 'UI.OpLog').
opLogIndicator :: AttrName
opLogIndicator = attrName "opLogIndicator"

-- | Applied to the message text of an operational-log line (see 'UI.OpLog').
opLogText :: AttrName
opLogText = attrName "opLogText"

-- | Applied to the "Start server" label rendered above the server-start form's input fields (see
-- 'UI.drawStartServer').
startServerLabel :: AttrName
startServerLabel = attrName "startServerLabel"

-- | Foreground color for the "arrow" component of the Haskell logo (see 'UI.haskellArt'\/'UI.drawHaskellArt'):
-- the leftmost double-chevron bracket shape, the darkest of the logo's three purple tones
-- (@#453a62@, matching the official @purple0@ from the source in @georgefst\/haskell-logo@).
haskellLogoArrow :: AttrName
haskellLogoArrow = attrName "haskellLogoArrow"

-- | Foreground color for the "lambda" component of the Haskell logo: the stylized "\955" glyph, the medium
-- purple tone (@#5e5086@, @purple1@).
haskellLogoLambda :: AttrName
haskellLogoLambda = attrName "haskellLogoLambda"

-- | Foreground color for the "equals" component of the Haskell logo: the two-bar "=" to the right of the
-- lambda, the lightest\/most pink-toned purple (@#8f4e8b@, @purple2@).
haskellLogoEquals :: AttrName
haskellLogoEquals = attrName "haskellLogoEquals"

-- | Applied to an active-task row's target name (see 'UI.ActiveTasks.draw') and, analogously, to a bytecode
-- cache stats line's primary figure (the BCO count, see 'UI.Project.draw'\'s @bcoLine@) -- the "headline"
-- part of a two-part row that 'taskTime' styles the secondary part of.
taskName :: AttrName
taskName = attrName "taskName"

-- | Applied to an active-task row's elapsed-time\/status label and, analogously, to a bytecode cache stats
-- line's last-access figure -- the secondary, de-emphasized part of a row 'taskName' styles the primary
-- part of.
taskTime :: AttrName
taskTime = attrName "taskTime"

taskResult :: AttrName
taskResult = attrName "taskResult"

-- | Foreground accent for the "Active Tasks" panel header. Panel headers replace the borders that used to
-- delimit the main view's panels (active tasks\/project) -- see 'UI.ActiveTasks.draw', 'UI.Project.draw' --
-- with distinct color accents instead, so the panels remain visually distinguishable without drawing a border
-- around each of them.
sectionActiveTasks :: AttrName
sectionActiveTasks = attrName "sectionActiveTasks"

-- | Foreground accent for the "Project" panel header (see 'sectionActiveTasks'). Also used for the
-- project view's bytecode-cache child rows, since that panel was merged into this one.
sectionProject :: AttrName
sectionProject = attrName "sectionProject"

sectionSettings :: AttrName
sectionSettings = attrName "sectionSettings"

-- | Applied to a module name within a task\/tree label (see 'UI.Utils.styledTarget', 'UI.Project.draw'):
-- blue, bold. Shared between the task view's @unit  module@ labels and the project view's module rows so
-- both use the same color coding.
moduleName :: AttrName
moduleName = attrName "moduleName"

-- | Applied to the literal @"metadata"@ keyword within a task label (see 'UI.Utils.styledTarget'): magenta,
-- bold.
metadata :: AttrName
metadata = attrName "metadata"

-- | Applied to the literal @"execute"@ keyword within a task label (see 'UI.Utils.styledTarget'): green,
-- bold.
execute :: AttrName
execute = attrName "execute"

-- | Applied to a project-view node's own label (unit header\/module name), bold -- so node labels share the
-- same weight as the task view's colored labels ('moduleName'\/'metadata').
nodeLabel :: AttrName
nodeLabel = attrName "nodeLabel"

-- | Suffix appended to a 'UI.Project.ModuleRow' label once that module has been built successfully.
builtMarker :: Text
builtMarker = " \10004"

-- | Suffix appended to a 'UI.Project.ModuleRow'\/'Header' label once that module\/unit has failed to build.
failedMarker :: Text
failedMarker = " \10008"
