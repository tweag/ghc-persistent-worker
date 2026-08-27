module UI.Types where

import Brick.AttrMap (AttrName, attrName)
import Data.Text (Text)

data Name
  = ActiveTasks
  | TaskDetails
  | TaskTree
  | SessionSelector
  | OptionsEditor
  | OEExtraGhcOptions
  | BytecodeBrowser
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

-- | Suffix appended to a 'UI.TaskTree.ModuleRow' label once that module has been built successfully.
builtMarker :: Text
builtMarker = " \9989"
