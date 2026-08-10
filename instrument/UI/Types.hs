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

-- | Suffix appended to a 'UI.TaskTree.ModuleRow' label once that module has been built successfully.
builtMarker :: Text
builtMarker = " \9989"
