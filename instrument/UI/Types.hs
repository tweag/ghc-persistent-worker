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
  deriving stock (Eq, Ord, Show)

newtype WorkerId = WorkerId { unWorkerId :: Text }
  deriving stock (Eq, Ord, Show)

disabledAttr :: AttrName
disabledAttr = attrName "disabled"

canDebugAttr :: AttrName
canDebugAttr = attrName "canDebug"