-- | Operational message log: a small set of high-visibility, prominently displayed status\/lifecycle lines (e.g.
-- @ghc-server@ startup, shutdown steps), distinct from the fine-grained per-target entries in 'UI.LogViewer'. This
-- generalizes the earlier single-label placeholder text (\"Waiting for first session\") into an append-only list.
--
-- Stored at the top level of 'UI.State' rather than per-session ('UI.Session.State'), since operational events can
-- occur outside any session's lifetime (server startup before a session connects, shutdown after the session list
-- has been cleared).
module UI.OpLog where

import Brick.Types (Widget)
import Brick.Widgets.Core (hBox, txt, vBox, withAttr)
import Data.Coerce (coerce)
import Data.Text (Text)
import UI.Types (opLogIndicatorAttr, opLogTextAttr)

-- | A single operational message.
newtype Entry = Entry {message :: Text}
  deriving stock (Eq, Show)

-- | All operational messages ever recorded, newest first. Unbounded, like 'UI.LogViewer.State'\'s @rawEntries@;
-- only the most recent few are ever rendered (see 'latest'\/'draw').
newtype State = State {entries :: [Entry]}
  deriving stock (Eq, Show)

initialState :: State
initialState = State {entries = []}

addEntry :: Text -> State -> State
addEntry msg (State es) = State (Entry msg : es)

-- | Prefix rendered ahead of the latest message line.
indicator :: Text
indicator = "🮥 "

-- | Renders the @n@ most recent messages, newest at the bottom prefixed by 'indicator' (green) with the
-- message text in bright white. Left-aligned and unbordered; callers are responsible for placement (centering,
-- width limits, panes).
draw :: Int -> State -> Widget n
draw n =
  vBox . reverse . drawEntries . take n . coerce
 where
   drawEntries = \case
      [] -> []
      h : t -> drawLatest h : (txt . coerce <$> t)

   drawLatest (Entry msg) =
     hBox [
       withAttr opLogIndicatorAttr (txt indicator),
       withAttr opLogTextAttr (txt msg)
     ]
