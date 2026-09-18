module Ghc.Ui.Render.Section where

import Brick (AttrName, Padding (Pad), Widget (..), hBox, padLeft, str, vBox, withAttr)

-- | Draws a panel's header line plus its content, following the borderless section style (see
-- 'Ghc.Ui.Attr.sectionActiveTasks' and friends). A small solid rectangle (6 columns wide, 3 rows high, in
-- the section's accent color) is always drawn on the left edge, top-aligned with where the content starts
-- (i.e. two lines below the header), giving the panel a permanent visual anchor independent of whether it
-- currently has content. The header is indented four spaces so it doesn't align above the rectangle, and the
-- content area is offset two more cells past the rectangle's width.
drawSection :: AttrName -> Widget n -> Widget n -> Widget n
drawSection attr headline content =
  vBox [
    padLeft (Pad 4) headline,
    str " ",
    hBox [
      withAttr attr (vBox (replicate 6 (str (replicate 3 '\9608')))),
      padLeft (Pad 2) content
    ]
  ]
