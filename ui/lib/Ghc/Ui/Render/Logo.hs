module Ghc.Ui.Render.Logo where

import Brick (AttrName, Padding (Pad), Widget (..), hBox, padTop, txt, vBox, withAttr)
import qualified Data.Text as Text
import Ghc.Ui.Attr qualified as Attr
import Ghc.Ui.Data.Name (Name)
import Ghc.Ui.Render.Layout (hAnchorRightLayer)

data Cell =
  Cell {
    count :: Int,
    char :: Char,
    color :: Maybe AttrName
  }

renderCell :: Cell -> Widget Name
renderCell Cell {..} =
  maybe id withAttr color (txt (Text.replicate count (Text.singleton char)))

haskellLogo :: [[Cell]]
haskellLogo =
  [
    line1 0,
    line1 1,
    line1 2,
    line1 3,
    line2 4 10,
    line2 5 9,
    line [
      (6, segment 5 chevron chevronRightNegative chevronRight),
      (1, segment 5 lambda chevronRightNegative ur)
    ],
    line3 5 7 7,
    line3 4 9 6,
    line [
      (3, segment 5 chevron ul lr),
      (1, segment 5 lambda ul chevronUpNegative),
      (0, segment 4 lambda full ur)
    ],
    line4 2 1,
    line4 1 3,
    line4 0 5
  ]
 where
  chevron = Attr.haskellLogoArrow
  lambda = Attr.haskellLogoLambda
  equals = Attr.haskellLogoEquals

  line1 pre =
    line [
      (pre, chevron2),
      (1, lambda2)
    ]

  line2 pre eqWidth =
    line [
      (pre, chevron2),
      (1, segment 5 lambda ll ur),
      (1, segment eqWidth equals ll full)
    ]

  line3 pre lamWidth eqWidth =
    line [
      (pre, segment 5 chevron ul lr),
      (1, segment lamWidth lambda ul ur),
      (1, segment eqWidth equals ll full)
    ]

  line4 pre gap =
    line [
      (pre, chevron1),
      (1, lambda1),
      (gap, lambda2)
    ]

  chevron1 = segment 5 chevron ul lr

  chevron2 = segment 5 chevron ll ur

  lambda1 = segment 5 lambda ul lr

  lambda2 = segment 5 lambda ll ur

  line cells = mconcat [ws w ++ c | (w, c) <- cells]

  ws count =
    [
      Cell {
        count,
        char = ' ',
        color = Nothing
      }
    ]

  segment width color l r =
    [
      Cell 1 l (Just color),
      Cell width full (Just color),
      Cell 1 r (Just color)
    ]

  ul = '◢'
  ur = '◣'
  ll = '◥'
  lr = '◤'
  full = '█'
  chevronRight = '🭬'
  chevronRightNegative = '🭨'
  chevronUpNegative = '🭫'

-- | The Haskell logo banner layer, anchored to the top-right corner of the whole screen with a two-cell margin from
-- both edges.
renderLogo :: Widget Name
renderLogo =
  hAnchorRightLayer 2 $ padTop (Pad 2) $ vBox (logo ++ [txt " ", lettering])
  where
    logo = hBox . fmap renderCell <$> haskellLogo

    lettering =
      hBox [
        withAttr Attr.haskellLogoArrow (txt "   G H C"),
        withAttr Attr.haskellLogoLambda (txt "      S E R V E R")
      ]
