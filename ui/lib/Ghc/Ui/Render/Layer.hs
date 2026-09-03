module Ghc.Ui.Render.Layer where

import Brick (
  Context,
  Location (..),
  RenderM,
  Result,
  Size (..),
  Widget (..),
  addResultOffset,
  availHeight,
  availWidth,
  getContext,
  image,
  )
import Graphics.Vty (imageHeight, imageWidth, translate)

wrap ::
  (Result n -> Context n -> (Int, Int)) ->
  Widget n ->
  RenderM n (Result n)
wrap compute widget = do
  result <- render widget
  ctx <- getContext
  let (x, y) = compute result ctx
  pure if x > 0 || y > 0 then addOffset x y result else result
  where
    addOffset x y result =
      addResultOffset (Location (x, y)) $
      result {image = translate x y (image result)}

-- | Position a widget as a transparent, non-space-filling layer -- like 'Brick.Widgets.Center.vCenterLayer',
-- which this generalizes (a fraction of 0.5 reproduces it exactly) -- so that its vertical center sits at the
-- given fraction of the available height, measured from the top of the rendering context. Only usable as a
-- top-level layer (see 'Brick.Main.App' 'appDraw'\/'drawUI'): unlike 'vCenterLayer' it isn't meant to be nested
-- inside another layout, since its positioning is computed against whatever the ambient context's available
-- height happens to be at the point it renders.
vAnchorLayer :: Double -> Widget n -> Widget n
vAnchorLayer frac widget =
  Widget (hSize widget) Greedy $ wrap computeOffset widget
  where
    computeOffset result ctx =
      (0, round (frac * fromIntegral (availHeight ctx)) - imageHeight (image result) `div` 2)

-- | Position a widget as a transparent, non-space-filling layer -- the horizontal, right-anchored analogue of
-- 'vAnchorLayer' (and of 'Brick.Widgets.Center.hCenterLayer', which this would reproduce if the margin were
-- chosen to center rather than right-align) -- so that its right edge sits the given number of columns from
-- the right edge of the whole screen. Only usable as a top-level layer, for the same reason as 'vAnchorLayer'.
hAnchorRightLayer :: Int -> Widget n -> Widget n
hAnchorRightLayer marginRight widget =
  Widget Greedy (vSize widget) $ wrap computeOffset widget
  where
    computeOffset result ctx =
      (availWidth ctx - imageWidth (image result) - marginRight, 0)
