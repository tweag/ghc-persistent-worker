module UI.Utils where

import Brick.AttrMap (attrMapLookup)
import Brick.Types (Context (ctxAttrMap, ctxAttrName), EventM, Result (image), Size (Fixed), Widget (..), emptyResult, getContext)
import Brick.Widgets.Border (borderWithLabel)
import Brick.Widgets.Center (centerLayer)
import Brick.Widgets.Core (hLimitPercent, str, vLimitPercent)
import Brick.Widgets.List (GenericList, Splittable, handleListEvent, handleListEventVi)
import Data.Fixed (Fixed (..), Pico)
import Data.Sequence qualified as Seq
import Data.Text.Lazy qualified as TL
import Graphics.Vty qualified as V
import Graphics.Vty.Image.Internal qualified as VI
import Lens.Micro.Platform (Traversal', zoom)
import UI.Types (Name)

popup :: Int -> String -> Widget Name -> Widget Name
popup size popupTitle content =
  centerLayer $
    hLimitPercent size $
      vLimitPercent size $
        borderWithLabel (str $ " " ++ popupTitle ++ " ") content

-- | Build a widget from a string with an explicitly declared display width, ignoring whatever vty's
-- 'wcwidth'-based computation would infer from the content.
--
-- Needed for glyphs (e.g. status emoji) whose 'Emoji_Presentation=Yes' Unicode property makes terminals
-- render them wider than vty's East-Asian-Width-based 'wcwidth' reports (typically 1 column instead of
-- the 2 most terminals actually draw). Ordinary Brick combinators ('hLimit', 'padRight') and even
-- rewriting a rendered image's width after the fact ('Graphics.Vty.Image.resizeWidth') don't help,
-- because vty recomputes the /physical/ cursor advance from the same per-character 'wcwidth' table when
-- it composites and flushes text spans - only text built through the low-level 'HorizText' constructor,
-- which carries an explicit, uninspected @outputWidth@, sidesteps that recomputation entirely. This
-- mirrors a widely-reported Brick GitHub issue workaround for the same problem.
--
-- The widget still resolves its attribute from the current rendering context/attr map (mimicking what
-- 'str'\/'txt' do), so it behaves like a normal attribute-aware widget under 'withAttr'.
wideStr :: Int -> String -> Widget n
wideStr displayWidth s =
  Widget Fixed Fixed $ do
    c <- getContext
    let a = attrMapLookup (ctxAttrName c) (ctxAttrMap c)
    pure emptyResult{image = VI.HorizText a (TL.pack s) displayWidth (length s)}

formatBytes :: (Integral a, Show a) => a -> String
formatBytes = format ["b", "Kb", "Mb", "Gb", "Tb", "Pb"]

formatPs :: (Integral a, Show a) => a -> String
formatPs = format ["ps", "ns", "µs", "ms", "s"]

formatPico :: Pico -> String
formatPico (MkFixed n) = formatPs n

format :: (Integral a, Show a) => [String] -> a -> String
format [unit] n = show n ++ unit
format (unit : units) n
  | n >= 10_000 = format units (n `div` 1_000)
  | otherwise = show n ++ unit
format [] _ = error "No units given"

stripEscSeqs :: String -> String
stripEscSeqs [] = []
stripEscSeqs ('\ESC' : '[' : xs) = stripEscSeqs (drop 1 (dropWhile (/= 'm') xs))
stripEscSeqs (x : xs) = x : stripEscSeqs xs

upsertAscSeq :: (Ord b) => (a -> b) -> a -> Seq.Seq a -> (Int, Seq.Seq a)
upsertAscSeq meas x as = binSearch 0 (Seq.length as - 1)
 where
  binSearch l r
    | l > r = (l, Seq.insertAt l x as)
    | otherwise =
        let m = (l + r) `div` 2
            x' = Seq.index as m
            b' = meas x'
         in if meas x < b'
              then binSearch l (m - 1)
              else
                if meas x > b'
                  then binSearch (m + 1) r
                  else (m, Seq.update m x as)

handleListEventOf :: (Foldable t, Splittable t, Ord n) => Traversal' s (GenericList n t e) -> V.Event -> EventM n s ()
handleListEventOf lens = zoom lens . handleListEventVi handleListEvent