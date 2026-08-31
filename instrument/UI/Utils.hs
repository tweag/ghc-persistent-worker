module UI.Utils where

import Brick.AttrMap (AttrName, attrMapLookup)
import Brick.Types (Context (ctxAttrMap, ctxAttrName), EventM, Result (image), Size (Fixed), Widget (..), emptyResult, getContext)
import Brick.Widgets.Border (borderWithLabel)
import Brick.Widgets.Center (centerLayer)
import Brick.Widgets.Core (Padding (Pad), hBox, hLimitPercent, padLeft, str, vBox, vLimitPercent, withAttr, (<+>))
import Brick.Widgets.List (GenericList, Splittable, handleListEvent, handleListEventVi)
import Data.Fixed (Fixed (..), Pico)
import Data.Sequence qualified as Seq
import Data.Text.Lazy qualified as TL
import Graphics.Vty qualified as V
import Graphics.Vty.Image.Internal qualified as VI
import Lens.Micro.Platform (Traversal', zoom)
import UI.Types (Name, metadataAttr, moduleNameAttr, executeAttr)

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

-- | Render a colon-separated @unit:thing@ target string with syntax highlighting: the part after the colon
-- is shown in 'UI.Types.moduleNameAttr' (blue, bold) or, if it is the literal @"metadata"@ keyword, in
-- 'UI.Types.metadataAttr' (magenta, bold) instead -- and the colon separator itself is replaced with two
-- spaces. A string with no colon (unrecognized shape) is rendered plainly, unstyled.
styledTarget :: String -> Widget n
styledTarget spec =
  case break (== ':') spec of
    (unitPart, ':' : rest) -> str unitPart <+> str "  " <+> coloredRest rest
    _ -> str spec
 where
  coloredRest = \case
    "metadata" -> withAttr metadataAttr (str "metadata")
    "execute" -> withAttr executeAttr (str "execute")
    m -> case break (== ':') m of
      (modulePart, ':' : rest) -> renderModuleName modulePart <+> str "  " <+> coloredRest rest
      _ -> renderModuleName m

  renderModuleName = withAttr moduleNameAttr . str

-- | Draws a panel's header line plus its content, following the borderless section style (see
-- 'UI.Types.sectionActiveTasksAttr' and friends). A small solid rectangle (6 columns wide, 3 rows high, in
-- the section's accent color) is always drawn on the left edge, top-aligned with where the content starts
-- (i.e. two lines below the header), giving the panel a permanent visual anchor independent of whether it
-- currently has content. The header is indented four spaces so it doesn't align above the rectangle, and the
-- content area is offset two more cells past the rectangle's width.
drawSection :: AttrName -> Widget n -> Widget n -> Widget n
drawSection attr headline content =
  vBox
    [ padLeft (Pad 4) headline
    , str " "
    , hBox [withAttr attr (vBox (replicate 6 (str (replicate 3 '\9608')))), padLeft (Pad 2) content]
    ]
