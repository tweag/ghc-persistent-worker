module Ghc.Ui.Render.Target where

import Brick (Widget (..), txt, withAttr, (<+>))
import qualified Data.Text as Text
import Data.Text (Text)
import Ghc.Ui.Attr qualified as Attr

-- | Render a colon-separated @unit:thing@ target string with syntax highlighting: the part after the colon
-- is shown in 'Ghc.Ui.Attr.moduleName' (blue, bold) or, if it is the literal @"metadata"@ keyword, in
-- 'Ghc.Ui.Attr.metadata' (magenta, bold) instead -- and the colon separator itself is replaced with two
-- spaces. A string with no colon (unrecognized shape) is rendered plainly, unstyled.
-- TODO operate directly on Target
styledTarget :: Text -> Widget n
styledTarget spec =
  case Text.break (== ':') spec of
    (unitPart, suf) | Just (':', rest) <- Text.uncons suf -> txt unitPart <+> txt "  " <+> coloredRest rest
    _ -> txt spec
 where
  coloredRest = \case
    "metadata" -> withAttr Attr.metadata (txt "metadata")
    "execute" -> withAttr Attr.execute (txt "execute")
    m -> case Text.break (== ':') m of
      (modulePart, suf) | Just (':', rest) <- Text.uncons suf -> renderModuleName modulePart <+> txt "  " <+> coloredRest rest
      _ -> renderModuleName m

  renderModuleName = withAttr Attr.moduleName . txt
