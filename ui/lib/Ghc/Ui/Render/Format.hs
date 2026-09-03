module Ghc.Ui.Render.Format where

import Data.Fixed (Fixed (..), Pico)
import Data.List.NonEmpty (NonEmpty (..), nonEmpty)
import Data.Text (Text)
import Types.Text (showText)

format :: (Integral a, Show a) => NonEmpty Text -> a -> Text
format (unit :| units) n
  | n >= 10_000
  , Just rest <- nonEmpty units
  = format rest (n `div` 1_000)
  | otherwise
  = showText n <> unit

formatBytes :: (Integral a, Show a) => a -> Text
formatBytes = format ["b", "Kb", "Mb", "Gb", "Tb", "Pb"]

formatPs :: (Integral a, Show a) => a -> Text
formatPs = format ["ps", "ns", "µs", "ms", "s"]

formatPico :: Pico -> Text
formatPico (MkFixed n) = formatPs n
