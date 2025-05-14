{-# language NoImplicitPrelude #-}

module M1 (
  RequireCallStack,
  require,
) where

import Prelude (id)
import RequireCallStack

require :: RequireCallStack => a -> a
require = id
