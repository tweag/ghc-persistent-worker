module M2 (
  module M1,
  user,
) where

import M1
import RequireCallStack (provideCallStack)

user :: ()
user = provideCallStack (require ())
