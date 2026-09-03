module Ghc.Ui.Data.Name where

import Data.Generics.Labels ()

data Name =
  Global
  |
  Tasks
  |
  TaskDetails
  |
  Project
  |
  Sessions
  |
  StartServer
  |
  StartServerRoot
  |
  StartServerOptions
  |
  Log
  |
  OpLog
  |
  OpLogDebug
  deriving stock (Eq, Ord, Show)
