{-# LANGUAGE ApplicativeDo #-}

module Ghc.Ui.Cli where

import Options.Applicative (
  Parser,
  ParserInfo,
  fullDesc,
  header,
  help,
  helper,
  info,
  long,
  metavar,
  optional,
  progDesc,
  strOption,
  switch,
  (<**>),
  )
import System.OsPath (OsPath)
import System.OsPath.Extra (toOsPath)

data Options =
  Options {
    serverExe :: Maybe OsPath,
    -- | Skip cleaning and killing the ghc-server when quitting the UI.
    remain :: Bool
  }

optionsParser :: Parser Options
optionsParser = do
  serverExe <- optional (toOsPath <$> strOption (long "server-exe" <> metavar "PATH" <> help serverExeHelp))
  remain <- switch (long "remain" <> help remainHelp)
  pure Options {..}
  where
    serverExeHelp = "Path to the ghc-server executable to use when starting one (defaults to a PATH lookup)"

    remainHelp = "Do not kill the ghc-server instance or clean its cache/output directories on exit"

optionsInfo :: ParserInfo Options
optionsInfo =
  info (optionsParser <**> helper) (fullDesc <> progDesc "Terminal UI for ghc-server" <> header "ghc-ui")
