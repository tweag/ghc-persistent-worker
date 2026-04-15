-- | CLI entry point for the @gen-project@ executable.
module GhcServer.GenProject.Run where

import GhcServer.GenProject (
  ExtDepsConfig (..),
  productionUnitCount,
  writeFlatProject,
  writeProductionProject,
  writeProject,
  writeWideProject,
  wideUnitCount,
  )
import Options.Applicative (
  Parser,
  ParserInfo,
  argument,
  auto,
  command,
  execParser,
  fullDesc,
  header,
  help,
  helper,
  hsubparser,
  info,
  metavar,
  progDesc,
  str,
  (<**>),
  )
import System.Directory (createDirectoryIfMissing)
import System.Environment (lookupEnv)

-- | A project generation command parsed from CLI args.
data GenCommand =
  Deep {dir :: String, depth :: Int}
  |
  Wide {dir :: String, depth :: Int, modsPerUnit :: Int}
  |
  Flat {dir :: String, numModules :: Int}
  |
  Production {dir :: String, depth :: Int, bigMods :: Int}
  deriving stock (Show)

deepParser :: Parser GenCommand
deepParser =
  Deep
    <$> argument str (metavar "DIRECTORY" <> help "Output directory")
    <*> argument auto (metavar "DEPTH" <> help "Tree depth (> 0)")

wideParser :: Parser GenCommand
wideParser =
  Wide
    <$> argument str (metavar "DIRECTORY" <> help "Output directory")
    <*> argument auto (metavar "DEPTH" <> help "Tree depth (> 0)")
    <*> argument auto (metavar "MODULES_PER_UNIT" <> help "Modules per unit (> 0)")

flatParser :: Parser GenCommand
flatParser =
  Flat
    <$> argument str (metavar "DIRECTORY" <> help "Output directory")
    <*> argument auto (metavar "NUM_MODULES" <> help "Number of modules (> 0)")

productionParser :: Parser GenCommand
productionParser =
  Production
    <$> argument str (metavar "DIRECTORY" <> help "Output directory")
    <*> argument auto (metavar "DEPTH" <> help "Tree depth (> 0)")
    <*> argument auto (metavar "BIG_MODS" <> help "Modules per level in big unit (> 0)")

genCommandParser :: Parser GenCommand
genCommandParser =
  hsubparser
    ( command "deep" (info deepParser (progDesc "Binary tree of modules, 2*depth units (default)"))
    <> command "wide" (info wideParser (progDesc "Binary tree of units, 2^depth-1 units"))
    <> command "flat" (info flatParser (progDesc "Single unit, module 0 imports all others"))
    <> command "production" (info productionParser (progDesc "Binary tree of small units + one large downstream unit"))
    )

genParserInfo :: ParserInfo GenCommand
genParserInfo =
  info (genCommandParser <**> helper)
    (fullDesc
    <> progDesc "Generate test projects for ghc-server"
    <> header "gen-project - test project generator")

-- | Build an 'ExtDepsConfig' from the @resource_test_ext_deps@ environment variable.
--
-- Uses ext dep indexes 0–4 (the 5 independent leaf packages from @test-ext-deps.nix@).
getExtDepsConfig :: IO (Maybe ExtDepsConfig)
getExtDepsConfig =
  lookupEnv "resource_test_ext_deps" >>= \case
    Nothing -> pure Nothing
    Just dir -> pure (Just ExtDepsConfig {extDepsDir = dir, extDepIndexes = [0 .. 4]})

runGenProject :: IO ()
runGenProject = do
  cmd <- execParser genParserInfo
  extDeps <- getExtDepsConfig
  executeCommand extDeps cmd

executeCommand :: Maybe ExtDepsConfig -> GenCommand -> IO ()
executeCommand extDeps = \case
  Deep {dir, depth} -> do
    createDirectoryIfMissing True dir
    writeProject dir depth extDeps
    let levels = 2 * depth
        modules = sum [2 ^ (l + 1) | l <- [0 .. levels - 1]] :: Int
        units = 2 * depth
    putStrLn ("Generated project in " ++ dir)
    putStrLn ("  depth:   " ++ show depth ++ " (" ++ show levels ++ " levels)")
    putStrLn ("  units:   " ++ show units)
    putStrLn ("  modules: " ++ show modules)
    putStrLn ("  ext deps: " ++ extDepsInfo extDeps)

  Wide {dir, depth, modsPerUnit} -> do
    createDirectoryIfMissing True dir
    writeWideProject dir depth modsPerUnit extDeps
    let units = wideUnitCount depth
    putStrLn ("Generated wide project in " ++ dir)
    putStrLn ("  depth:          " ++ show depth)
    putStrLn ("  units:          " ++ show units)
    putStrLn ("  modules/unit:   " ++ show modsPerUnit)
    putStrLn ("  total modules:  " ++ show (units * modsPerUnit))
    putStrLn ("  ext deps: " ++ extDepsInfo extDeps)

  Flat {dir, numModules} -> do
    createDirectoryIfMissing True dir
    writeFlatProject dir numModules extDeps
    putStrLn ("Generated flat project in " ++ dir)
    putStrLn ("  units:          1")
    putStrLn ("  total modules:  " ++ show numModules)
    putStrLn ("  ext deps: " ++ extDepsInfo extDeps)

  Production {dir, depth, bigMods} -> do
    createDirectoryIfMissing True dir
    writeProductionProject dir depth bigMods extDeps
    let units = productionUnitCount depth
        treeUnits = units - 1
        bigUnitMods = 20 * bigMods + 1
    putStrLn ("Generated production project in " ++ dir)
    putStrLn ("  tree depth:     " ++ show depth)
    putStrLn ("  tree units:     " ++ show treeUnits ++ " (3 modules each)")
    putStrLn ("  big unit mods:  " ++ show bigUnitMods ++ " (20 levels x " ++ show bigMods ++ " + BigMain)")
    putStrLn ("  total units:    " ++ show units)
    putStrLn ("  total modules:  " ++ show (treeUnits * 3 + bigUnitMods))
    putStrLn ("  ext deps: " ++ extDepsInfo extDeps)

extDepsInfo :: Maybe ExtDepsConfig -> String
extDepsInfo =
  maybe "none" (\ c -> show (length c.extDepIndexes))
