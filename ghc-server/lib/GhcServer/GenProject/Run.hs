-- | CLI entry point for the @gen-project@ executable.
module GhcServer.GenProject.Run where

import GhcServer.GenProject (
  ExtDepsConfig (..),
  TestsShape (..),
  productionUnitCount,
  wideUnitCount,
  writeFlatProject,
  writeProductionProject,
  writeProject,
  writeTestsProject,
  writeWideProject,
  )
import GhcServer.GenProject.BuckTests (BuckTestsShape (..), defaultBuckTestsShape, writeBuckTestsProject)
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
  long,
  metavar,
  option,
  progDesc,
  str,
  value,
  (<**>),
  )
import System.Directory.OsPath (createDirectoryIfMissing)
import System.Environment (lookupEnv)
import System.Exit (die)
import System.OsPath (OsPath)
import System.OsPath.Extra (fromOsPath, toOsPath)

-- | A project generation command parsed from CLI args.
data GenCommand =
  Deep {dir :: OsPath, depth :: Int}
  |
  Wide {dir :: OsPath, depth :: Int, modsPerUnit :: Int}
  |
  Flat {dir :: OsPath, numModules :: Int}
  |
  Production {dir :: OsPath, depth :: Int, bigMods :: Int}
  |
  Tests {dir :: OsPath, shape :: TestsShape}
  |
  BuckTests {dir :: OsPath, buckShape :: BuckTestsShape}
  deriving stock (Show)

deepParser :: Parser GenCommand
deepParser =
  Deep
    <$> argument (toOsPath <$> str) (metavar "DIRECTORY" <> help "Output directory")
    <*> argument auto (metavar "DEPTH" <> help "Tree depth (> 0)")

wideParser :: Parser GenCommand
wideParser =
  Wide
    <$> argument (toOsPath <$> str) (metavar "DIRECTORY" <> help "Output directory")
    <*> argument auto (metavar "DEPTH" <> help "Tree depth (> 0)")
    <*> argument auto (metavar "MODULES_PER_UNIT" <> help "Modules per unit (> 0)")

flatParser :: Parser GenCommand
flatParser =
  Flat
    <$> argument (toOsPath <$> str) (metavar "DIRECTORY" <> help "Output directory")
    <*> argument auto (metavar "NUM_MODULES" <> help "Number of modules (> 0)")

productionParser :: Parser GenCommand
productionParser =
  Production
    <$> argument (toOsPath <$> str) (metavar "DIRECTORY" <> help "Output directory")
    <*> argument auto (metavar "DEPTH" <> help "Tree depth (> 0)")
    <*> argument auto (metavar "BIG_MODS" <> help "Modules per level in big unit (> 0)")

testsParser :: Parser GenCommand
testsParser =
  Tests
    <$> argument (toOsPath <$> str) (metavar "DIRECTORY" <> help "Output directory")
    <*> (TestsShape
      <$> argument auto (metavar "UNITS" <> help "Number of units (> 0)")
      <*> argument auto (metavar "MODULES" <> help "Modules per unit, including tests")
      <*> argument auto (metavar "TESTS" <> help "Test modules per unit (< MODULES)"))

buckTestsParser :: Parser GenCommand
buckTestsParser =
  BuckTests
    <$> argument (toOsPath <$> str) (metavar "DIRECTORY" <> help "Output directory")
    <*> (BuckTestsShape
      <$> count "u1" "Units with tests" defaultBuckTestsShape.u1
      <*> count "u2" "Dependency units" defaultBuckTestsShape.u2
      <*> count "mt" "Test modules per test unit" defaultBuckTestsShape.mt
      <*> count "m1" "Library modules per test unit" defaultBuckTestsShape.m1
      <*> count "m2" "Library modules per dependency unit" defaultBuckTestsShape.m2)
  where
    count name description def = option auto (long name <> metavar "NUM" <> help description <> value def)

genCommandParser :: Parser GenCommand
genCommandParser =
  hsubparser
    ( command "deep" (info deepParser (progDesc "Binary tree of modules, 2*depth units (default)"))
    <> command "wide" (info wideParser (progDesc "Binary tree of units, 2^depth-1 units"))
    <> command "flat" (info flatParser (progDesc "Single unit, module 0 imports all others"))
    <> command "tests" (info testsParser (progDesc "Chain of units, each with library modules and test modules defining main"))
    <> command "buck-tests" (info buckTestsParser (progDesc "Buck project with haskell_eval test targets for the worker"))
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
    Just dir -> pure (Just ExtDepsConfig {extDepsDir = toOsPath dir, extDepIndexes = [0 .. 4]})

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
    putStrLn ("Generated project in " ++ fromOsPath dir)
    putStrLn ("  depth:   " ++ show depth ++ " (" ++ show levels ++ " levels)")
    putStrLn ("  units:   " ++ show units)
    putStrLn ("  modules: " ++ show modules)
    putStrLn ("  ext deps: " ++ extDepsInfo extDeps)

  Wide {dir, depth, modsPerUnit} -> do
    createDirectoryIfMissing True dir
    writeWideProject dir depth modsPerUnit extDeps
    let units = wideUnitCount depth
    putStrLn ("Generated wide project in " ++ fromOsPath dir)
    putStrLn ("  depth:          " ++ show depth)
    putStrLn ("  units:          " ++ show units)
    putStrLn ("  modules/unit:   " ++ show modsPerUnit)
    putStrLn ("  total modules:  " ++ show (units * modsPerUnit))
    putStrLn ("  ext deps: " ++ extDepsInfo extDeps)

  Flat {dir, numModules} -> do
    createDirectoryIfMissing True dir
    writeFlatProject dir numModules extDeps
    putStrLn ("Generated flat project in " ++ fromOsPath dir)
    putStrLn ("  units:          1")
    putStrLn ("  total modules:  " ++ show numModules)
    putStrLn ("  ext deps: " ++ extDepsInfo extDeps)

  Production {dir, depth, bigMods} -> do
    createDirectoryIfMissing True dir
    writeProductionProject dir depth bigMods extDeps
    let units = productionUnitCount depth
        treeUnits = units - 1
        bigUnitMods = 20 * bigMods + 1
    putStrLn ("Generated production project in " ++ fromOsPath dir)
    putStrLn ("  tree depth:     " ++ show depth)
    putStrLn ("  tree units:     " ++ show treeUnits ++ " (3 modules each)")
    putStrLn ("  big unit mods:  " ++ show bigUnitMods ++ " (20 levels x " ++ show bigMods ++ " + BigMain)")
    putStrLn ("  total units:    " ++ show units)
    putStrLn ("  total modules:  " ++ show (treeUnits * 3 + bigUnitMods))
    putStrLn ("  ext deps: " ++ extDepsInfo extDeps)

  Tests {dir, shape}
    | shape.units < 1 || shape.tests < 0 || shape.modules <= shape.tests ->
      die "tests: need UNITS >= 1, TESTS >= 0 and MODULES > TESTS"
    | otherwise -> do
      createDirectoryIfMissing True dir
      writeTestsProject dir shape extDeps
      putStrLn ("Generated tests project in " ++ fromOsPath dir)
      putStrLn ("  units:          " ++ show shape.units)
      putStrLn ("  modules/unit:   " ++ show shape.modules ++ " (" ++ show shape.tests ++ " tests)")
      putStrLn ("  total modules:  " ++ show (shape.units * shape.modules))
      putStrLn ("  total tests:    " ++ show (shape.units * shape.tests))
      putStrLn ("  ext deps: " ++ extDepsInfo extDeps)

  BuckTests {dir, buckShape} -> do
    writeBuckTestsProject dir buckShape
    putStrLn ("Generated Buck tests project in " ++ fromOsPath dir)
    putStrLn ("  test units:     " ++ show buckShape.u1)
    putStrLn ("  dep units:      " ++ show buckShape.u2)
    putStrLn ("  total tests:    " ++ show (buckShape.u1 * buckShape.mt))

extDepsInfo :: Maybe ExtDepsConfig -> String
extDepsInfo =
  maybe "none" (\ c -> show (length c.extDepIndexes))
