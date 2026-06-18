-- | Haskell implementation of @compare-features@ and @profile-production@.
--
-- Runs all 16 permutations of 'FeatureFlag' configurations against a production-topology project,
-- measuring total time and allocation from GHC profiling output.
module GhcServer.CompareFeatures (
  main_compareFeatures,
) where

import Control.Concurrent (threadDelay)
import Control.Exception (finally)
import Control.Monad (unless, when)
import Data.Char (isDigit, isSpace)
import Data.Foldable (traverse_)
import Data.List (intercalate, isPrefixOf, isSuffixOf, sort, sortOn, subsequences)
import GhcServer.GenProject (ExtDepsConfig (..), writeProductionProject)
import Options.Applicative (
  Parser,
  ParserInfo,
  auto,
  execParser,
  fullDesc,
  header,
  help,
  helper,
  info,
  long,
  metavar,
  option,
  progDesc,
  short,
  strOption,
  value,
  (<**>),
  )
import System.Directory (
  copyFile,
  createDirectoryIfMissing,
  doesDirectoryExist,
  doesFileExist,
  getTemporaryDirectory,
  listDirectory,
  removeDirectoryRecursive,
  removeFile,
  )
import System.Environment (getEnvironment, lookupEnv)
import System.Exit (ExitCode (..), exitFailure)
import System.FilePath (takeDirectory, (</>))
import System.IO (IOMode (..), hFlush, hPutStrLn, openFile, stderr)
import System.Process (
  CreateProcess (..),
  ProcessHandle,
  StdStream (..),
  createProcess,
  getProcessExitCode,
  interruptProcessGroupOf,
  proc,
  readProcess,
  waitForProcess,
  )
import Types.FeatureFlags (FeatureFlag (..), FeatureFlags (..))

-- ---------------------------------------------------------------------------
-- Configuration
-- ---------------------------------------------------------------------------

-- | CLI configuration for the compare-features executable.
data CompareConfig =
  CompareConfig {
    depth :: Int,
    bigMods :: Int,
    serverApp :: String,
    clientApp :: String
  }
  deriving stock (Show)

configParser :: Parser CompareConfig
configParser =
  CompareConfig
    <$> option auto (long "depth" <> short 'd' <> metavar "N" <> help "Tree depth" <> value 8)
    <*> option auto (long "big-mods" <> short 'm' <> metavar "N"
          <> help "Modules per level in big unit" <> value 50)
    <*> strOption (long "server-app" <> metavar "APP"
          <> help "Nix flake app for the profiled server"
          <> value "env.profiled-linkables.ghc-server")
    <*> strOption (long "client-app" <> metavar "APP"
          <> help "Nix flake app for the client"
          <> value "env.profiled-linkables.ghc-client")

configParserInfo :: ParserInfo CompareConfig
configParserInfo =
  info (configParser <**> helper)
    (fullDesc
    <> progDesc "Compare all feature flag permutations via profiling"
    <> header "compare-features - feature flag benchmark")

-- ---------------------------------------------------------------------------
-- Results
-- ---------------------------------------------------------------------------

-- | Profiling result for one configuration.
data BenchResult =
  BenchResult {
    features :: FeatureFlags,
    totalTimeSecs :: Double,
    totalAllocMB :: Integer
  }
  deriving stock (Show)

-- ---------------------------------------------------------------------------
-- Feature flag permutations
-- ---------------------------------------------------------------------------

-- | All feature flag constructors.
allFlags :: [FeatureFlag]
allFlags =
  [FeatureFixedNodesCache, FeatureFlagParser, FeatureConcurrentInitUnits, FeatureIncrementalBuildPlan]

-- | All 16 permutations (power set) of feature flags.
allPermutations :: [FeatureFlags]
allPermutations =
  map flagsFromList (subsequences allFlags)

-- | Build 'FeatureFlags' from a list of enabled flags (rest disabled).
flagsFromList :: [FeatureFlag] -> FeatureFlags
flagsFromList enabled =
  FeatureFlags {
    fixedNodesCache = FeatureFixedNodesCache `elem` enabled,
    flagParser = FeatureFlagParser `elem` enabled,
    concurrentInitUnits = FeatureConcurrentInitUnits `elem` enabled,
    incrementalBuildPlan = FeatureIncrementalBuildPlan `elem` enabled,
    instrument = False,
    lazyByteCode = False,
    lazyByteCodeCacheLimit = Nothing
  }

-- | Convert 'FeatureFlags' to CLI args for the server.
featureFlagsToArgs :: FeatureFlags -> [String]
featureFlagsToArgs flags =
  concatMap flagArg allFlags
  where
    flagArg flag =
      [prefix, name]
      where
        enabled = case flag of
          FeatureFixedNodesCache -> flags.fixedNodesCache
          FeatureFlagParser -> flags.flagParser
          FeatureConcurrentInitUnits -> flags.concurrentInitUnits
          FeatureIncrementalBuildPlan -> flags.incrementalBuildPlan

        prefix = if enabled then "--enable" else "--disable"

        name = case flag of
          FeatureFixedNodesCache -> "fixed-nodes-cache"
          FeatureFlagParser -> "flag-parser"
          FeatureConcurrentInitUnits -> "concurrent-init-units"
          FeatureIncrementalBuildPlan -> "incremental-build-plan"

-- | Short label for a feature config.
featureLabel :: FeatureFlags -> String
featureLabel flags
  | null active = "(none)"
  | otherwise = intercalate "+" active
  where
    active =
      [abbrev | (abbrev, test) <- flagAbbrevs, test flags]

    flagAbbrevs =
      [
        ("FNC", (.fixedNodesCache)),
        ("FP", (.flagParser)),
        ("CIU", (.concurrentInitUnits)),
        ("IBP", (.incrementalBuildPlan))
      ]

-- ---------------------------------------------------------------------------
-- Source hash computation
-- ---------------------------------------------------------------------------

-- | Compute @buck_source_hashes@ JSON for all @.hs@ files in a unit directory.
-- Writes the JSON file in the parent directory and returns its path.
computeSourceHashes :: FilePath -> IO FilePath
computeSourceHashes unitDir = do
  files <- listDirectory unitDir
  let hsFiles = sort [unitDir </> f | f <- files, ".hs" `isSuffixOf` f]
  digests <- traverse mkDigest hsFiles
  let json = "{\n  \"version\": 1,\n  \"digests\": [\n"
              ++ intercalate ",\n" digests
              ++ "\n  ]\n}"
  writeFile metadataFile json
  pure metadataFile
  where
    metadataFile = takeDirectory unitDir </> "source_hashes.json"

    mkDigest path = do
      sha <- computeSha1 path
      size <- readFileSize path
      pure $ "    {\"path\": \"" ++ path ++ "\", \"digest\": \"" ++ sha ++ ":" ++ show size ++ "\"}"

computeSha1 :: FilePath -> IO String
computeSha1 path = do
  out <- readProcess "sha1sum" [path] ""
  pure (takeWhile (not . isSpace) out)

readFileSize :: FilePath -> IO Integer
readFileSize path = do
  out <- readProcess "stat" ["-c%s", path] ""
  pure (read (filter isDigit out))

-- ---------------------------------------------------------------------------
-- Profile file parsing
-- ---------------------------------------------------------------------------

-- | Parse a GHC @.prof@ file for total time and total allocation.
parseProfFile :: FilePath -> IO (Maybe (Double, Integer))
parseProfFile path =
  doesFileExist path >>= \case
    False -> pure Nothing
    True -> do
      contents <- readFile path
      let ls = lines contents
      pure do
        t <- parseTotalTime ls
        a <- parseTotalAlloc ls
        pure (t, a)

-- | Extract total time from: @total time  =        0.18 secs   (...)@
parseTotalTime :: [String] -> Maybe Double
parseTotalTime [] = Nothing
parseTotalTime (l : rest)
  | "total time" `isPrefixOf` dropWhile isSpace l =
      case break (== '=') l of
        (_, _ : after) -> case reads numStr of
          [(v, _)] -> Just v
          _ -> parseTotalTime rest
          where
            numStr = filter (\ c -> isDigit c || c == '.') (takeWhile (/= 's') after)
        _ -> parseTotalTime rest
  | otherwise = parseTotalTime rest

-- | Extract total alloc from: @total alloc = 316,291,528 bytes  (...)@
parseTotalAlloc :: [String] -> Maybe Integer
parseTotalAlloc [] = Nothing
parseTotalAlloc (l : rest)
  | "total alloc" `isPrefixOf` dropWhile isSpace l =
      case break (== '=') l of
        (_, _ : after) -> case reads numStr of
          [(v, _)] -> Just v
          _ -> parseTotalAlloc rest
          where
            numStr = filter isDigit (takeWhile (/= 'b') after)
        _ -> parseTotalAlloc rest
  | otherwise = parseTotalAlloc rest

-- ---------------------------------------------------------------------------
-- Subprocess management
-- ---------------------------------------------------------------------------

-- | Start a server process via @nix run@. Returns the process handle.
-- Uses @-po@ RTS option to direct the @.prof@ file to @projectDir@.
-- Waits for the server socket to appear before returning.
startServer :: String -> [String] -> FilePath -> [(String, String)] -> IO ProcessHandle
startServer app featureArgs projectDir envList = do
  devNull <- openFile "/dev/null" WriteMode
  logH <- openFile (projectDir </> "server.log") WriteMode
  let profPrefix = projectDir </> "ghc-server"
      nixArgs = ["run", ".#" ++ app, "--"] ++ featureArgs ++ [projectDir, "+RTS", "-po" ++ profPrefix, "-p", "-RTS"]
      cp = (proc "nix" nixArgs) {
        std_out = UseHandle devNull,
        std_err = UseHandle logH,
        create_group = True,
        env = Just envList
      }
  (_, _, _, ph) <- createProcess cp
  waitForSocket (projectDir </> "socket" </> "server.sock") ph
  pure ph

-- | Poll for a socket file to appear, up to a timeout.
-- Also checks whether the server process exited early (indicating a build or startup failure).
waitForSocket :: FilePath -> ProcessHandle -> IO ()
waitForSocket sock ph =
  go (300 :: Int)
  where
    go 0 =
      hPutStrLn stderr ("  Timeout waiting for server socket: " ++ sock)
    go n = do
      getProcessExitCode ph >>= \case
        Just ec -> hPutStrLn stderr ("  Server exited early: " ++ show ec)
        Nothing ->
          doesFileExist sock >>= \case
            True -> pure ()
            False -> do
              threadDelay 500_000
              go (n - 1)

-- | Stop a server process by sending SIGINT to its process group.
stopServer :: ProcessHandle -> IO ()
stopServer ph = do
  interruptProcessGroupOf ph
  _ <- waitForProcess ph
  pure ()

-- | Run a client command via @nix run@ and wait for completion.
runClient :: String -> [String] -> [(String, String)] -> IO ExitCode
runClient app args envList = do
  devNull <- openFile "/dev/null" WriteMode
  let nixArgs = ["run", ".#" ++ app, "--"] ++ args
      cp = (proc "nix" nixArgs) {
        std_out = UseHandle devNull,
        std_err = UseHandle devNull,
        env = Just envList
      }
  (_, _, _, ph) <- createProcess cp
  waitForProcess ph

-- | Run a server for the duration of an action, stopping it afterward.
withServer :: String -> [String] -> FilePath -> [(String, String)] -> IO a -> IO a
withServer app featureArgs projectDir envList action = do
  serverPh <- startServer app featureArgs projectDir envList
  action `finally` stopServer serverPh

-- ---------------------------------------------------------------------------
-- File utilities
-- ---------------------------------------------------------------------------

-- | Copy a directory tree recursively.
copyDirRecursive :: FilePath -> FilePath -> IO ()
copyDirRecursive src dst = do
  createDirectoryIfMissing True dst
  entries <- listDirectory src
  traverse_ copyEntry entries
  where
    copyEntry entry = do
      let srcPath = src </> entry
          dstPath = dst </> entry
      doesDirectoryExist srcPath >>= \case
        True -> copyDirRecursive srcPath dstPath
        False -> copyFile srcPath dstPath

removeIfExists :: FilePath -> IO ()
removeIfExists path =
  doesFileExist path >>= \case
    True -> removeFile path
    False -> pure ()

removeIfExistsDir :: FilePath -> IO ()
removeIfExistsDir path =
  doesDirectoryExist path >>= \case
    True -> removeDirectoryRecursive path
    False -> pure ()

-- ---------------------------------------------------------------------------
-- Single profiling run
-- ---------------------------------------------------------------------------

-- | Modify BigMain to trigger incremental metadata.
modifyBigMain :: FilePath -> IO ()
modifyBigMain projectDir =
  appendFile (projectDir </> "unitbig" </> "BigMain.hs") "\nnew_binding :: Int\nnew_binding = 42\n"

-- | Delete unitbig cache, socket dir, and prof file to prepare for profiled rebuild.
cleanForRebuild :: FilePath -> IO ()
cleanForRebuild projectDir = do
  removeIfExists (projectDir </> "cache" </> "unitbig" </> "cached_unit.json")
  removeIfExistsDir (projectDir </> "socket")
  removeIfExists (projectDir </> "ghc-server.prof")

-- | Run one profiling cycle for a given feature configuration.
profileRun :: CompareConfig -> FilePath -> FeatureFlags -> IO (Maybe BenchResult)
profileRun config templateDir flags = do
  tmpBase <- getTemporaryDirectory
  let runDir = tmpBase </> "compare-features-run"
  removeIfExistsDir runDir
  copyDirRecursive templateDir runDir
  flip finally (removeIfExistsDir runDir) (profileRunIn config flags runDir)

-- | The profiling cycle body, given a prepared run directory.
profileRunIn :: CompareConfig -> FeatureFlags -> FilePath -> IO (Maybe BenchResult)
profileRunIn config flags runDir = do
  let unitbigDir = runDir </> "unitbig"
      featureArgs = featureFlagsToArgs flags

  baseEnv <- getEnvironment
  metadataFile <- computeSourceHashes unitbigDir
  let envList = ("buck_source_hashes", metadataFile) : baseEnv

  -- Initial metadata build (unprofiled run to populate cache)
  initialOk <- withServer config.serverApp featureArgs runDir envList do
    ec <- runClient config.clientApp [runDir, "--wait", "unitbig:metadata"] envList
    pure (ec == ExitSuccess)

  unless initialOk do
    serverLog <- readFile (runDir </> "server.log")
    hPutStrLn stderr ("  Initial metadata failed for " ++ featureLabel flags)
    hPutStrLn stderr ("  Server log: " ++ serverLog)

  -- Modify and prepare for profiled rebuild
  modifyBigMain runDir
  metadataFile2 <- computeSourceHashes unitbigDir
  let envList2 = ("buck_source_hashes", metadataFile2) : baseEnv
  cleanForRebuild runDir

  -- Profiled metadata rebuild (the measured run)
  rebuildOk <- withServer config.serverApp featureArgs runDir envList2 do
    ec <- runClient config.clientApp [runDir, "--wait", "unitbig:metadata"] envList2
    pure (ec == ExitSuccess)

  unless rebuildOk do
    hPutStrLn stderr ("  Profiled rebuild failed for " ++ featureLabel flags)

  -- Parse results
  parseProfFile (runDir </> "ghc-server.prof") >>= \case
    Just (time, allocBytes) ->
      pure (Just BenchResult {
        features = flags,
        totalTimeSecs = time,
        totalAllocMB = div allocBytes 1_048_576
      })
    Nothing -> do
      hPutStrLn stderr ("  No prof output for " ++ featureLabel flags)
      pure Nothing

-- ---------------------------------------------------------------------------
-- Reporting
-- ---------------------------------------------------------------------------

-- | Print comparison table of results sorted by total time.
printResults :: [BenchResult] -> IO ()
printResults results = do
  putStrLn ""
  putStrLn "=== Feature Comparison Results ==="
  putStrLn ""
  putStrLn (padRight 24 "Configuration" ++ padLeft 12 "Time (s)" ++ padLeft 12 "Alloc (MB)")
  putStrLn (replicate 24 '-' ++ " " ++ replicate 11 '-' ++ " " ++ replicate 11 '-')
  traverse_ printRow sorted
  putStrLn ""
  printSummary sorted
  where
    sorted = sortOn (.totalTimeSecs) results

    printRow r =
      putStrLn (padRight 24 (featureLabel r.features)
                ++ padLeft 12 (showTime r.totalTimeSecs)
                ++ padLeft 12 (show r.totalAllocMB))

printSummary :: [BenchResult] -> IO ()
printSummary [] = pure ()
printSummary sorted@(best : _) = do
  putStrLn ("Fastest: " ++ featureLabel best.features ++ " (" ++ showTime best.totalTimeSecs ++ "s)")
  putStrLn ("Slowest: " ++ featureLabel worst.features ++ " (" ++ showTime worst.totalTimeSecs ++ "s)")
  when (best.totalTimeSecs > 0) do
    let ratio = worst.totalTimeSecs / best.totalTimeSecs
    putStrLn ("Ratio:   " ++ showTime ratio ++ "x")
  where
    worst = last sorted

padRight :: Int -> String -> String
padRight n s = s ++ replicate (max 0 (n - length s)) ' '

padLeft :: Int -> String -> String
padLeft n s = replicate (max 0 (n - length s)) ' ' ++ s

showTime :: Double -> String
showTime d =
  show whole ++ "." ++ fracStr
  where
    centis = round (d * 100) :: Integer
    whole = div centis 100
    frac = mod (abs centis) 100
    fracStr = (if frac < 10 then "0" else "") ++ show frac

-- ---------------------------------------------------------------------------
-- Main entry point
-- ---------------------------------------------------------------------------

-- | Entry point for the compare-features executable.
main_compareFeatures :: IO ()
main_compareFeatures = do
  config <- execParser configParserInfo
  runCompareFeatures config

runCompareFeatures :: CompareConfig -> IO ()
runCompareFeatures config = do
  hPutStrLn stderr ("Compare features: depth=" ++ show config.depth ++ ", big_mods=" ++ show config.bigMods)
  hPutStrLn stderr ("Server: nix run .#" ++ config.serverApp)
  hPutStrLn stderr ("Client: nix run .#" ++ config.clientApp)
  hPutStrLn stderr "Running 16 feature permutations..."
  hFlush stderr

  -- Generate template project
  tmpBase <- getTemporaryDirectory
  let templateDir = tmpBase </> "compare-features-template"
  removeIfExistsDir templateDir
  createDirectoryIfMissing True templateDir

  extDeps <- getExtDepsConfig
  writeProductionProject templateDir config.depth config.bigMods extDeps

  hPutStrLn stderr ("Generated production project at " ++ templateDir)
  hFlush stderr

  -- Run all permutations
  let perms = allPermutations
      total = length perms
      runPermutation (i, flags) = do
        hPutStrLn stderr ("\n[" ++ show i ++ "/" ++ show total ++ "] " ++ featureLabel flags)
        hFlush stderr
        profileRun config templateDir flags
  results <- flip finally (removeIfExistsDir templateDir) do
    traverse runPermutation (zip [1 :: Int ..] perms)

  -- Report
  let successful = [r | Just r <- results]
  if null successful
    then do
      hPutStrLn stderr "No successful runs."
      exitFailure
    else printResults successful

-- | Build an 'ExtDepsConfig' from the @resource_test_ext_deps@ environment variable.
getExtDepsConfig :: IO (Maybe ExtDepsConfig)
getExtDepsConfig =
  lookupEnv "resource_test_ext_deps" >>= \case
    Nothing -> pure Nothing
    Just dir -> pure (Just ExtDepsConfig {extDepsDir = dir, extDepIndexes = [0 .. 4]})
