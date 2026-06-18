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
import Data.List (intercalate, isPrefixOf, sort, sortOn, subsequences)
import qualified Data.Set as Set
import Data.Set (Set)
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
import System.Directory.OsPath (
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
import qualified System.File.OsPath as OsPath
import System.IO (IOMode (..), hFlush, hPutStrLn, openFile, stderr)
import System.OsPath (OsPath, isExtensionOf, osp, takeDirectory, (</>))
import System.OsPath.Extra (fromOsPath, toOsPath)
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
import Types.ByteString (Utf8 (fromUtf8Lazy), toUtf8Lazy)
import Types.FeatureFlags (Feature (..))

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
          <> value "env.profiled-fixed.ghc-server")
    <*> strOption (long "client-app" <> metavar "APP"
          <> help "Nix flake app for the client"
          <> value "env.profiled-fixed.ghc-client")

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
    features :: Set Feature,
    totalTimeSecs :: Double,
    totalAllocMB :: Integer
  }
  deriving stock (Show)

-- ---------------------------------------------------------------------------
-- Feature flag permutations
-- ---------------------------------------------------------------------------

-- | All feature flag constructors.
allFlags :: [Feature]
allFlags =
  [FeatureFixedNodesCache, FeatureFlagParser, FeatureIncrementalBuildPlan, FeatureLazyByteCode]

-- | All 16 permutations (power set) of feature flags.
allPermutations :: [Set Feature]
allPermutations =
  [Set.fromList (FeatureConcurrentInitUnits : perm) | perm <- subsequences allFlags]

-- | Convert 'FeatureFlags' to CLI args for the server.
featureFlagsToArgs :: Set Feature -> [String]
featureFlagsToArgs flags =
  concatMap flagArg allFlags
  where
    flagArg flag =
      [prefix, name]
      where
        enabled = Set.member flag flags

        prefix = if enabled then "--enable" else "--disable"

        name = case flag of
          FeatureFixedNodesCache -> "fixed-nodes-cache"
          FeatureFlagParser -> "flag-parser"
          FeatureIncrementalBuildPlan -> "incremental-build-plan"
          FeatureLazyByteCode -> "lazy-byte-code"
          FeatureConcurrentInitUnits -> "concurrent-init-units"
          FeatureInstrument -> "instrument"
          FeatureSharedMemory -> "shared-memory"
          FeatureSharedPayloads -> "shared-payloads"

-- | Short label for a feature config.
featureLabel :: Set Feature -> String
featureLabel flags
  | null active = "(none)"
  | otherwise = intercalate "+" active
  where
    active =
      [abbrev | (abbrev, flag) <- flagAbbrevs, Set.member flag flags]

    flagAbbrevs =
      [
        ("FNC", FeatureFixedNodesCache),
        ("FP", FeatureFlagParser),
        ("IBP", FeatureIncrementalBuildPlan),
        ("LBC", FeatureLazyByteCode)
      ]

-- ---------------------------------------------------------------------------
-- Source hash computation
-- ---------------------------------------------------------------------------

-- | Compute @buck_source_hashes@ JSON for all @.hs@ files in a unit directory.
-- Writes the JSON file in the parent directory and returns its path.
computeSourceHashes :: OsPath -> IO OsPath
computeSourceHashes unitDir = do
  files <- listDirectory unitDir
  let hsFiles = sort [unitDir </> f | f <- files, [osp|hs|] `isExtensionOf` f]
  digests <- traverse mkDigest hsFiles
  let json = "{\n  \"version\": 1,\n  \"digests\": [\n"
              ++ intercalate ",\n" digests
              ++ "\n  ]\n}"
  OsPath.writeFile metadataFile (toUtf8Lazy json)
  pure metadataFile
  where
    metadataFile = takeDirectory unitDir </> [osp|source_hashes.json|]

    mkDigest path = do
      sha <- computeSha1 path
      size <- readFileSize path
      pure $ "    {\"path\": \"" ++ fromOsPath path ++ "\", \"digest\": \"" ++ sha ++ ":" ++ show size ++ "\"}"

computeSha1 :: OsPath -> IO String
computeSha1 path = do
  out <- readProcess "sha1sum" [fromOsPath path] ""
  pure (takeWhile (not . isSpace) out)

readFileSize :: OsPath -> IO Integer
readFileSize path = do
  out <- readProcess "stat" ["-c%s", fromOsPath path] ""
  pure (read (filter isDigit out))

-- ---------------------------------------------------------------------------
-- Profile file parsing
-- ---------------------------------------------------------------------------

-- | Parse a GHC @.prof@ file for total time and total allocation.
parseProfFile :: OsPath -> IO (Maybe (Double, Integer))
parseProfFile path =
  doesFileExist path >>= \case
    False -> pure Nothing
    True -> do
      contents <- OsPath.readFile path
      let ls = lines (fromUtf8Lazy contents)
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
startServer :: String -> [String] -> OsPath -> [(String, String)] -> IO ProcessHandle
startServer app featureArgs projectDir envList = do
  devNull <- openFile "/dev/null" WriteMode
  logH <- OsPath.openFile (projectDir </> [osp|server.log|]) WriteMode
  let profPrefix = projectDir </> [osp|ghc-server|]
      nixArgs = ["run", ".#" ++ app, "--"] ++ featureArgs ++ [fromOsPath projectDir, "+RTS", "-po" ++ fromOsPath profPrefix, "-p", "-RTS"]
      cp = (proc "nix" nixArgs) {
        std_out = UseHandle devNull,
        std_err = UseHandle logH,
        create_group = True,
        env = Just envList
      }
  (_, _, _, ph) <- createProcess cp
  waitForSocket (projectDir </> [osp|socket|] </> [osp|server.sock|]) ph
  pure ph

-- | Poll for a socket file to appear, up to a timeout.
-- Also checks whether the server process exited early (indicating a build or startup failure).
waitForSocket :: OsPath -> ProcessHandle -> IO ()
waitForSocket sock ph =
  go (300 :: Int)
  where
    go 0 =
      hPutStrLn stderr ("  Timeout waiting for server socket: " ++ fromOsPath sock)
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
withServer :: String -> [String] -> OsPath -> [(String, String)] -> IO a -> IO a
withServer app featureArgs projectDir envList action = do
  serverPh <- startServer app featureArgs projectDir envList
  action `finally` stopServer serverPh

-- ---------------------------------------------------------------------------
-- File utilities
-- ---------------------------------------------------------------------------

-- | Copy a directory tree recursively.
copyDirRecursive :: OsPath -> OsPath -> IO ()
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

removeIfExists :: OsPath -> IO ()
removeIfExists path =
  doesFileExist path >>= \case
    True -> removeFile path
    False -> pure ()

removeIfExistsDir :: OsPath -> IO ()
removeIfExistsDir path =
  doesDirectoryExist path >>= \case
    True -> removeDirectoryRecursive path
    False -> pure ()

-- ---------------------------------------------------------------------------
-- Single profiling run
-- ---------------------------------------------------------------------------

-- | Modify BigMain to trigger incremental metadata.
modifyBigMain :: OsPath -> IO ()
modifyBigMain projectDir =
  OsPath.appendFile (projectDir </> [osp|unitbig|] </> [osp|BigMain.hs|]) "\nnew_binding :: Int\nnew_binding = 42\n"

-- | Delete unitbig cache, socket dir, and prof file to prepare for profiled rebuild.
cleanForRebuild :: OsPath -> IO ()
cleanForRebuild projectDir = do
  removeIfExists (projectDir </> [osp|cache|] </> [osp|unitbig|] </> [osp|cached_unit.json|])
  removeIfExistsDir (projectDir </> [osp|socket|])
  removeIfExists (projectDir </> [osp|ghc-server.prof|])

-- | Run one profiling cycle for a given feature configuration.
profileRun :: CompareConfig -> OsPath -> Set Feature -> IO (Maybe BenchResult)
profileRun config templateDir flags = do
  tmpBase <- getTemporaryDirectory
  let runDir = tmpBase </> [osp|compare-features-run|]
  removeIfExistsDir runDir
  copyDirRecursive templateDir runDir
  flip finally (removeIfExistsDir runDir) (profileRunIn config flags runDir)

-- | The profiling cycle body, given a prepared run directory.
profileRunIn :: CompareConfig -> Set Feature -> OsPath -> IO (Maybe BenchResult)
profileRunIn config flags runDir = do
  let unitbigDir = runDir </> [osp|unitbig|]
      featureArgs = featureFlagsToArgs flags

  baseEnv <- getEnvironment
  metadataFile <- computeSourceHashes unitbigDir
  let envList = ("buck_source_hashes", fromOsPath metadataFile) : baseEnv

  -- Initial metadata build (unprofiled run to populate cache)
  initialOk <- withServer config.serverApp featureArgs runDir envList do
    ec <- runClient config.clientApp [fromOsPath runDir, "--wait", "unitbig:metadata"] envList
    pure (ec == ExitSuccess)

  unless initialOk do
    serverLog <- OsPath.readFile (runDir </> [osp|server.log|])
    hPutStrLn stderr ("  Initial metadata failed for " ++ featureLabel flags)
    hPutStrLn stderr ("  Server log: " ++ fromUtf8Lazy serverLog)

  -- Modify and prepare for profiled rebuild
  modifyBigMain runDir
  metadataFile2 <- computeSourceHashes unitbigDir
  let envList2 = ("buck_source_hashes", fromOsPath metadataFile2) : baseEnv
  cleanForRebuild runDir

  -- Profiled metadata rebuild (the measured run)
  rebuildOk <- withServer config.serverApp featureArgs runDir envList2 do
    ec <- runClient config.clientApp [fromOsPath runDir, "--wait", "unitbig:metadata"] envList2
    pure (ec == ExitSuccess)

  unless rebuildOk do
    hPutStrLn stderr ("  Profiled rebuild failed for " ++ featureLabel flags)

  -- Parse results
  parseProfFile (runDir </> [osp|ghc-server.prof|]) >>= \case
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
  let templateDir = tmpBase </> [osp|compare-features-template|]
  removeIfExistsDir templateDir
  createDirectoryIfMissing True templateDir

  extDeps <- getExtDepsConfig
  writeProductionProject templateDir config.depth config.bigMods extDeps

  hPutStrLn stderr ("Generated production project at " ++ fromOsPath templateDir)
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
    Just dir -> pure (Just ExtDepsConfig {extDepsDir = toOsPath dir, extDepIndexes = [0 .. 4]})
