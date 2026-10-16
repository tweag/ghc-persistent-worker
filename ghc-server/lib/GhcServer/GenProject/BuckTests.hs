-- | Generate a Buck project of @haskell_eval@ tests for exercising the worker's eval mode.
--
-- The project is flat (all sources and the @BUCK@ file live in the root directory) and consists of:
--
-- * @u1@ test units, each a library of @m1@ modules plus @mt@ test modules, each a @haskell_eval@ target.
-- * @u2@ dependency units, each a library of @m2@ modules.
-- * A shared test runner module @Runner@ in the first dependency unit, which allocates a megabyte of
--   'String's per iteration (see 'runnerSource').
--
-- All modules are padded with large literal tables and functions so that their bytecode is sizable.
module GhcServer.GenProject.BuckTests where

import Data.List (intercalate)
import System.Directory.OsPath (createDirectoryIfMissing)
import qualified System.File.OsPath as OsPath
import System.OsPath (OsPath, osp, (<.>), (</>))
import System.OsPath.Extra (toOsPath)
import Types.ByteString (Utf8 (toUtf8Lazy))

-- | Shape of a Buck tests project.
data BuckTestsShape =
  BuckTestsShape {
    -- | Number of units containing tests.
    u1 :: Int,
    -- | Number of dependency units.
    u2 :: Int,
    -- | Test modules per test unit.
    mt :: Int,
    -- | Library modules per test unit.
    m1 :: Int,
    -- | Library modules per dependency unit.
    m2 :: Int
  }
  deriving stock (Show)

-- | The shape used when no options are given.
defaultBuckTestsShape :: BuckTestsShape
defaultBuckTestsShape = BuckTestsShape {u1 = 4, u2 = 4, mt = 4, m1 = 1, m2 = 1}

m1Module :: Int -> Int -> String
m1Module u m = "U1Unit" ++ show u ++ "Mod" ++ show m

m2Module :: Int -> Int -> String
m2Module u m = "U2Unit" ++ show u ++ "Mod" ++ show m

testModule :: Int -> Int -> String
testModule u t = "U1Unit" ++ show u ++ "Test" ++ show t

runnerModule :: String
runnerModule = "Runner"

-- Target names must not end in a number component (@-1@): the package name derived from them would be rejected by
-- @ghc-pkg@.
u1Library :: Int -> String
u1Library u = "u1_lib" ++ show u

u2Library :: Int -> String
u2Library u = "u2_lib" ++ show u

testTarget :: Int -> Int -> String
testTarget u t = "u1_unit" ++ show u ++ "_test" ++ show t

-- | Number of entries in the literal table of each module.
fatEntries :: Int
fatEntries = 600

-- | Number of generated functions in each module.
fatFunctions :: Int
fatFunctions = 120

-- | Source of a module with a large table and many functions, exporting @value :: Int@ which combines the
-- table, the functions and the values of the @imports@.
fatBody :: String -> [String] -> [String]
fatBody name imports =
  ["", "table :: [(Int, String)]", "table ="]
  ++ ["  [ " ++ intercalate "\n  , " [show (i, "entry " ++ name ++ " " ++ show i) | i <- [1 .. fatEntries]], "  ]", ""]
  ++ concatMap function ([0 .. fatFunctions - 1] :: [Int])
  ++ [
    "value :: Int",
    "value =",
    "  sum (fst <$> table) + sum (length . snd <$> table)",
    "  + sum [" ++ intercalate ", " [fname i ++ " " ++ show i | i <- [0 .. fatFunctions - 1]] ++ "]"
  ]
  ++ ["  + " ++ i ++ ".value" | i <- imports]
  where
    fname i = "fun" ++ show i
    function i =
      [
        fname i ++ " :: Int -> Int",
        fname i ++ " x = case x `mod` 8 of"
      ]
      ++ ["  " ++ show k ++ " -> x * " ++ show (i + k + 2) ++ " + " ++ show (i * k) | k <- [0 .. 6 :: Int]]
      ++ ["  _ -> x - " ++ show i, ""]

-- | Source of a library module.
librarySource :: String -> String
librarySource name =
  unlines (["module " ++ name ++ " where"] ++ fatBody name [])

-- | Source of a test module, whose @test@ prints a value computed from all of its dependencies and then runs the
-- shared test runner.
testSource :: String -> [String] -> String
testSource name deps =
  unlines $
    ["module " ++ name ++ " where", "", "import qualified " ++ runnerModule]
    ++ ["import qualified " ++ dep | dep <- deps]
    ++ fatBody name []
    ++ [
      "",
      "test :: IO String",
      "test = do",
      "  putStrLn (" ++ show (name ++ " dependencies: ") ++ " ++ show (sum [" ++ intercalate ", " (["value"] ++ [dep ++ ".value" | dep <- deps]) ++ "]))",
      "  " ++ runnerModule ++ ".runTest " ++ show name
    ]

-- | Source of the shared test runner.
-- Each iteration allocates one megabyte of 'String' (a cons cell is three words), reports it and sleeps.
runnerSource :: String
runnerSource =
  unlines [
    "module " ++ runnerModule ++ " where",
    "",
    "import Control.Concurrent (threadDelay)",
    "import Control.Exception (evaluate)",
    "import Control.Monad (forM_)",
    "import System.Environment (lookupEnv)",
    "import System.Random (newStdGen, randomRs)",
    "import Text.Read (readMaybe)",
    "",
    "iterations :: IO Int",
    "iterations = maybe 1 id . (>>= readMaybe) <$> lookupEnv \"test_iterations\"",
    "",
    "-- | One megabyte, with 24 bytes per list cell.",
    "stringLength :: Int",
    "stringLength = 1024 * 1024 `div` 24",
    "",
    "runTest :: String -> IO String",
    "runTest name = do",
    "  count <- iterations",
    "  forM_ [1 .. count] $ \\ i -> do",
    "    gen <- newStdGen",
    "    let chars = take stringLength (randomRs ('a', 'z') gen)",
    "    _ <- evaluate (length chars)",
    "    putStrLn (\"Test \" ++ name ++ \" iteration \" ++ show i)",
    "    threadDelay 100000",
    "    evaluate (length chars) >>= \\ n -> if n == stringLength then pure () else fail \"corrupt string\"",
    "  pure (\"Test \" ++ name ++ \" succeeded after \" ++ show count ++ \" iterations\")"
  ]

-- | A Buck attribute value.
data Attr =
  List [String]
  |
  Str String
  |
  Dict [(String, String)]

-- | A Buck target declaration.
target :: String -> String -> [(String, Attr)] -> String
target rule name attrs =
  unlines $
    [rule ++ "(", "    name = " ++ show name ++ ","]
    ++ concatMap attr attrs
    ++ [")", ""]
  where
    attr (key, List values) =
      ["    " ++ key ++ " = ["] ++ ["        " ++ show v ++ "," | v <- values] ++ ["    ],"]
    attr (key, Str value) =
      ["    " ++ key ++ " = " ++ show value ++ ","]
    attr (key, Dict entries) =
      ["    " ++ key ++ " = {"] ++ ["        " ++ show k ++ ": " ++ show v ++ "," | (k, v) <- entries] ++ ["    },"]

-- | The library of a dependency unit; the first one also contains the runner.
u2Target :: BuckTestsShape -> Int -> String
u2Target shape u =
  target "haskell_lib" (u2Library u) [
    ("srcs", List [m ++ ".hs" | m <- mods]),
    ("deps", List (["//haskell:base"] ++ ["//haskell:random" | u == 1]))
  ]
  where
    mods = [m2Module u m | m <- [1 .. shape.m2]] ++ [runnerModule | u == 1]

u1Target :: BuckTestsShape -> Int -> String
u1Target shape u =
  target "haskell_lib" (u1Library u) [
    ("srcs", List [m1Module u m ++ ".hs" | m <- [1 .. shape.m1]]),
    ("deps", List ["//haskell:base"])
  ]

-- | Names of the modules a test of unit @u@ depends on: all of its unit's library modules and all dependency units'.
testDependencies :: BuckTestsShape -> Int -> [String]
testDependencies shape u =
  [m1Module u m | m <- [1 .. shape.m1]] ++ [m2Module d m | d <- [1 .. shape.u2], m <- [1 .. shape.m2]]

evalTarget :: BuckTestsShape -> Int -> Int -> String
evalTarget shape u t =
  target "haskell_eval" (testTarget u t) [
    ("srcs", List [testModule u t ++ ".hs"]),
    ("modname", Str (testModule u t)),
    ("expr", Str "test"),
    ("executor", Str (u1Library u)),
    ("env", Dict [("test_iterations", show u)]),
    ("deps", List (["//haskell:base", ":" ++ u1Library u] ++ [":" ++ u2Library d | d <- [1 .. shape.u2]]))
  ]

-- | The @BUCK@ file declaring all libraries and tests.
buckFile :: BuckTestsShape -> String
buckFile shape =
  unlines ["load(\"//haskell:custom.bzl\", \"haskell_eval\", \"haskell_lib\")", ""]
  ++ concat [u2Target shape u | u <- [1 .. shape.u2]]
  ++ concat [u1Target shape u | u <- [1 .. shape.u1]]
  ++ concat [evalTarget shape u t | u <- [1 .. shape.u1], t <- [1 .. shape.mt]]

-- | Write the project to disk.
writeBuckTestsProject :: OsPath -> BuckTestsShape -> IO ()
writeBuckTestsProject root shape = do
  createDirectoryIfMissing True root
  OsPath.writeFile (root </> [osp|BUCK|]) (toUtf8Lazy (buckFile shape))
  write runnerModule runnerSource
  mapM_ (\ (name, deps) -> write name (testSource name deps)) tests
  mapM_ (\ name -> write name (librarySource name)) libraries
  where
    write name = OsPath.writeFile (root </> toOsPath name <.> [osp|hs|]) . toUtf8Lazy
    libraries = [m1Module u m | u <- [1 .. shape.u1], m <- [1 .. shape.m1]]
      ++ [m2Module u m | u <- [1 .. shape.u2], m <- [1 .. shape.m2]]
    tests = [(testModule u t, testDependencies shape u) | u <- [1 .. shape.u1], t <- [1 .. shape.mt]]
