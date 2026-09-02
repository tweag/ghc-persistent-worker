module Test.Source where

import Data.ByteString.Lazy (ByteString)
import Data.Foldable (for_, toList)
import Data.List (intercalate)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Text.Lazy as Text
import Data.Text.Lazy.Encoding (encodeUtf8)
import System.Directory.OsPath (createDirectoryIfMissing)
import qualified System.File.OsPath as OsPath
import System.OsPath.Extra (OsPath, (</>))
import Test.Data.Project (ErrorVariant (..), ModuleKey (..), ModuleSource (..))
import Test.Data.SourceMode (SourceMode (..))
import Test.Path (
  extDepModuleName,
  extDepValueName,
  indexedValueName,
  moduleName,
  moduleSourcePath,
  moduleValueName,
  testFunctionName,
  unitDir,
  )

sumExpr :: String -> [String] -> String
sumExpr base = \case
  [] -> base
  names -> intercalate " + " (base : names)

-- | The expression for one value binding in the generated module.
--
-- When TH is enabled, values imported from dependencies are wrapped in a splice to trigger bytecode linking.
valueExpr :: Bool -> String -> [String] -> String
valueExpr useTh base depValues =
  sumExpr base (wrapValue <$> depValues)
  where
    wrapValue v
      | useTh = "$(lift @_ @Int " ++ v ++ ")"
      | otherwise = v

-- | Write a source file for a module according to specifications.
--
-- Each module exports @bindings@ value bindings whose expressions sum the corresponding values imported from the
-- dependencies, ensuring imports are actually used and type-checked by GHC.
-- The primary binding is @value_X_Y@; additional bindings are @value_X_Y_1@, @value_X_Y_2@, etc.
moduleSource :: ModuleSource -> SourceMode -> ModuleKey -> ByteString
moduleSource ModuleSource {..} mode key =
  encodeUtf8 $
  Text.pack $
  unlines $
    thPragma
    ++ ["module " ++ headerName ++ " where", ""]
    ++ thImport
    ++ ["import " ++ moduleName d | d <- deps]
    ++ ["import " ++ extDepModuleName i | i <- toList extDeps]
    ++ concatMap valueBinding (enumFromTo 0 (bindings - 1))
  where
    (thPragma, thImport)
      | th = (["{-# LANGUAGE TemplateHaskell #-}"], ["import Language.Haskell.TH.Syntax (lift)"])
      | otherwise = ([], [])

    headerName = case mode of
      SourceFixed -> moduleName (key {errorVariant = Nothing})
      _ -> moduleName key

    base = case mode of
      SourceNormal -> maybe "1" errorBase key.errorVariant
      SourceModified -> "100"
      SourceFixed -> "1"

    errorBase = \case
      UndefinedVariable -> "x"
      TypeMismatch -> "True"

    allDepValues = (moduleValueName <$> deps) ++ (extDepValueName <$> toList extDeps)

    valueBinding i =
      [valName i ++ " :: Int", valName i ++ " = " ++ valueExpr th base allDepValues]

    valName i
      | i == 0 = moduleValueName key
      | otherwise = indexedValueName key i

-- | Write source files for all modules.
writeProjectSources :: OsPath -> Map ModuleKey ModuleSource -> IO ()
writeProjectSources srcDir modules =
  for_ (Map.toList modules) \ (key, ms) -> do
    createDirectoryIfMissing True (srcDir </> unitDir key.unit)
    OsPath.writeFile (srcDir </> moduleSourcePath key) (moduleSource ms SourceNormal key)

-- | Like 'moduleSource', but appends a top-level @(Int, Int)@ binding named via 'testFunctionName', of the shape
-- @(total, failed)@ expected by 'Internal.Evaluate.evaluate'. Forcing the binding also forces the module's primary
-- value binding via 'seq', so evaluating it exercises the same code path an ordinary value import would.
-- Used for leaf/test modules whose entry point is invoked directly via the worker's eval mode rather than only
-- imported by other modules.
testModuleSource :: ModuleSource -> ModuleKey -> ByteString
testModuleSource ms key =
  moduleSource ms SourceNormal key <> encodeUtf8 (Text.pack extra)
  where
    extra =
      unlines [
        "",
        testFunctionName key ++ " :: (Int, Int)",
        testFunctionName key ++ " = " ++ moduleValueName key ++ " `seq` (1, 0)"
        ]

-- | Write source files for all test/leaf modules, using 'testModuleSource' instead of 'moduleSource'.
writeTestModuleSources :: OsPath -> Map ModuleKey ModuleSource -> IO ()
writeTestModuleSources srcDir modules =
  for_ (Map.toList modules) \ (key, ms) -> do
    createDirectoryIfMissing True (srcDir </> unitDir key.unit)
    OsPath.writeFile (srcDir </> moduleSourcePath key) (testModuleSource ms key)

-- | Generate source for a "fat" module whose sole purpose is to produce a large aggregate bytecode footprint.
--
-- Each of the @numFunctions@ fat functions is @'Int' -> 'Int'@ with a @case@ expression of @caseArms@ integer
-- alternatives; each alternative is a distinct compile-time constant, so GHC cannot collapse them even at @-O0@,
-- and the instruction array grows with @caseArms@.
--
-- Empirically (see @compare-eviction@ experiments), the dominant lever on freed heap per evicted module is
-- @numFunctions@ (the count of top-level bindings), not @caseArms@: holding @numFunctions * caseArms@ constant,
-- splitting the same total case-alternative budget across more functions with fewer arms each frees substantially
-- more heap per unit of compile time than concentrating it into fewer, larger functions.
--
-- The module exports exactly one 'Int' binding (the standard 'moduleValueName' for @key@), whose value is
-- @fat_fun_0 0@; this preserves type compatibility with test modules that import and use the value in arithmetic.
-- The fat functions themselves are top-level bindings compiled to separate BCOs and counted by the bytecode
-- cache tracker, but they are not imported by any other module.
fatModuleSource :: Int -> Int -> ModuleKey -> ByteString
fatModuleSource numFunctions caseArms key =
  encodeUtf8 $
  Text.pack $
  unlines $
    [ "module " ++ moduleName key ++ " where"
    , ""
    , moduleValueName key ++ " :: Int"
    , moduleValueName key ++ " = fat_fun_0 0"
    , ""
    ]
    ++ concatMap (fatFun caseArms) ([0 .. numFunctions - 1] :: [Int])
  where
    fatFun :: Int -> Int -> [String]
    fatFun n i =
      [ "fat_fun_" ++ show i ++ " :: Int -> Int"
      , "fat_fun_" ++ show i ++ " x = case x of"
      ]
      ++ ["  " ++ show j ++ " -> " ++ show (j * numFunctions + i + 1) | j <- [0 .. n - 1 :: Int]]
      ++ ["  _ -> x + " ++ show i, ""]

-- | Write a fat module source file for the bytecode eviction benchmark. See 'fatModuleSource'.
writeFatModuleSource :: OsPath -> Int -> Int -> ModuleKey -> IO ()
writeFatModuleSource srcDir numFunctions caseArms key = do
  createDirectoryIfMissing True (srcDir </> unitDir key.unit)
  OsPath.writeFile (srcDir </> moduleSourcePath key) (fatModuleSource numFunctions caseArms key)
