{-# LANGUAGE CPP #-}

-- | Parse Cabal package descriptions to discover units.
module GhcServer.Cabal where

import Control.Monad.Extra (unlessM, whenM)
import Data.Either (partitionEithers)
import Data.Foldable (find, toList, traverse_)
import Data.List (intercalate)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Traversable (for)
import qualified Distribution.ModuleName as Cabal
import Distribution.Package (packageName)
import Distribution.Simple.PackageDescription (readGenericPackageDescription)
import Distribution.Types.BuildInfo (BuildInfo (..))
import Distribution.Types.CondTree (CondTree (..))
import Distribution.Types.Dependency (Dependency (..))
import Distribution.Types.GenericPackageDescription (GenericPackageDescription (..))
import Distribution.Types.Library (Library (..))
import Distribution.Types.LibraryName (LibraryName (..))
import Distribution.Types.PackageName (unPackageName)
import Distribution.Types.UnqualComponentName (unUnqualComponentName)
import Distribution.Utils.Path (getSymbolicPath)
import Distribution.Verbosity (silent)
import GHC (ModuleName, mkModuleName)
import GHC.Data.Graph.Directed (graphFromEdgedVerticesOrd)
import GhcServer.Data.Unit (Project (..), Unit (..), UnitName (..), mkUnitCache)
import GhcServer.Project (unitDepNode)
import Language.Haskell.Extension (Extension (..), Language (..))
import System.Directory.OsPath (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, listDirectory)
import System.OsPath (OsPath, isExtensionOf, osp, (</>))
import System.OsPath.Extra (fromOsPath, toOsPath)
import Types.Log (Logger (..))

#if MIN_VERSION_Cabal(3,14,0)

import Distribution.Utils.Path (makeSymbolicPath)

#endif

-- | Find the first @.cabal@ file in a directory.
findCabalFile :: OsPath -> IO (Maybe OsPath)
findCabalFile dir = do
  entries <- listDirectory dir
  pure ((dir </>) <$> find (isExtensionOf [osp|cabal|]) entries)

-- TODO Can we look these up in the global package DB?
corePackages :: Set UnitName
corePackages =
  [
    "array",
    "base",
    "binary",
    "bytestring",
    "Cabal",
    "Cabal-syntax",
    "containers",
    "deepseq",
    "directory",
    "exceptions",
    "file-io",
    "filepath",
    "ghc",
    "ghc-bignum",
    "ghc-boot",
    "ghc-boot-th",
    "ghc-compact",
    "ghc-experimental",
    "ghc-heap",
    "ghc-internal",
    "ghc-prim",
    "ghci",
    "haskeline",
    "hpc",
    "integer-gmp",
    "libiserv",
    "mtl",
    "os-string",
    "parsec",
    "pretty",
    "process",
    "rts",
    "semaphore-compat",
    "stm",
    "template-haskell",
    "terminfo",
    "text",
    "time",
    "transformers",
    "unix",
    "Win32",
    "xhtml"
  ]

-- | Extract sublibrary deps within the current package, core libraries, and external packages from the deps.
partitionDeps ::
  UnitName ->
  Set UnitName ->
  [Dependency] ->
  ([UnitName], [UnitName], [UnitName])
partitionDeps current components deps =
  (mconcat local, notCore, core)
  where
    (notCore, core) = partitionEithers external

    (local, external) = partitionEithers (decideDep <$> deps)

    decideDep (Dependency name _ sub)
      | package == current
      = Left (mapMaybe nonMain (toList sub))
      | Set.member package corePackages
      = Right (Right package)
      | Set.member package components
      = Left [package]
      | otherwise
      = Right (Left package)
      where
        package = UnitName (unPackageName name)

    nonMain = \case
      LMainLibName -> Nothing
      LSubLibName c -> Just (UnitName (unUnqualComponentName c))

-- | The source dirs of a component, resolved against the project root.
--
-- Cabal treats an empty @hs-source-dirs@ as the package directory.
sourceDirs :: OsPath -> [FilePath] -> [OsPath]
sourceDirs projectRoot = \case
  [] -> [projectRoot]
  srcDirs -> [projectRoot </> toOsPath s | s <- srcDirs]

-- | Ensure that a declared source dir exists and is a directory.
checkSourceDir :: OsPath -> IO ()
checkSourceDir dir = do
  whenM (doesFileExist dir) do
    dirError "is a file"
  unlessM (doesDirectoryExist dir) do
    dirError "does not exist"
  where
    dirError msg =
      fail ("Source dir " ++ fromOsPath dir ++ " " ++ msg)

-- | File extensions considered when resolving a module name to a source file.
sourceSuffixes :: [String]
sourceSuffixes = ["hs", "lhs"]

-- | Resolve a Cabal module name to its source file.
--
-- Mirrors Cabal's own lookup: the module's components are appended to each source dir (via
-- 'Cabal.toFilePath') and tried with each Haskell source suffix, in source dir order.
findModuleSource :: [OsPath] -> Cabal.ModuleName -> IO (Maybe OsPath)
findModuleSource dirs modName =
  firstExisting [dir </> toOsPath (base ++ "." ++ ext) | dir <- dirs, ext <- sourceSuffixes]
  where
    firstExisting = \case
      [] -> pure Nothing
      path : rest -> doesFileExist path >>= \case
        True -> pure (Just path)
        False -> firstExisting rest

    base = Cabal.toFilePath modName

-- | Render a Cabal module name in its dotted form.
moduleNameString :: Cabal.ModuleName -> String
moduleNameString =
  intercalate "." . Cabal.components

-- | Resolve all modules declared by a component to their source files.
--
-- Fails if a module has no source file in any of the component's source dirs.
componentModules :: UnitName -> [OsPath] -> [Cabal.ModuleName] -> IO [(ModuleName, OsPath)]
componentModules name dirs modNames = do
  traverse_ checkSourceDir dirs
  for modNames \ modName ->
    findModuleSource dirs modName >>= \case
      Just path -> pure (mkModuleName (moduleNameString modName), path)
      Nothing -> fail (unwords [
        "No source file for module",
        moduleNameString modName,
        "of component",
        name.string,
        "in:",
        unwords (fromOsPath <$> dirs)
        ])

-- | Render a Cabal 'Extension' as the GHC CLI flag that enables (or disables) it.
extensionArg :: Extension -> String
extensionArg = \case
  EnableExtension ext -> "-X" ++ show ext
  DisableExtension ext -> "-XNo" ++ show ext
  UnknownExtension name -> "-X" ++ name

-- | Render a Cabal 'Language' (@default-language@) as the GHC CLI flag that selects it, e.g.
-- @GHC2021@ becomes @-XGHC2021@. Defaults to 'Haskell2010' when a component has no
-- @default-language@ field, mirroring Cabal's own behavior for that case (a warning, not an
-- error, since @default-language@ is technically optional).
languageArg :: Maybe Language -> String
languageArg = \case
  Just lang -> "-X" ++ show lang
  Nothing -> "-XHaskell2010"

-- | Build a 'Unit' from a library component.
buildUnit ::
  OsPath ->
  OsPath ->
  OsPath ->
  UnitName ->
  Set UnitName ->
  UnitName ->
  Library ->
  IO Unit
buildUnit projectRoot outputDir tmpDir pkgName components name lib = do
  modules <- componentModules name srcDirs (lib.exposedModules ++ bi.otherModules)
  createDirectoryIfMissing True (outputDir </> toOsPath name.string)
  createDirectoryIfMissing True (tmpDir </> toOsPath name.string)
  pure Unit {
    name,
    dir,
    ghcArgs,
    modules,
    depUnits = localDeps,
    extDeps,
    cache = mkUnitCache projectRoot name
  }
  where
    -- TODO support multiple source dirs
    dir = case srcDirPaths of
      d : _ -> projectRoot </> toOsPath d
      [] -> projectRoot

    srcDirs = sourceDirs projectRoot srcDirPaths

    srcDirPaths = map getSymbolicPath bi.hsSourceDirs

    (localDeps, extDeps, coreDeps) = partitionDeps pkgName components bi.targetBuildDepends

    extensionArgs = languageArg bi.defaultLanguage : map extensionArg bi.defaultExtensions

    ghcArgs = extensionArgs ++ concatMap (\ d -> ["-package", d.string]) (extDeps ++ coreDeps)

    bi = lib.libBuildInfo

processPackage :: OsPath -> OsPath -> OsPath -> GenericPackageDescription -> IO Project
processPackage projectRoot outputDir tmpDir gpd = do
  units <- for (main ++ sub) \ (name, tree) ->
    buildUnit projectRoot outputDir tmpDir pkgName components name tree.condTreeData
  pure Project {
    units = Map.fromList [(u.name, u) | u <- units],
    depGraph = graphFromEdgedVerticesOrd (map unitDepNode units)
  }
  where
    components = Set.fromList (fst <$> sub)

    main = (pkgName,) <$> toList gpd.condLibrary

    sub = [(UnitName (unUnqualComponentName name), lib) | (name, lib) <- gpd.condSubLibraries]

    pkgName = UnitName (unPackageName (packageName gpd))

-- | Discover a project from a @.cabal@ file.
--
-- Extracts local dependencies, external dependencies, and extensions.
discoverCabalProject :: Logger -> OsPath -> OsPath -> OsPath -> OsPath -> IO Project
discoverCabalProject logger projectRoot outputDir tmpDir cabalPath = do
  createDirectoryIfMissing True outputDir
  createDirectoryIfMissing True tmpDir
  logger.info ("Loading project configuration from " ++ cabalFile)
#if MIN_VERSION_Cabal(3,14,0)
  package <- readGenericPackageDescription silent Nothing (makeSymbolicPath cabalFile)
#else
  package <- readGenericPackageDescription silent cabalFile
#endif
  processPackage projectRoot outputDir tmpDir package
  where
    cabalFile = fromOsPath cabalPath
