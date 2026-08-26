{-# LANGUAGE CPP #-}
-- | Parse Cabal package descriptions to discover units for the standalone GHC server.
--
-- Each library component (main library and sub-libraries) becomes a 'Unit'.
-- Dependencies between local libraries are resolved by matching package names
-- against the set of known library names in the same package.
module GhcServer.Cabal where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Distribution.Compat.NonEmptySet (toList)
import Distribution.Package (packageName)
import Distribution.Simple.PackageDescription (readGenericPackageDescription)
import Distribution.Types.BuildInfo (BuildInfo (..))
import Distribution.Types.CondTree (CondTree (..))
import Distribution.Types.Dependency (Dependency, depLibraries, depPkgName)
import Distribution.Types.GenericPackageDescription (GenericPackageDescription (..))
import Distribution.Types.Library (Library (..))
import Distribution.Types.LibraryName (LibraryName (..))
import Distribution.Types.PackageName (unPackageName)
import Distribution.Types.UnqualComponentName (unUnqualComponentName)
import Distribution.Utils.Path (getSymbolicPath)
#if MIN_VERSION_Cabal(3,14,0)
import Distribution.Utils.Path (makeSymbolicPath)
#endif
import Data.Foldable (find)
import Distribution.Verbosity (silent)
import GHC.Data.Graph.Directed (graphFromEdgedVerticesOrd)
import GHC.Data.OsPath (isSuffixOf)
import GhcServer.Data.Unit (Project (..), Unit (..), UnitName (..), mkUnitCache)
import Language.Haskell.Extension (Extension (..), Language (..))
import GhcServer.Path (osPath)
import GhcServer.Project (isHaskellSource, unitDepNode)
import System.Directory.OsPath (createDirectoryIfMissing, doesFileExist, listDirectory)
import System.OsPath (OsPath, (</>))
import System.OsPath.Extra (fromOsPath, toOsPath)
import Types.Log (Logger (..))

-- | Find the first @.cabal@ file in a directory.
findCabalFile :: OsPath -> IO (Maybe OsPath)
findCabalFile dir = do
  entries <- listDirectory dir
  pure ((dir </>) <$> find (toOsPath ".cabal" `isSuffixOf`) entries)

-- | The name of a library component.
libraryUnitName :: String -> LibraryName -> UnitName
libraryUnitName pkgName = \case
  LMainLibName -> UnitName pkgName
  LSubLibName c -> UnitName (unUnqualComponentName c)

-- | Extract the set of all library component names in a package.
localLibNames :: String -> GenericPackageDescription -> Set.Set UnitName
localLibNames pkgName gpd =
  Set.fromList $
    [libraryUnitName pkgName LMainLibName | Just _ <- [gpd.condLibrary]]
    ++
    [libraryUnitName pkgName (LSubLibName c) | (c, _) <- gpd.condSubLibraries]

-- | Extract local (home unit) dependency names from a dependency.
--
-- Handles two cases:
--
-- * @build-depends: test-project:lib-a@ — @depPkgName@ is the package name, the sub-library
--   name is in @depLibraries@.
-- * @build-depends: lib-a@ — @depPkgName@ is @lib-a@ directly (only if it matches a local name).
classifyDep :: String -> Set.Set UnitName -> Dependency -> [UnitName]
classifyDep pkgName locals dep
  | depName == pkgName =
    -- Qualified dep like test-project:lib-a — extract sub-library names
    [libraryUnitName pkgName ln | ln <- toList (depLibraries dep), ln /= LMainLibName]
  | UnitName depName `Set.member` locals =
    [UnitName depName]
  | otherwise =
    []
  where
    depName = unPackageName (depPkgName dep)

-- | Partition dependencies into local (home unit) and external names.
--
-- Packages bundled with the GHC toolchain itself (see 'bootPackages') still need an explicit
-- @-package@ flag (the server always builds with @-hide-all-packages@, which suppresses GHC's normal
-- auto-exposure of the global package database), but they are excluded from the /ext-deps/ list: they
-- are always resolvable via GHC's global package database, so they never need to be built into the
-- Cabal store via "GhcServer.Cabal.ExtDeps".
--
-- Returns @(localDeps, extDeps, allExternalPackageNames)@, where @extDeps@ is the subset of
-- @allExternalPackageNames@ that additionally requires an in-process Cabal store build.
partitionDeps :: String -> Set.Set UnitName -> [Dependency] -> ([UnitName], [String], [String])
partitionDeps pkgName locals deps =
  (concatMap (classifyDep pkgName locals) deps, extDeps, allExternals)
  where
    allExternals =
      [ unPackageName (depPkgName d)
      | d <- deps
      , let dn = unPackageName (depPkgName d)
      , dn /= pkgName
      , UnitName dn `Set.notMember` locals
      ]
    extDeps = filter (`Set.notMember` bootPackages) allExternals

-- | Package names bundled with the GHC toolchain, always resolvable via the global package database
-- without requiring an external Cabal build.
bootPackages :: Set.Set String
bootPackages =
  Set.fromList [
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

-- | Discover source files in a list of source directories.
discoverSources :: OsPath -> [FilePath] -> IO [OsPath]
discoverSources projectRoot srcDirs = do
  let dirs = if null srcDirs then [projectRoot] else map (\ s -> projectRoot </> osPath s) srcDirs
  concat <$> traverse listSourceDir dirs
  where
    listSourceDir dir = do
      exists <- doesFileExist dir >>= \case
        True -> pure False
        False -> pure True
      if exists
        then do
          entries <- listDirectory dir
          pure [dir </> e | e <- entries, isHaskellSource e]
        else pure []

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
  String ->
  Set.Set UnitName ->
  LibraryName ->
  Library ->
  IO Unit
buildUnit projectRoot outputDir tmpDir pkgName locals libName lib = do
  let name = libraryUnitName pkgName libName
      bi = lib.libBuildInfo
      srcDirPaths = map getSymbolicPath bi.hsSourceDirs
      (localDeps, extDeps, allExternals) = partitionDeps pkgName locals bi.targetBuildDepends
      extensionArgs = languageArg bi.defaultLanguage : map extensionArg bi.defaultExtensions
      ghcArgs = extensionArgs ++ concatMap (\ d -> ["-package", d]) allExternals
  sources <- discoverSources projectRoot srcDirPaths
  createDirectoryIfMissing True (outputDir </> osPath name.string)
  createDirectoryIfMissing True (tmpDir </> osPath name.string)
  pure Unit {
    name,
    dir = case srcDirPaths of
      (d : _) -> projectRoot </> osPath d
      [] -> projectRoot,
    ghcArgs,
    sources,
    depUnits = localDeps,
    extDeps,
    cache = mkUnitCache projectRoot name
  }

-- | Discover a project from a @.cabal@ file.
--
-- Each library component becomes a unit. Sub-libraries that depend on each other
-- are linked via 'depUnits'. External dependencies become @-package@ flags.
discoverCabalProject :: Logger -> OsPath -> OsPath -> OsPath -> OsPath -> IO Project
discoverCabalProject logger projectRoot outputDir tmpDir cabalFile = do
  createDirectoryIfMissing True outputDir
  createDirectoryIfMissing True tmpDir
  logger.info ("Loading project configuration from " ++ cabalFp)
#if MIN_VERSION_Cabal(3,14,0)
  gpd <- readGenericPackageDescription silent Nothing (makeSymbolicPath cabalFp)
#else
  gpd <- readGenericPackageDescription silent cabalFp
#endif
  let pkgName = unPackageName (packageName gpd)
      locals = localLibNames pkgName gpd
  units <- sequence $
    [buildUnit projectRoot outputDir tmpDir pkgName locals LMainLibName lib
     | Just ct <- [gpd.condLibrary]
     , let lib = ct.condTreeData]
    ++
    [buildUnit projectRoot outputDir tmpDir pkgName locals (LSubLibName c) (ct.condTreeData)
     | (c, ct) <- gpd.condSubLibraries]
  let unitMap = Map.fromList [(u.name, u) | u <- units]
      depGraph = graphFromEdgedVerticesOrd (map unitDepNode units)
  pure Project {units = unitMap, depGraph}
  where
    cabalFp = fromOsPath cabalFile
