{-# LANGUAGE NoFieldSelectors #-}

module TestSetup where

import Control.Concurrent (MVar)
import Data.Foldable (for_, toList)
import Data.List (intersperse)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Traversable (for)
import GHC.Paths (ghc_pkg)
import GHC.Unit (UnitId, stringToUnitId, unitIdString)
import Internal.State (newStateWith)
import Prelude hiding (log)
import System.Directory (createDirectoryIfMissing)
import qualified System.Directory.OsPath as OsPath (createDirectoryIfMissing)
import System.FilePath ((<.>), (</>))
import System.IO.Temp (withSystemTempDirectory)
import System.OsPath.Extra (OsPath, fromOsPath, toOsPath)
import qualified System.OsPath.Extra as OsPath ((</>))
import System.Process.Typed (proc, runProcess_)
import Types.Args (Args (..), TargetId (..), emptyArgs)
import Types.State (WorkerState (..))
import Types.State.Oneshot (OneshotCacheFeatures (..))

-- | Global configuration for a worker compilation test.
data Conf =
  Conf {
    -- | Root directory of the test in @/tmp@.
    tmp :: OsPath,

    -- | The worker state.
    state :: MVar WorkerState,

    -- | The base cli args used for all modules.
    args0 :: Args
  }

-- | Config for a single test module.
data ModuleSpec =
  ModuleSpec {
    -- | Module name.
    name :: String,

    -- | The module's source code.
    content :: String
  }
  deriving stock (Eq, Show)

data Reexport =
  Reexport {
    unit :: String,
    moduleName :: String
  }
  deriving stock (Eq, Show)

-- | Config for a single test home unit.
data UnitSpec =
  UnitSpec {
    -- | Unit ID.
    name :: String,

    -- | Names of home units on which this unit depends.
    deps :: [String],

    -- | The modules belonging to this unit.
    modules :: NonEmpty ModuleSpec,

    reexports :: [Reexport],

    extraDbConf :: [String]
  }
  deriving stock (Eq, Show)

-- | Generated data for a test module.
data Module =
  Module {
    -- | Module name.
    name :: String,

    -- | Path to the source file.
    src :: OsPath,

    -- | Home unit to which this module belongs.
    unit :: String
  }
  deriving stock (Eq, Show)

-- | Generated data for a test unit.
data Unit =
  Unit {
    -- | Unit ID.
    uid :: UnitId,

    -- | Unit ID.
    name :: String,

    -- | Root source directory of this unit.
    dir :: OsPath,

    -- | Names of home units on which this unit depends.
    deps :: [String],

    -- | Path to the dummy package DB created for the metadata step, analogous to what's created by Buck.
    db :: OsPath,

    -- | The modules belonging to this unit.
    modules :: NonEmpty Module
  }
  deriving stock (Eq)

instance Show Unit where
  showsPrec d Unit {..} =
    showParen (d > 5) (
      showString "Unit { uid = "
      .
      showsPrec 5 (unitIdString uid)
      .
      showString ", name = "
      .
      showsPrec 5 name
      .
      showString ", dir = "
      .
      showsPrec 5 dir
      .
      showString ", db = "
      .
      showsPrec 5 db
      .
      showString ", modules = "
      .
      showsPrec 5 modules
      .
      showString " }"
    )

-- | General CLI args used by each module job.
baseArgs :: OsPath -> Args
baseArgs tmp =
  (emptyArgs []) {
    workerTargetId = Just (TargetId "test"),
    tempDir = Nothing,
    ghcOptions = (artifactDir =<< ["o", "hie", "dump"]) ++ [
      "-fwrite-ide-info",
      "-no-link",
      "-dynamic",
      -- "-fwrite-if-simplified-core",
      "-fbyte-code-and-object-code",
      "-fprefer-byte-code",
      -- "-shared",
      "-fPIC",
      "-osuf",
      "dyn_o",
      "-hisuf",
      "dyn_hi",
      "-package",
      "base"
      -- , "-v"
      -- , "-ddump-if-trace"
    ]
  }
  where
    artifactDir a = ["-" ++ a ++ "dir", fromOsPath tmp </> "out"]

-- | A package DB config file for the given unit.
dbConf ::
  OsPath ->
  String ->
  NonEmpty Module ->
  [Reexport] ->
  [String] ->
  String
dbConf srcDir unit modules reexports extra =
  unlines $ [
    "name: " ++ unit,
    "version: 1.0",
    "id: " ++ unit,
    "key: " ++ unit,
    "import-dirs: " ++ fromOsPath srcDir,
    "exposed: True",
    "exposed-modules: " ++ mconcat (intersperse ", " (exposed ++ (formatReexport <$> reexports)))
  ] ++ extra
  where
    exposed = [name | Module {name} <- toList modules]

    formatReexport Reexport {unit = runit, ..} =
      moduleName ++ " from " ++ runit ++ ":" ++ moduleName

-- | Write a fresh package DB without a library to the specified directory, using @ghc-pkg@ from the directory in
-- 'Conf'.
createDb :: OsPath -> String -> IO OsPath
createDb dir confFile = do
  OsPath.createDirectoryIfMissing False db
  runProcess_ (proc ghc_pkg ["-v0", "--package-db", fromOsPath db, "recache"])
  runProcess_ (proc ghc_pkg ["-v0", "--package-db", fromOsPath db, "register", "--force", confFile])
  pure db
  where
    db = dir OsPath.</> toOsPath "package.conf.d"

writeDb :: UnitSpec -> OsPath -> String -> IO OsPath
writeDb unit dir db = do
  writeFile confFile db
  createDb dir confFile
  where
    confFile = fromOsPath dir </> unit.name <.> "conf"

-- | Create a package DB for a set of 'ModuleSpec' and assemble everything into a 'Unit'.
-- This is used for home units that are part of the build – like Buck, we create a package DB without any interfaces so
-- downsweep can see dependencies.
-- This is gonna be legacy soon, since we've changed metadata to use the actual home units instead, pending some
-- performance optimizations.
createEmptyHomeUnitDb :: UnitSpec -> OsPath -> NonEmpty Module -> IO OsPath
createEmptyHomeUnitDb unit dir modules =
  writeDb unit dir (dbConf dir unit.name modules unit.reexports unit.extraDbConf)

withTmp ::
  (OsPath -> IO a) ->
  IO a
withTmp use =
  withSystemTempDirectory "buck-worker-test" \ tmp -> do
    for_ @[] ["src", "tmp", "out"] \ dir ->
      createDirectoryIfMissing False (tmp </> dir)
    use (toOsPath tmp)

-- | Set up an environment with dummy package DBs for the set of modules returned by the first argument, then run the
-- second argument with the resulting unit configurations.
withProject ::
  (Conf -> IO (NonEmpty UnitSpec)) ->
  (Conf -> NonEmpty Unit -> IO a) ->
  IO a
withProject mkTargets use =
  withTmp \ tmp -> do
    state <- newStateWith OneshotCacheFeatures {
      loader = False,
      enable = True,
      names = False,
      finder = False,
      eps = False
    }
    let conf = Conf {tmp, state, args0 = baseArgs tmp, ..}
    targets <- mkTargets conf
    units <- for targets \ unit -> do
      let dir = tmp OsPath.</> toOsPath ("src" </> unit.name)
      OsPath.createDirectoryIfMissing False dir
      modules <- for unit.modules \ ModuleSpec {name, content} -> do
        let src = fromOsPath dir </> name <.> "hs"
        writeFile src content
        pure Module {unit = unit.name,src = toOsPath src,..}
      db <- createEmptyHomeUnitDb unit dir modules
      pure Unit {
        uid = stringToUnitId unit.name,
        name = unit.name,
        deps = unit.deps,
        dir,
        db,
        modules
      }
    use conf units
