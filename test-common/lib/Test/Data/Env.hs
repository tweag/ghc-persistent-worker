module Test.Data.Env where

import Data.IORef (IORef)
import Data.Set (Set)
import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable)
import System.OsPath.Extra (OsPath)
import Test.Tasty (TestTree, askOption)
import Test.Tasty.Options (IsOption (..), OptionDescription (..), flagCLParser, safeRead)
import Types.Args (Args)
import Types.Env (Env)

newtype MaxUnits =
  MaxUnits Int
  deriving stock (Eq, Show, Typeable)
  deriving newtype (Num, Real, Enum, Integral, Ord)

instance IsOption MaxUnits where
  defaultValue = 5
  parseValue = fmap MaxUnits . safeRead
  optionName = pure "max-units"
  optionHelp = pure "Maximum number of units in the generated project"

newtype MaxModulesPerUnit =
  MaxModulesPerUnit Int
  deriving stock (Eq, Show, Typeable)
  deriving newtype (Num, Real, Enum, Integral, Ord)

instance IsOption MaxModulesPerUnit where
  defaultValue = 5
  parseValue = fmap MaxModulesPerUnit . safeRead
  optionName = pure "max-modules-per-unit"
  optionHelp = pure "Maximum number of modules per unit in the generated project"

newtype MaxJobs =
  MaxJobs Int
  deriving stock (Eq, Show, Typeable)
  deriving newtype (Num, Real, Enum, Integral, Ord)

instance IsOption MaxJobs where
  defaultValue = 6
  parseValue = fmap MaxJobs . safeRead
  optionName = pure "max-concurrent-jobs"
  optionHelp = pure "Maximum number of concurrent build jobs in the scheduler"

-- | When set, the source and temp directories of a failing test case are not deleted after the test run, allowing
-- inspection of the generated project and build artifacts. The path is printed to the console when a failure is
-- detected.
newtype KeepFailedDirs =
  KeepFailedDirs Bool
  deriving stock (Eq, Show, Typeable)

instance IsOption KeepFailedDirs where
  defaultValue = KeepFailedDirs False
  parseValue = fmap KeepFailedDirs . safeRead
  optionName = pure "keep-failed-dirs"
  optionHelp = pure "Do not delete the source and temp directories of a failing test case"
  optionCLParser = flagCLParser Nothing (KeepFailedDirs True)

data TestConfig =
  TestConfig {
    maxUnits :: MaxUnits,
    maxModulesPerUnit :: MaxModulesPerUnit,
    maxConcurrentJobs :: MaxJobs
  }
  deriving stock (Show)

testConfigOptions :: [OptionDescription]
testConfigOptions =
  [
    Option (Proxy @MaxUnits),
    Option (Proxy @MaxModulesPerUnit),
    Option (Proxy @MaxJobs),
    Option (Proxy @KeepFailedDirs)
  ]

withTestConfig :: (TestConfig -> TestTree) -> TestTree
withTestConfig use =
  askOption \ maxUnits ->
    askOption \ maxModulesPerUnit ->
      askOption \ maxConcurrentJobs ->
        use TestConfig {..}

data TestEnv =
  TestEnv {
    -- | Root temp dir.
    rootDir :: OsPath,
    -- | Empty worker args that contain the GHC distribution directory (@topdir@).
    baseArgs :: Args,
    -- | Whether to retain 'rootDir' instead of deleting it when a test case fails.
    keepFailedDirs :: Bool,
    -- | Set to 'True' when a failing test case's directories should be retained. Checked when 'rootDir' is torn
    -- down at the end of the test run.
    retainedDirs :: IORef Bool
  }

-- | Environment for a single GHC session with fresh temp directories for sources and outputs, as well as the basic
-- worker 'Env' with a fresh 'WorkerState'.
data SessionEnv =
  SessionEnv {
    shared :: TestEnv,
    sourceDir :: OsPath,
    tempDir :: OsPath,
    env :: Env,
    -- | Per-package DB paths for external dependency packages, matching the Buck model where each
    -- external dep has its own package DB and the transitive closure is passed as separate @-package-db@ flags.
    extDepDbs :: [FilePath],
    -- | All external dependency indexes used by any module in the project.
    extDeps :: Set Int
  }
