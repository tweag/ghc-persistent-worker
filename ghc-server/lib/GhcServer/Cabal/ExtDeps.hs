-- | Build a Cabal project's external (non-local) dependencies into the Cabal store, in-process, using
-- @cabal-install@'s own project orchestration machinery ("Distribution.Client.ProjectOrchestration"),
-- the same code path that backs the @cabal build@ command.
--
-- The standalone server doesn't reimplement dependency resolution or the Cabal solver: it depends on
-- @cabal-install@ and @Cabal@ as ordinary libraries and drives them directly, mirroring what
-- @cabal build --only-dependencies@ does. Once the in-process build completes, the store's package
-- database (for the exact GHC used by the server, via 'GHC.Paths.ghc') contains registrations for every
-- external dependency, so a unit's own @-package@ flags (added at Cabal project discovery time) can be
-- resolved by GHC via an additional @-package-db@ flag pointing at that database.
module GhcServer.Cabal.ExtDeps (
  resolvePackageDb,
  ensureExtDepsDb,
) where

import Control.Concurrent.MVar (modifyMVar)
import Control.Exception (SomeAsyncException, SomeException, displayException, fromException, throwIO, try)
import Data.Char (isAlphaNum)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Distribution.Client.Config (defaultStoreDir)
import Distribution.Client.DistDirLayout (CabalDirLayout (..), StoreDirLayout (..))
import Distribution.Client.InstallPlan (GenericPlanPackage (..))
import qualified Distribution.Client.InstallPlan as InstallPlan
import Distribution.Client.ProjectConfig.Types (ProjectConfig (..), ProjectConfigBuildOnly (..), ProjectConfigShared (..))
import Distribution.Client.ProjectOrchestration (
  CurrentCommand (OtherCommand),
  ProjectBaseContext (..),
  ProjectBuildContext (..),
  establishProjectBaseContext,
  runProjectBuildPhase,
  runProjectPreBuildPhase,
 )
import Distribution.Client.ProjectPlanning (pruneInstallPlanToDependencies)
import Distribution.Client.ProjectPlanning.Types (ElaboratedConfiguredPackage (..), ElaboratedInstallPlan, ElaboratedSharedConfig (..))
import Distribution.Package (HasUnitId (..))
import Distribution.Simple.Flag (toFlag)
import Distribution.Types.UnitId (UnitId)
import Distribution.Verbosity (verbose)
import GHC.Paths (ghc, libdir)
import GhcServer.Data.BuildEnv (BuildEnv (..))
import GhcServer.Path (fp)
import System.IO (stderr, stdout)
import System.IO.Silently (hCapture)
import Types.Log (Logger (..))
import Prelude hiding (log)

-- | Store directory scoped to this exact GHC build.
--
-- Cabal's default store directory (@$XDG_STATE_HOME\/cabal\/store\/ghc-\<version\>-inplace@) is keyed
-- only by the GHC /version string/, not by the specific compiler build. This project uses a
-- custom-patched GHC provided by Nix, and other custom builds sharing the same version (e.g. a stock
-- nixpkgs GHC, or a different patch set) may have already populated that directory with incompatible
-- packages (different ABI\/representation for things like generic deriving), which manifests as GHC
-- panics rather than a clean rebuild or a version mismatch error.
--
-- Appending a sanitized form of 'GHC.Paths.libdir' (unique per Nix derivation) to the default store
-- directory guarantees the store is exclusive to this GHC build, while still living in a writable
-- location (unlike @libdir@ itself, which is read-only inside the Nix store).
scopedStoreDir :: IO FilePath
scopedStoreDir = do
  base <- defaultStoreDir
  pure (base ++ "-" ++ map sanitize libdir)
  where
    sanitize c = if isAlphaNum c then c else '-'

-- | Collect the 'UnitId's of packages that are local to the project (as opposed to dependencies), so
-- they can be excluded from the install plan, mirroring @cabal build --only-dependencies@.
localUnitIds :: ElaboratedInstallPlan -> Set.Set UnitId
localUnitIds plan =
  Set.fromList [installedUnitId pkg | pkg <- InstallPlan.reverseTopologicalOrder plan, isLocal pkg]
  where
    isLocal (Configured elab) = elab.elabLocalToProject
    isLocal (Installed elab) = elab.elabLocalToProject
    isLocal (PreExisting _) = False

-- | Build a project's external dependencies into the store and return the resulting package database
-- path.
--
-- Drives the same in-process machinery as @cabal build --only-dependencies all@: establishes the
-- project context rooted at @projectDir@, prunes the elaborated install plan down to just the
-- dependencies of the local packages, executes that pruned plan, then reports the store's package
-- database path for the compiler that was used (pinned to the server's own in-process GHC via
-- 'GHC.Paths.ghc', ensuring ABI compatibility).
--
-- Cabal/@cabal-install@ report progress and failures by printing to stdout\/stderr at the given
-- 'Verbosity' rather than through a return value or a pluggable logger (there is no supported hook to
-- redirect this to an arbitrary sink), so 'verbose' is used everywhere a 'Verbosity' is threaded through
-- below to ensure failures are actually descriptive. That output is captured for the duration of the
-- build via 'hCapture' -- which also stops it from interleaving with concurrent unit builds' output on
-- the real handles -- then re-emitted through 'logger.info' (so it still ends up in the log panel like
-- ordinary server stdio) and, on failure, appended to the returned error so it reaches 'TaskFailed'
-- directly instead of only being discoverable by scrolling back through the log.
resolvePackageDb :: Logger -> FilePath -> IO (Either String FilePath)
resolvePackageDb logger projectDir = do
  logger.info ("Building external dependencies in-process via cabal-install for " ++ projectDir)
  (captured, attempt) <- hCapture [stdout, stderr] (try build)
  logger.info captured
  case attempt of
    Left (e :: SomeException)
      -- Don't swallow the scheduler's task-timeout cancellation (see 'Test.Scheduler.Concurrent.runTask'):
      -- rethrow async exceptions instead of reporting them as an ordinary build failure.
      | Just asyncExc <- fromException e -> throwIO (asyncExc :: SomeAsyncException)
      | otherwise -> pure (Left (displayException e ++ "\n" ++ captured))
    Right dbPath -> pure (Right dbPath)
  where
    build = do
      storeDir <- scopedStoreDir
      let
        -- Force a serial build (@-j1@, no @jsem@) so that 'resolveBuildTimeSettings' picks
        -- @Serial@ for 'buildSettingNumJobs'. This keeps @cabal-install@ from enabling its default
        -- per-package build-log file (only used for parallel builds or @--build-log@) and from
        -- forcing 'Distribution.Client.SetupWrapper.SelfExecMethod' for @Setup.hs@ invocations
        -- ('forceExternalSetupMethod' is tied to the same parallelism flag). @SelfExecMethod@
        -- re-execs the current process expecting it to be the @cabal@ executable itself (with its
        -- hidden "act as Setup.hs" dispatch), which does not hold when @cabal-install@ is embedded
        -- as a library inside @ghc-server@; avoiding it entirely lets @cabal-install@ fall back to
        -- driving @Setup.hs@ in-process ('InternalMethod'), which works correctly here.
        cliConfig =
          mempty {
            projectConfigShared =
              mempty {
                projectConfigProjectDir = toFlag projectDir,
                projectConfigHcPath = toFlag ghc,
                -- See 'scopedStoreDir': avoids ABI-incompatible reuse of a store populated by a
                -- different custom GHC build that happens to share the same version string.
                projectConfigStoreDir = toFlag storeDir
              },
            -- Verbosity for the per-package configure\/build steps that 'runProjectBuildPhase' drives
            -- (Setup.hs invocations); see the 'verbose' arguments below for the top-level orchestration
            -- verbosity, and the module-level note above 'resolvePackageDb' for why this is necessary.
            projectConfigBuildOnly =
              mempty {
                projectConfigVerbosity = toFlag verbose
              }
            -- projectConfigBuildOnly =
            --   mempty {
            --     projectConfigNumJobs = toFlag (Just 1),
            --     projectConfigUseSemaphore = toFlag False
            --   }
          }
      ctx <- establishProjectBaseContext verbose cliConfig OtherCommand
      buildCtx <-
        runProjectPreBuildPhase verbose ctx \elaboratedPlan ->
          case pruneInstallPlanToDependencies (localUnitIds elaboratedPlan) elaboratedPlan of
            Left err -> fail ("Cannot prune install plan to dependencies: " ++ show err)
            Right pruned -> pure (pruned, Map.empty)
      outcomes <- runProjectBuildPhase verbose ctx buildCtx
      case [err | (_, Left err) <- Map.toList outcomes] of
        (err : _) -> fail ("Build failed: " ++ show err)
        [] ->
          pure (storePackageDBPath ctx.cabalDirLayout.cabalStoreDirLayout buildCtx.elaboratedShared.pkgConfigCompiler)

-- | Ensure the project's external Cabal dependencies have been built into the store, memoizing the
-- result in 'BuildEnv.extDepsDb' so the (potentially slow) build only runs once per server lifetime,
-- regardless of how many units request it concurrently.
--
-- Takes the caller's 'Logger' (rather than reaching into 'BuildEnv.log' directly) so that whichever
-- caller happens to trigger the actual build gets its own instrument-connected logger used for
-- 'resolvePackageDb''s output, ensuring it reaches the instrument UI's log viewer like the rest of
-- that caller's log output.
ensureExtDepsDb :: BuildEnv -> Logger -> IO (Either String FilePath)
ensureExtDepsDb env logger =
  modifyMVar env.extDepsDb \case
    Just cached -> pure (Just cached, cached)
    Nothing -> do
      result <- resolvePackageDb logger (fp env.projectRoot)
      pure (Just result, result)
