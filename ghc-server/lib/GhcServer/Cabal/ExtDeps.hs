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
import Control.Exception (displayException, try)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Distribution.Client.DistDirLayout (CabalDirLayout (..), StoreDirLayout (..))
import Distribution.Client.InstallPlan (GenericPlanPackage (..))
import qualified Distribution.Client.InstallPlan as InstallPlan
import Distribution.Client.ProjectConfig.Types (ProjectConfig (..), ProjectConfigShared (..))
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
import Distribution.Verbosity (silent)
import GHC.Paths (ghc)
import GhcServer.Data.BuildEnv (BuildEnv (..))
import GhcServer.Path (fp)
import Types.Log (Logger (..))
import Prelude hiding (log)

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
resolvePackageDb :: Logger -> FilePath -> IO (Either String FilePath)
resolvePackageDb logger projectDir = do
  logger.info ("Building external dependencies in-process via cabal-install for " ++ projectDir)
  attempt <- try build
  pure case attempt of
    Left (e :: IOError) -> Left (displayException e)
    Right dbPath -> Right dbPath
  where
    build = do
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
                projectConfigHcPath = toFlag ghc
              }
            -- projectConfigBuildOnly =
            --   mempty {
            --     projectConfigNumJobs = toFlag (Just 1),
            --     projectConfigUseSemaphore = toFlag False
            --   }
          }
      ctx <- establishProjectBaseContext silent cliConfig OtherCommand
      buildCtx <-
        runProjectPreBuildPhase silent ctx \elaboratedPlan ->
          case pruneInstallPlanToDependencies (localUnitIds elaboratedPlan) elaboratedPlan of
            Left err -> fail ("Cannot prune install plan to dependencies: " ++ show err)
            Right pruned -> pure (pruned, Map.empty)
      outcomes <- runProjectBuildPhase silent ctx buildCtx
      case [err | (_, Left err) <- Map.toList outcomes] of
        (err : _) -> fail ("Build failed: " ++ show err)
        [] ->
          pure (storePackageDBPath ctx.cabalDirLayout.cabalStoreDirLayout buildCtx.elaboratedShared.pkgConfigCompiler)

-- | Ensure the project's external Cabal dependencies have been built into the store, memoizing the
-- result in 'BuildEnv.extDepsDb' so the (potentially slow) build only runs once per server lifetime,
-- regardless of how many units request it concurrently.
ensureExtDepsDb :: BuildEnv -> IO (Either String FilePath)
ensureExtDepsDb env =
  modifyMVar env.extDepsDb \case
    Just cached -> pure (Just cached, cached)
    Nothing -> do
      result <- resolvePackageDb env.log (fp env.projectRoot)
      pure (Just result, result)
