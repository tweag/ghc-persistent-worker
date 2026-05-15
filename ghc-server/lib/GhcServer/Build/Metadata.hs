module GhcServer.Build.Metadata where

import Control.Monad.Extra (ifM)
import qualified Data.Map.Strict as Map
import GhcServer.Cache (buildDepPlans, writeUnitCache)
import GhcServer.Data.BuildEnv (BuildEnv (..))
import GhcServer.Data.BuildEvent (BuildEvent (..), logEvent)
import GhcServer.Data.Unit (Project (..), Unit (..), UnitName (..))
import GhcServer.Log (withBuildLog)
import GhcServer.Path (fromOsPath, toOsPath)
import Internal.Metadata (computeMetadata)
import Prelude hiding (log)
import System.OsPath.Extra (OsPath, (</>))
import Types.Args (Args (..))
import Types.CachedDeps (CachedBuildPlans)
import Types.Env (Env (..))
import Types.Log (Logger (..))

-- | Static GHC arguments used for every metadata step, matching the flags used by the Buck rules and property test.
--
-- These enable dynamic linking, bytecode generation, and explicit package management.
staticMetaArgs :: [String]
staticMetaArgs =
  [
    "-i",
    "-hide-all-packages",
    "-include-pkg-deps",
    "-no-link",
    "-dynamic",
    "-fbyte-code-and-object-code",
    "-fprefer-byte-code",
    "-fPIC",
    "-osuf", "dyn_o",
    "-hisuf", "dyn_hi",
    "-package", "base"
  ]

-- | Construct the GHC CLI arguments for a metadata step.
metadataArgs :: Args -> OsPath -> Maybe CachedBuildPlans -> Unit -> Args
metadataArgs base outputDir cachedPlans unit =
  base {
    buildPlan = Just buildPlanPath,
    cachedBuildPlans = cachedPlans,
    ghcOptions =
      staticMetaArgs
      ++ unit.ghcArgs
      ++ depFlags
      ++ [
        "-this-unit-id", unit.name.string,
        "-odir", fromOsPath outDir,
        "-hidir", fromOsPath outDir,
        "-stubdir", fromOsPath outDir,
        "-dep-makefile", "/dev/null"
      ]
      ++ sourcePaths
  }
  where
    buildPlanPath = outDir </> toOsPath "build-plan.json"

    outDir = outputDir </> toOsPath unit.name.string

    depFlags = concatMap depFlag unit.depUnits

    depFlag dep = ["-package-id", dep.string]

    sourcePaths = map fromOsPath unit.sources

-- | Run the metadata step for a unit.
--
-- On success, writes the unit's cache files (args + 'CachedUnit' JSON) so that subsequent builds
-- can restore the unit without rerunning metadata.
--
-- Returns errors (empty on success) and the captured build log.
runMetadata :: BuildEnv -> UnitName -> IO ([(UnitName, String)], [String])
runMetadata buildEnv name = do
  buildEnv.log.debug ("Metadata: " ++ name.string)
  logEvent buildEnv.events (MetadataRan name)
  case Map.lookup name buildEnv.project.units of
    Nothing -> pure ([(name, "Unit not found in project")], [])
    Just unit -> withBuildLog (run unit)
  where
    run unit logger = do
      cachedPlans <- buildDepPlans buildEnv.project.depGraph unit
      let env = Env {
            log = logger,
            state = buildEnv.stateVar,
            args = metadataArgs buildEnv.baseArgs buildEnv.outputDir (Just cachedPlans) unit
          }
      ifM (fst <$> computeMetadata env) (success unit (Just cachedPlans) env.args logger) (failure logger)

    success unit cachedPlans args logger = do
      cacheResult <- case args.buildPlan of
        Nothing -> pure (Right ())
        Just buildPlan -> writeUnitCache logger unit.cache cachedPlans buildPlan args.ghcOptions
      case cacheResult of
        Left err -> do
          captured <- logger.flush
          pure ([(name, "Cache write failed: " ++ err)], captured)
        Right () -> do
          captured <- logger.flush
          pure ([], captured)

    failure logger = do
      details <- unlines <$> logger.flush
      pure ([(name, "Metadata failed:\n" ++ details)], [])
