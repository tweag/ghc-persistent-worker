module GhcServer.Build.Compile where

import qualified Data.Text as Text
import GHC (ModuleName, mkModule, moduleNameString)
import GHC.Unit.Types (stringToUnit)
import GhcServer.Cache (loadHomeUnitCache)
import GhcServer.Data.BuildEnv (BuildEnv (..))
import GhcServer.Data.Unit (Unit (..))
import GhcServer.Log (emitEvent, instrumentLogger, withBuildLog)
import Internal.Compile.Make (compileModuleWithDepsInHpt)
import Internal.Session (withGhcMakeModule)
import Prelude hiding (log)
import System.Directory.OsPath (createDirectoryIfMissing)
import System.OsPath ((</>))
import System.OsPath.Extra (toOsPath)
import Types.Api (UnitName (..))
import Types.Args (Args (..))
import Types.BuckArgs (IsInterpreted (..))
import Types.CachedDeps (CachedDeps)
import Types.Env (Env (..))
import Types.Log (Logger (..))
import Types.Target (ModuleTarget (..), TargetSpec (..))

-- | Construct a 'ModuleTarget' for a named module in a given unit.
moduleTarget :: UnitName -> ModuleName -> ModuleTarget
moduleTarget name modName =
  ModuleTarget {
    module_ = mkModule (stringToUnit (Text.unpack name.text)) modName
  }

-- | Run an action in a GHC session scoped to a single module of a unit.
--
-- Shared by the compile and execute task kinds, which differ only in what they do with the
-- session: a per-module temp directory is created, the unit's home-unit cache is restored, and a
-- capturing logger wired to the instrument channel is installed.  The captured log is returned
-- alongside the action's result, since both task kinds report it on failure.
--
-- @label@ distinguishes session kinds for the same module in the temp directory name and the log
-- category, so that concurrent sessions of different kinds do not share scratch space.
withModuleSession ::
  BuildEnv ->
  Unit ->
  ModuleName ->
  Maybe String ->
  CachedDeps ->
  (Logger -> Env -> ModuleTarget -> IO a) ->
  IO (a, [String])
withModuleSession buildEnv unit modName label cachedDeps use = do
  createDirectoryIfMissing True modTmpDir
  cachedUnit <- loadHomeUnitCache unit.cache
  withBuildLog \ rawLogger -> do
    let
      logger = instrumentLogger buildEnv.instrChan logCategory rawLogger
      args = buildEnv.baseArgs {
        tempDir = Just modTmpDir,
        homeUnit = cachedUnit,
        cachedDeps = Just cachedDeps
      }
      env = Env {log = logger, state = buildEnv.stateVar, args}
    result <- use logger env (moduleTarget unit.name modName)
    captured <- logger.flush
    pure (result, captured)
  where
    modBaseName = moduleNameString modName

    unitDir = toOsPath (Text.unpack unit.name.text)

    modTmpDir = buildEnv.tmpDir </> unitDir </> toOsPath (maybe modBaseName ((modBaseName ++ "-") ++) label)

    logCategory = Text.unpack unit.name.text ++ ":" ++ modBaseName ++ maybe "" (":" ++) label

-- | Compile a single module within a unit.
--
-- The caller provides the pre-assembled 'CachedDeps' (computed from the module map)
-- which are passed to the worker for HPT pre-population.
compileSingleModule ::
  BuildEnv ->
  Unit ->
  ModuleName ->
  CachedDeps ->
  Int ->
  IO ([(UnitName, ModuleName, String)], [String])
compileSingleModule buildEnv unit modName cachedDeps requestId = do
  (result, captured) <- withModuleSession buildEnv unit modName Nothing cachedDeps \ logger env target ->
    withGhcMakeModule Compiled target env Nothing \ _targetSpec ->
      compileModuleWithDepsInHpt logger (emitEvent buildEnv.instrChan) requestId (TargetModule target)
  pure $ case result of
    Just _ -> ([], captured)
    Nothing -> ([(unit.name, modName, "Compilation failed:\n" ++ unlines captured)], [])
