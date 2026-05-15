{-# LANGUAGE CPP #-}

module UnitIndexTest where

import Test.Run (unitTest)
import Test.Tasty (TestTree)

#if defined(UNIT_INDEX)

import Control.Monad (foldM)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Foldable (for_, toList)
import qualified Data.List.NonEmpty as NonEmpty
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import qualified Data.Text as Text
import Data.Text (Text)
import GHC (
  DynFlags (homeUnitId_, packageDBFlags, packageFlags),
  GeneralFlag (..),
  Ghc,
  GhcMonad (..),
  HscEnv,
  ModuleName,
  mkModuleName,
  )
import GHC.Driver.DynFlags (
  ModRenaming (..),
  PackageArg (..),
  PackageDBFlag (..),
  PackageFlag (..),
  PkgDbRef (..),
  gopt_set,
  )
import GHC.Driver.Env.Types (HscEnv (..))
import GHC.Driver.Monad (modifySession)
import GHC.Types.Unique.Map (lookupUniqMap)
import GHC.Unit (UnitState (..), homeUnitId, initUnits, stringToUnit, stringToUnitId, unitIdString)
import GHC.Unit.Env (HomeUnitEnv (..), UnitEnv (..), ue_home_unit_graph)
import GHC.Unit.State (UnitIndexQuery (..), unitIndexQuery)
import qualified GHC.Utils.Logger as GHC
import GHC.Utils.Outputable (hang, parens, ppr, text, vcat, (<+>))
import Internal.Cache.Metadata (insertHomeUnit)
import Internal.Log (dbgp)
import Internal.State.UnitIndex (newUnitIndex)
import Prelude hiding (log)
import Test.Run (expectNoDiagnostics, testSession)
import TestSetup (Conf (..), ModuleSpec (..), Reexport (..), Unit (..), UnitSpec (..), withProject)

#if defined(MWB)

import GHC.Unit.Home.Graph (unitEnv_keys, unitEnv_lookup, unitEnv_new)
import System.OsPath (unsafeEncodeUtf)

#else

import GHC.Unit.Env (unitEnv_keys, unitEnv_lookup, unitEnv_new)

#endif

depFlagRename :: String -> Bool -> [(ModuleName, ModuleName)] -> PackageFlag
depFlagRename name expose rename =
  ExposePackage ("-package " ++ name) (PackageArg name) (ModRenaming expose rename)

depFlag :: String -> PackageFlag
depFlag name =
  depFlagRename name True []

homeDep :: Word -> [PackageFlag]
homeDep num =
  [ExposePackage ("-package-id " ++ name) (UnitIdArg (stringToUnit name)) (ModRenaming True [])]
  where
    name = "unit" ++ show (num - 1)

dbFlag :: String -> PackageDBFlag
dbFlag path =
#if defined(MWB)
  PackageDB (PkgDbPath (toOsPath path))
#else
  PackageDB (PkgDbPath path)
#endif

testDepUnits ::
  Int ->
  Conf ->
  NonEmpty UnitSpec
testDepUnits count Conf {} =
  NonEmpty.fromList $ [
    UnitSpec {
      name = "dep" ++ show i,
      deps = [],
      modules = [
        ModuleSpec ("Dep" ++ show i) (content i)
      ],
      reexports = [],
      extraDbConf = []
    }
    |
    i <- [1..count]
  ] ++ [
    UnitSpec {
      name = "depre",
      deps = ["dep2"],
      modules = [
        ModuleSpec "DepRe" "module DepRe where"
      ],
      reexports = [Reexport {unit = "dep2", moduleName = "Dep2"}],
      extraDbConf = [
        "depends: dep2"
      ]
    }
  ]
  where
    content i = "module Dep" ++ show i ++ " where"

homeUnits1 :: NonEmpty (String, [Text])
homeUnits1 =
  [
    ("unit1", ["dep1", "dep2"]),
    ("unit2", ["unit1", "depre"])
  ]

homeUnitCount :: Word
homeUnitCount = 2

addUnit ::
  GHC.Logger ->
  DynFlags ->
  [PackageFlag] ->
  [PackageDBFlag] ->
  String ->
  UnitEnv ->
  IO UnitEnv
addUnit logger dflags0 packageFlags packageDBFlags name unit_env = do
  let dflags = dflags0 {homeUnitId_ = unit, packageFlags, packageDBFlags}
#if defined(UNIT_INDEX)
  (dbs, unit_state, home_unit, _) <- initUnits logger dflags unit_env.ue_index Nothing allUnitIds
#else
  (dbs, unit_state, home_unit, _) <- initUnits logger dflags Nothing allUnitIds
#endif
  insertHomeUnit unit dflags dbs unit_state home_unit unit_env
  where
    allUnitIds = unitEnv_keys unit_env.ue_home_unit_graph

    unit = stringToUnitId name

withRenaming :: Unit -> (Unit, (Bool, [(ModuleName, ModuleName)]))
withRenaming unit =
  (unit, renaming unit.name)
  where
    renaming = \case
      "dep1" -> (False, [(mkModuleName "Dep1", mkModuleName "DepRenamed1")])
      _ -> (True, [])

synthDepFlag :: (Unit, (Bool, [(ModuleName, ModuleName)])) -> PackageFlag
synthDepFlag (Unit {uid}, rename) =
  uncurry (depFlagRename (unitIdString uid)) rename

showUnitState ::
  HomeUnitEnv ->
  UnitIndexQuery ->
  IO ()
showUnitState unit query =
  dbgp $
  hang (ppr (maybe (text "no id") (ppr . homeUnitId) unit.homeUnitEnv_home_unit)) 2 $
  vcat [
    ppr (unitProvider "Dep2"),
    ppr (origin "Dep2") <+> parens "available in unit1, reexport in unit2"
  ]
  where
    unitProvider n = lookupUniqMap providers (mkModuleName n)

    origin n = query.findOrigin state (mkModuleName n) False

    providers = state.moduleNameProvidersMap

    state = unit.homeUnitEnv_units

testUnitIndex ::
  NonEmpty (Unit, (Bool, [(ModuleName, ModuleName)])) ->
  NonEmpty (String, [Text]) ->
  HscEnv ->
  Ghc ()
testUnitIndex synthDeps homeUnits HscEnv {hsc_logger, hsc_dflags, hsc_unit_env} = do
  unit_env <- liftIO $ foldM add hsc_unit_env homeUnits
  for_ @[] ["unit1", "unit2"] \ name -> do
    let uid = stringToUnitId name
        unit = unitEnv_lookup uid unit_env.ue_home_unit_graph
    query <- liftIO $ unitIndexQuery unit_env.ue_index uid
    liftIO $ showUnitState unit query
  where
    add ue (name, deps) =
      addUnit hsc_logger dflags (catMaybes ((depFlagsByName Map.!?) <$> deps)) dbs name ue

    depFlagsByName =
      Map.fromList [(Text.pack name, synthDepFlag dep) | dep@(Unit {name}, (_, _)) <- toList synthDeps]

    dbs = [dbFlag db | (Unit {db}, _) <- toList synthDeps]

    dflags = gopt_set hsc_dflags Opt_HideAllPackages

test_unitIndex :: TestTree
test_unitIndex =
  unitTest "unit index" do
    testSession expectNoDiagnostics \ _ lower ->
      withProject (pure . testDepUnits 2) \ _conf units -> lower do
        ue_index <- liftIO newUnitIndex
        modifySession \ hsc_env -> do
          hsc_env {hsc_unit_env = hsc_env.hsc_unit_env {ue_index, ue_home_unit_graph = unitEnv_new []}}
        testUnitIndex (withRenaming <$> units) homeUnits1 =<< getSession
        pure (Just ())

#else

test_unitIndex :: TestTree
test_unitIndex =
  unitTest "unit index not available" do
    pure ()

#endif
