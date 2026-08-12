-- | Bundled arguments that flow through the build pipeline.
module GhcServer.Data.BuildEnv where

import Control.Concurrent.Chan (Chan)
import Control.Concurrent.MVar (MVar)
import GhcServer.Data.BuildEvent (BuildEvents)
import GhcServer.Data.Unit (Project)
import System.OsPath (OsPath)
import Types.Args (Args)
import Types.Instrument (Event)
import Types.Log (Logger)
import Types.State (WorkerState)

-- | Common arguments threaded from 'runBuild' through dispatch to worker adapters.
data BuildEnv =
  BuildEnv {
    baseArgs :: Args,
    projectRoot :: OsPath,
    outputDir :: OsPath,
    tmpDir :: OsPath,
    stateVar :: MVar WorkerState,
    project :: Project,
    log :: Logger,
    events :: BuildEvents,
    -- | Channel for pushing 'Types.Instrument.Event's to the instrument UI, if the @instrument@ feature is
    -- enabled. 'Nothing' when the feature is disabled, avoiding the cost of constructing events that nobody
    -- consumes.
    instrChan :: Maybe (Chan Event),
    -- | Memoized result of building the project's external Cabal dependencies into the store, shared by
    -- all units so the build only runs once per server lifetime. 'Nothing' until first requested.
    extDepsDb :: MVar (Maybe (Either String FilePath))
  }
