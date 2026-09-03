module Ghc.Ui.Event.Session where

import Brick (EventM, zoom)
import Control.Lens (each, filtered, (%%=), (%=), (<>=))
import Data.Foldable (for_)
import qualified Data.Map.Strict as Map
import Data.Text qualified as Text
import Data.Time (secondsToNominalDiffTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import qualified Ghc.Ui.Data.Log as Ui
import Ghc.Ui.Data.Name (Name)
import Ghc.Ui.Data.Project (ProjectUnit (..))
import qualified Ghc.Ui.Data.Session as Session
import Ghc.Ui.Data.Session (SessionEvent (..), SessionState (..), Worker (..))
import qualified Ghc.Ui.Data.Tasks as Tasks
import Ghc.Ui.Data.WorkerId (WorkerId)
import qualified Ghc.Ui.Event.Log as Log
import Ghc.Ui.Event.Project qualified as Project
import Ghc.Ui.Event.Tasks qualified as Tasks
import qualified Types.Api as Api
import Types.Api (Event (..), Target, UnitSummary (..))

stripEscSeqs :: String -> String
stripEscSeqs [] = []
stripEscSeqs ('\ESC' : '[' : xs) = stripEscSeqs (drop 1 (dropWhile (/= 'm') xs))
stripEscSeqs (x : xs) = x : stripEscSeqs xs

compileEnd ::
  Target ->
  Int ->
  String ->
  (Maybe String) ->
  Int ->
  EventM Name SessionState ()
compileEnd target exitCode stderr result requestId = do
  zoom #tasks do
    Tasks.completeTask requestId outcome
  zoom #project do
    mark target
  where
    content = stripEscSeqs stderr

    (outcome, mark) =
      if exitCode == 0
      then (Tasks.Succeeded result, Project.markBuilt)
      else (Tasks.Failed content, Project.markFailed)

handleApiEvent :: WorkerId -> Api.Event -> EventM Name SessionState ()
handleApiEvent worker = \case
  CompileStart {..} ->
    zoom #tasks do
      Tasks.addTask target worker debuggable requestId

  CompileEnd {..} ->
    compileEnd target exitCode stderr result requestId

  Stats {..} -> do
    #workers . each . filtered (\ w -> w.workerId == worker) . #stats %= \ Session.Stats {} ->
      Session.Stats {
        memory,
        gc_cpu_ns = gcCpuNs,
        cpu_ns = cpuNs
      }

  ProjectStructure {..} ->
    zoom #project do
      Project.load [ProjectUnit {unit = name, modules} | UnitSummary {name, modules} <- units]

  PhaseStart {phase, requestId} ->
    zoom #tasks do
      Tasks.phaseStart requestId phase

  PhaseEnd {durationMs, requestId} ->
    zoom #tasks do
      Tasks.phaseEnd requestId durationMs

  RequestCompleted {..} ->
    zoom #tasks do
      Tasks.addSeparator statusMessage

  LogMessage {..} ->
    #log %= Log.insertMessage Ui.LogMessage {
      category = Text.pack category,
      level = Text.pack level,
      message = Text.pack message,
      time = posixSecondsToUTCTime (secondsToNominalDiffTime time)
    }

  BytecodeSnapshot {..} ->
    zoom #project (Project.updateBytecode entries)

handleSessionEvent :: SessionEvent -> EventM Name SessionState ()
handleSessionEvent (ApiEvent worker event) = handleApiEvent worker event

removeWorker :: WorkerId -> EventM Name SessionState ()
removeWorker target = do
  removed <- #workers %%= Map.updateLookupWithKey (\ _ _ -> Nothing) target
  for_ removed \ worker ->
    #finishedWorkerStats <>= worker.stats {Session.memory = mempty}
