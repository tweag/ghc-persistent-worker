module GhcWorker.Instrumentation where

import Common.Grpc (GrpcHandler (..))
import Control.Concurrent (MVar, modifyMVar, modifyMVar_, readMVar)
import Control.Concurrent.Chan (Chan, writeChan)
import Control.Exception (bracket_)
import GhcWorker.Grpc (mkStats)
import GhcWorker.LogEvents (instrumentLoggerWith)
import Internal.Log (dbg)
import Prelude hiding (log)
import Types.Api (Event (..), Events (..), ProcessStats, RequestId, Target, eventsForRequest, eventsNoop)
import Types.Log (Logger)
import Types.State (WorkerState)

-- | Rudimentary dummy state for instrumentation, counting concurrently compiling sessions.
data WorkerStatus =
  WorkerStatus {
    active :: Int,
    -- | Counter for request IDs, used to associate API events with each other.
    nextRequestId :: RequestId
  }

-- | Obtain a new ID for the current request.
startRequest :: MVar WorkerStatus -> IO RequestId
startRequest var =
  modifyMVar var \ ws@WorkerStatus {nextRequestId} ->
    pure (ws {nextRequestId = nextRequestId + 1}, nextRequestId)

-- | Callbacks passed to GHC request handlers that trigger instrumentation events.
data Hooks =
  Hooks {
    -- | Hook for publishing instrumentation events.
    events :: Events,

    -- | Forward the messages of a logger to the instrumentation events, using the given action to obtain the
    -- category, which is the current target.
    instrumentLogger :: IO String -> Logger -> Logger
  }

-- | Dummy implementation of 'Hooks'.
hooksNoop :: Hooks
hooksNoop =
  Hooks {
    events = eventsNoop,
    instrumentLogger = const id
  }

-- | The outcome of an eval request, as reported to the instrumentation.
data EvalReport =
  EvalReport {
    -- | An error message for a failure to evaluate, as opposed to a successful evaluation of a failing test.
    outcome :: Either String Bool,
    -- | The value that the evaluated expression produced, if it was shown.
    result :: Maybe String,
    -- | The RTS stats of the executor, if the evaluation ran in one.
    stats :: Maybe ProcessStats
  }

-- | The report for an evaluation that failed with an error message.
failedEval :: String -> EvalReport
failedEval err =
  EvalReport {outcome = Left err, result = Nothing, stats = Nothing}

-- | Publish an event that doesn't depend on the request.
publishEvent :: Hooks -> Event -> IO ()
publishEvent hooks event =
  hooks.events.publish \ (_ :: RequestId) -> event

-- | Run an evaluation, publishing 'CompileStart' and 'CompileEnd' events for the module under test.
-- The evaluation is a subprocess task if it runs in an executor.
reportEval :: Hooks -> Target -> Bool -> IO EvalReport -> IO EvalReport
reportEval hooks target process run = do
  hooks.events.publish \ requestId -> CompileStart {target, debuggable = False, process, requestId}
  report <- run
  hooks.events.publish \ requestId ->
    CompileEnd {
      target,
      exitCode = either (const 1) (\ ok -> if ok then 0 else 1) report.outcome,
      stderr = either id (const "") report.outcome,
      result = report.result,
      processStats = report.stats,
      noMain = False,
      requestId
    }
  pure report

-- | A request handler that is aware of instrumentation.
newtype InstrumentedHandler =
  InstrumentedHandler { create :: Hooks -> GrpcHandler }

-- | Register a newly started job by incrementing the active job count.
startJob ::
  MVar WorkerStatus ->
  IO ()
startJob var =
  modifyMVar_ var \ ws@WorkerStatus {active} -> do
    let new = active + 1
    dbg ("Starting job, now " ++ show new ++ " active")
    pure ws {active = new}

-- | Decrement the active job count.
finishJob ::
  MVar WorkerStatus ->
  IO ()
finishJob var = do
  modifyMVar_ var \ ws@WorkerStatus {active} -> do
    let new = active - 1
    dbg ("Finishing job, now " ++ show new ++ " active")
    pure ws {active = new}

-- | Run a 'GrpcHandler' with instrumentation enabled.
--
-- This consists of adapting the active job count and sending messages to the gRPC client running the instrumentation
-- app.
-- The handler is initialized by passing 'Hooks' to its constructor function, which contains callbacks for sending
-- additional messages.
withInstrumentation ::
  Chan Event ->
  MVar WorkerStatus ->
  MVar WorkerState ->
  InstrumentedHandler ->
  GrpcHandler
withInstrumentation instrChan status stateVar handler =
  GrpcHandler \ commandEnv argv -> do
    state <- readMVar stateVar
    bracket_ (startJob status) (finishJob status) do
      requestId <- startRequest status
      let hooks = Hooks {
            events = eventsForRequest requestId (writeChan instrChan),
            instrumentLogger = instrumentLoggerWith (writeChan instrChan)
          }
      result <- (handler.create hooks).run commandEnv argv
      stats <- mkStats state
      writeChan instrChan stats
      pure result

-- | Construct a 'GrpcHandler' by passing functioning 'Hooks' to an 'InstrumentedHandler' if the third argument contains
-- 'Just' a message channel, or passing no-op 'Hooks' otherwise.
toGrpcHandler ::
  InstrumentedHandler ->
  MVar WorkerStatus ->
  MVar WorkerState ->
  Maybe (Chan Event) ->
  GrpcHandler
toGrpcHandler createHandler status stateVar = \case
  Nothing -> createHandler.create hooksNoop
  Just instrChan -> withInstrumentation instrChan status stateVar createHandler
