module GhcWorker.Instrumentation where

import Common.Grpc (GrpcHandler (..))
import Control.Concurrent (MVar, modifyMVar, modifyMVar_, readMVar)
import Control.Concurrent.Chan (Chan, writeChan)
import Control.Exception (bracket_)
import Data.Foldable (traverse_)
import Data.Int (Int32)
import GhcWorker.Grpc (mkStats, pushBytecodeState)
import Internal.Log (dbg)
import Prelude hiding (log)
import Types.BuckArgs (BuckArgs (..))
import Types.Instrument (Event (..))
import Types.State (WorkerState)
import Types.Target (TargetSpec, renderTargetSpec)

-- | Rudimentary dummy state for instrumentation, counting concurrently compiling sessions.
data WorkerStatus =
  WorkerStatus {
    active :: Int,
    -- | Monotonic counter allocating a unique 'Types.Instrument.Event' request id for each compilation job (one
    -- allocation per 'withInstrumentation' invocation), so the @instrument@ UI can match 'CompileStart'\/
    -- 'CompileEnd'\/'PhaseStart'\/'PhaseEnd' events to the exact task instance.
    nextRequestId :: Int
  }

-- | Allocate a fresh, worker-lifetime-unique request id (see 'WorkerStatus'\'s 'nextRequestId').
allocRequestId :: MVar WorkerStatus -> IO Int
allocRequestId var =
  modifyMVar var \ ws@WorkerStatus {nextRequestId} ->
    pure (ws {nextRequestId = nextRequestId + 1}, nextRequestId)

-- | Callbacks passed to GHC request handlers that trigger instrumentation events.
data Hooks =
  Hooks {
    -- | A module compilation is started.
    -- If it can be determined at this point, the argument contains the file name.
    -- This is not available in multiplexer mode.
    compileStart :: BuckArgs -> Maybe TargetSpec -> IO (),

    -- | A module compilation has finished.
    -- If the job was successful, the argument contains 'Just' the stderr lines and the exit code, otherwise 'Nothing'.
    compileFinish :: Maybe (Maybe TargetSpec, [String], Int32) -> IO (),

    -- | An arbitrary instrumentation event fires during compilation, currently used for
    -- 'Internal.Compile.Make.withPhaseEvents''s 'Types.Instrument.PhaseEvent's.
    emitEvent :: Event -> IO (),

    -- | Id allocated once per compilation job (see 'WorkerStatus'\'s 'nextRequestId'), included in every
    -- 'Types.Instrument.Event' this job emits ('CompileStart'\/'CompileEnd'\/'PhaseStart'\/'PhaseEnd') so the
    -- @instrument@ UI can match events to the exact task instance instead of matching by target text.
    requestId :: Int
  }

-- | Dummy implementation of 'Hooks'.
hooksNoop :: Hooks
hooksNoop =
  Hooks {
    compileStart = const (const (pure ())),
    compileFinish = const (pure ()),
    emitEvent = const (pure ()),
    requestId = 0
  }

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

-- | Construct a grapesy message for a "compilation started" event.
messageCompileStart :: BuckArgs -> TargetSpec -> Int -> Event
messageCompileStart _args target requestId =
  CompileStart
    { target = renderTargetSpec target
    , canDebug = True
    , requestId
    }

-- | Construct a grapesy message for a "compilation finished" event. @ghc-worker@ has no execute-task result
-- exfiltration story (see @GhcServer.Build.Execute@), so @result@ is always 'Nothing' here.
messageCompileEnd :: Maybe TargetSpec -> Int32 -> [String] -> Int -> Event
messageCompileEnd target exitCode output requestId =
  CompileEnd
    { target = maybe "" renderTargetSpec target
    , exitCode = fromIntegral exitCode
    , stderr = unlines output
    , result = Nothing
    , requestId
    }

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
    bracket_ (startJob status) (finishJob status) do
      requestId <- allocRequestId status
      let hooks = Hooks {
            compileStart = compileStart requestId,
            compileFinish = compileFinish requestId,
            emitEvent = writeChan instrChan,
            requestId
          }
      result <- (handler.create hooks).run commandEnv argv
      state <- readMVar stateVar
      stats <- mkStats state
      writeChan instrChan stats
      pushBytecodeState stateVar instrChan
      pure result
  where
    compileStart requestId =
      \ args -> traverse_ \ target ->
        writeChan instrChan $ messageCompileStart args target requestId

    compileFinish requestId =
      traverse_ \ (target, output, exitCode) -> do
        writeChan instrChan $ messageCompileEnd target exitCode output requestId

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
