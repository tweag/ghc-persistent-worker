module Main where

import Control.Exception (Exception (..), SomeException (..), try)
import Control.Monad.IO.Class (MonadIO (liftIO))
import GhcWorker.Executor.Child (runPlanExecutor)
import GhcWorker.Run (parseCliArgs, runWorker)
import System.Environment (getArgs)
import System.IO (BufferMode (..), hPutStrLn, hSetBuffering, stderr, stdout)
import System.OsPath.Extra (toOsPath)

dbg :: MonadIO m => String -> m ()
dbg = liftIO . hPutStrLn stderr

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  getArgs >>= \case
    -- Relaunch as a persistent executor for bytecode plans, see 'GhcWorker.Executor.Client.spawnExecutorWith'.
    ["executor", "--socket", socket] -> runPlanExecutor (toOsPath socket)
    _ ->
      try (runWorker =<< parseCliArgs) >>= \case
        Right () ->
          dbg "Worker terminated without cancellation."
        Left (err :: SomeException) ->
          dbg ("Worker terminated with exception: " ++ displayException err)
