module CallbackHandleTest where

import Control.Concurrent (ThreadId, myThreadId)
import Data.IORef (IORef, atomicModifyIORef')
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

register :: IORef (Map ThreadId Int) -> Int -> IO ()
register registry task = do
  tid <- myThreadId
  atomicModifyIORef' registry \ m -> (Map.insert tid task m, ())
