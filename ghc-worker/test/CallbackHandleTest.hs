module CallbackHandleTest where

import Control.Concurrent (ThreadId, myThreadId)
import qualified Data.ByteString as ByteString
import Data.IORef (IORef, atomicModifyIORef')
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Types.Api (OutputStream (..))

type Output = Map (Int, OutputStream) [ByteString.ByteString]

register :: IORef (Map ThreadId Int) -> Int -> IO ()
register registry task = do
  tid <- myThreadId
  atomicModifyIORef' registry \ m -> (Map.insert tid task m, ())
