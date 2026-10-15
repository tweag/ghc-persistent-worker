module A where

import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Foreign.C.Types (CInt (..))
import System.IO.Unsafe (unsafePerformIO)

-- | State of the process that runs the tests of this unit, which survives between tests if they share a process.
counter :: IORef Int
counter = unsafePerformIO (newIORef 0)
{-# noinline counter #-}

-- | Print the process ID, the parent's, and the number of tests this process has run so far.
report :: String -> IO ()
report name = do
  pid <- c_getpid
  ppid <- c_getppid
  count <- atomicModifyIORef' counter (\ n -> (n + 1, n + 1))
  let line = "EXECUTOR unit=A test=" ++ name ++ " pid=" ++ show pid ++ " ppid=" ++ show ppid ++ " count=" ++ show count
  putStrLn line
  appendFile "/tmp/buck-eval-executor.log" (line ++ "\n")

foreign import ccall unsafe "getpid" c_getpid :: IO CInt

foreign import ccall unsafe "getppid" c_getppid :: IO CInt
