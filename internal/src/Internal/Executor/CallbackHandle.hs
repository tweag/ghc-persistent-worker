-- | Handles backed by IO callbacks, and a way to install them as the process-global @stdout@ and @stderr@.
--
-- In contrast to redirecting the file descriptors (as @System.IO.Silently.hCapture@ does with a temporary file), the
-- callback runs synchronously in the thread that writes to the handle, so it can use 'myThreadId' to attribute the
-- output to a task, which allows concurrent evaluations to have their output captured separately.
module Internal.Executor.CallbackHandle where

import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, readMVar)
import qualified Data.ByteString as ByteString
import Data.ByteString (ByteString)
import Data.Word (Word8)
import Foreign.Ptr (Ptr, castPtr)
import GHC.IO.Buffer (newByteBuffer)
import GHC.IO.BufferedIO (BufferedIO (..), readBuf, readBufNonBlocking, writeBuf, writeBufNonBlocking)
import GHC.IO.Device (IODevice (..), IODeviceType (Stream), RawIO (..))
import GHC.IO.Encoding (getLocaleEncoding)
import GHC.IO.Handle.Internals (mkFileHandle)
import GHC.IO.Handle.Types (Handle (..), Handle__, Newline (..), NewlineMode (..))
import System.IO (BufferMode (..), IOMode (..), hFlush, hSetBuffering)

-- | A device whose reads and writes are delegated to callbacks.
data CallbackDevice =
  CallbackDevice {
    -- | Fill the buffer with at most the given number of bytes and return the number of bytes written; 0 is EOF.
    cbRead :: Ptr Word8 -> Int -> IO Int,
    -- | Consume the given number of bytes from the buffer.
    cbWrite :: Ptr Word8 -> Int -> IO ()
  }

instance RawIO CallbackDevice where
  read d p _ n = cbRead d p n
  readNonBlocking d p _ n = Just <$> cbRead d p n
  write d p _ n = cbWrite d p n
  writeNonBlocking d p _ n = cbWrite d p n >> pure n

instance IODevice CallbackDevice where
  ready _ _ _ = pure True
  close _ = pure ()
  devType _ = pure Stream

instance BufferedIO CallbackDevice where
  newBuffer _ = newByteBuffer 8192
  fillReadBuffer = readBuf
  fillReadBuffer0 = readBufNonBlocking
  flushWriteBuffer = writeBuf
  flushWriteBuffer0 = writeBufNonBlocking

-- | Create a handle from a device, using the locale encoding and no newline translation.
mkCallbackHandle :: CallbackDevice -> IOMode -> IO Handle
mkCallbackHandle dev mode = do
  enc <- getLocaleEncoding
  mkFileHandle dev "callback" mode (Just enc) (NewlineMode LF LF)

-- | A write-only handle that passes the (copied) encoded bytes of every write to the callback.
--
-- It is unbuffered, so the callback runs in the thread that performed the write, as opposed to whichever thread
-- happens to flush a shared buffer.
sinkHandle :: (ByteString -> IO ()) -> IO Handle
sinkHandle sink = do
  h <- mkCallbackHandle CallbackDevice {cbRead = \ _ _ -> pure 0, cbWrite} WriteMode
  hSetBuffering h NoBuffering
  pure h
  where
    cbWrite ptr len = ByteString.packCStringLen (castBytes ptr, len) >>= sink
    castBytes = castPtr

handleVar :: Handle -> MVar Handle__
handleVar = \case
  FileHandle _ var -> var
  DuplexHandle _ var _ -> var

-- | Make the target handle behave like the source handle, returning an action that restores the original state.
--
-- Unlike @hDuplicateTo@, this does not require the source to have a file descriptor; it replaces the internal state
-- of the target, which is what the process-global @stdout@ and @stderr@ names refer to.
installHandle :: Handle -> Handle -> IO (IO ())
installHandle target source = do
  hFlush target
  new <- readMVar (handleVar source)
  old <- modifyMVar (handleVar target) \ old -> pure (new, old)
  pure do
    hFlush target
    modifyMVar_ (handleVar target) \ _ -> pure old

-- | The process's original standard output handles, usable while the globals are redirected.
data Stdio =
  Stdio {
    out :: Handle,
    err :: Handle
  }

-- | Write bytes to a handle and flush it.
writeBytes :: Handle -> ByteString -> IO ()
writeBytes h bytes = ByteString.hPut h bytes >> hFlush h
