-- | Handles backed by IO callbacks, and a way to install them as @stdout@ and @stderr@.
-- This is used to associate stdio with the executor task that emitted it.
module Internal.Executor.CallbackHandle where

import Control.Concurrent.MVar (MVar)
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
import System.IO (BufferMode (..), IOMode (..), hSetBuffering)

-- | A device whose reads and writes are delegated to callbacks.
data CallbackDevice =
  CallbackDevice {
    -- | Fill the buffer with at most the given number of bytes and return the number of bytes written; 0 is EOF.
    cbRead :: Ptr Word8 -> Int -> IO Int,
    -- | Consume the given number of bytes from the buffer.
    cbWrite :: Ptr Word8 -> Int -> IO ()
  }

instance RawIO CallbackDevice where
  read CallbackDevice {cbRead} p _ n = cbRead p n
  readNonBlocking CallbackDevice {cbRead} p _ n = Just <$> cbRead p n
  write CallbackDevice {cbWrite} p _ n = cbWrite p n
  writeNonBlocking CallbackDevice {cbWrite} p _ n = n <$ cbWrite p n

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
--
-- TODO for performance reasons we might want to make this line buffered
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
