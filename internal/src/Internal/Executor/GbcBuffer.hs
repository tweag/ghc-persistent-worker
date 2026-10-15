{-# LANGUAGE CPP #-}

-- | In-memory variants of GHC's GBC serialization ('GHC.ByteCode.Serialize.writeBinByteCode' and
-- 'GHC.ByteCode.Serialize.readBinByteCode'), which only operate on files.
-- The format, including the header, is identical, so buffers and @.gbc@ files are interchangeable.
module Internal.Executor.GbcBuffer where

#if defined(GBC)

import Control.Exception (throwIO)
import Control.Monad (unless)
import Data.Bits (shiftL, (.|.))
import qualified Data.ByteString as ByteString
import Data.ByteString (ByteString)
import Data.Char (ord)
import Data.Word (Word32)
import GHC.ByteCode.Binary (OnDiskModuleByteCode (..), addBinNameReader, addBinNameWriter)
import GHC.ByteCode.Serialize (ModuleByteCode (..), decodeOnDiskModuleByteCode)
import GHC.Driver.Env (HscEnv (..))
import GHC.Iface.Binary (CompressionIFace (..), TraceBinIFace (..), getWithUserData, putWithUserData)
import GHC.Settings.Constants (hiVersion)
import GHC.Utils.Binary (
  FixedLengthEncoding (..),
  ReadBinHandle,
  WriteBinHandle,
  get,
  openBinMem,
  put_,
  unsafeUnpackBinBuffer,
  withBinBuffer,
  )

-- | The magic number at the start of GBC files.
gbcMagic :: Word32
gbcMagic = foldl (\ acc c -> shiftL acc 8 .|. fromIntegral (ord c)) 0 ("gbc0" :: String)

gbcVersion :: String
gbcVersion = show hiVersion

putHeader :: WriteBinHandle -> IO ()
putHeader bh = do
  put_ bh (FixedLengthEncoding gbcMagic)
  put_ bh gbcVersion

checkHeader :: ReadBinHandle -> IO ()
checkHeader bh = do
  FixedLengthEncoding magic <- get bh
  unless (magic == gbcMagic) (invalid ("magic " ++ show magic))
  version <- get bh
  unless (version == gbcVersion) (invalid ("version " ++ version))
  where
    invalid what = throwIO (userError ("GBC buffer has invalid " ++ what))

-- | Serialize bytecode in the format of a @.gbc@ file, including the contents of its foreign object files.
encodeByteCode :: ModuleByteCode -> IO ByteString
encodeByteCode mbc = do
  foreignObjects <- traverse ByteString.readFile mbc.gbc_foreign_files
  bh <- addBinNameWriter =<< openBinMem (1024 * 1024)
  putHeader bh
  putWithUserData QuietBinIFace NormalCompression bh OnDiskModuleByteCode {
    odgbc_module = mbc.gbc_module,
    odgbc_compiled_byte_code = mbc.gbc_compiled_byte_code,
    odgbc_foreign = foreignObjects,
    odgbc_hash = mbc.gbc_hash
  }
  withBinBuffer bh (pure . ByteString.copy)

-- | Deserialize bytecode from a buffer in the format of a @.gbc@ file, without copying the buffer.
-- Like 'GHC.ByteCode.Serialize.readBinByteCode', this writes the foreign objects to temporary files.
decodeByteCode :: HscEnv -> ByteString -> IO ModuleByteCode
decodeByteCode env bytes = do
  bh <- unsafeUnpackBinBuffer bytes
  checkHeader bh
  named <- addBinNameReader env.hsc_NC bh
  decodeOnDiskModuleByteCode env =<< getWithUserData env.hsc_NC named

#endif
