-- | Shared-memory transport for executor bytecode payloads, enabled by 'Types.FeatureFlags.FeatureSharedPayloads'.
--
-- The parent appends each payload (a module's bytecode in the format of a @.gbc@ file) once to a segment file on a
-- @tmpfs@ (@\/dev\/shm@), whose pages are the shared memory, and sends executors 'SegmentSlice's instead of file
-- paths.
-- Executors map the whole segment read-only and decode payloads directly from the mapping, without reading files.
-- Since the segment is append-only, a mapping stays valid for all slices it covers; slices beyond it trigger a
-- remap.
--
-- This is a testbed for a future GHC API that loads bytecode from memory without decoding it into the heap; for now,
-- the child still deserializes the bytes.
-- The segment is never compacted, so it grows with every distinct payload until the server exits.
module Internal.Executor.PayloadArena where

import qualified Data.ByteString as ByteString
import Data.ByteString (ByteString)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import GHC.Fingerprint (Fingerprint)
import System.Directory (removePathForcibly)
import System.IO (IOMode (WriteMode), hClose, hFlush, openBinaryFile)
import System.IO.MMap (mmapFileByteString)
import System.Posix.Process (getProcessID)
import Types.State.Executor (PayloadArena (..), SegmentSlice (..))

-- | The segment of this process in @\/dev\/shm@, removed by 'GhcServer.Run.runServer' on exit.
payloadSegmentPath :: IO FilePath
payloadSegmentPath = do
  pid <- getProcessID
  pure ("/dev/shm/ghc-server-payloads-" ++ show pid)

-- | Create an empty segment at the given path, replacing an existing file.
openArena :: FilePath -> IO PayloadArena
openArena segment = do
  handle <- openBinaryFile segment WriteMode
  pure PayloadArena {segment, handle, end = 0, slices = Map.empty}

-- | Reuse the arena if it is at the given path, otherwise close it and create a new one.
ensureArena :: FilePath -> Maybe PayloadArena -> IO PayloadArena
ensureArena segment = \case
  Just arena | arena.segment == segment -> pure arena
  old -> do
    mapM_ (hClose . (.handle)) old
    openArena segment

-- | Look up the payload with the given hash, or append the bytes produced by the action.
appendPayload :: PayloadArena -> Fingerprint -> IO ByteString -> IO (PayloadArena, SegmentSlice)
appendPayload arena hash produce =
  case Map.lookup hash arena.slices of
    Just slice -> pure (arena, slice)
    Nothing -> do
      bytes <- produce
      ByteString.hPut arena.handle bytes
      hFlush arena.handle
      let slice = SegmentSlice {segment = arena.segment, offset = arena.end, size = ByteString.length bytes}
      pure (arena {end = arena.end + slice.size, slices = Map.insert hash slice arena.slices}, slice)

removeSegment :: FilePath -> IO ()
removeSegment = removePathForcibly

-- | Child-side read-only mappings of whole segments.
-- A mapping is unmapped when it and all slices of it are garbage collected.
newtype SegmentMappings = SegmentMappings (IORef (Map FilePath ByteString))

newSegmentMappings :: IO SegmentMappings
newSegmentMappings = SegmentMappings <$> newIORef Map.empty

-- | The bytes of a slice, mapping the segment if it isn't mapped yet or has grown past the current mapping.
sliceBytes :: SegmentMappings -> SegmentSlice -> IO ByteString
sliceBytes (SegmentMappings ref) slice = do
  mapping <- Map.lookup slice.segment <$> readIORef ref >>= \case
    Just bytes | covers bytes -> pure bytes
    _ -> do
      bytes <- mmapFileByteString slice.segment Nothing
      atomicModifyIORef' ref \ mappings -> (Map.insert slice.segment bytes mappings, ())
      pure bytes
  if covers mapping
  then pure (ByteString.take slice.size (ByteString.drop slice.offset mapping))
  else ioError (userError ("Payload segment " ++ slice.segment ++ " is shorter than " ++ show (end slice)))
  where
    covers bytes = ByteString.length bytes >= end slice

    end SegmentSlice {offset, size} = offset + size
