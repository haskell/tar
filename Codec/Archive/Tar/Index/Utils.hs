{-# LANGUAGE MagicHash, UnboxedTuples, BangPatterns, CPP #-}
module Codec.Archive.Tar.Index.Utils where

import Data.ByteString as BS
import Control.Exception (assert)

import Data.ByteString.Internal (ByteString(..), unsafeWithForeignPtr, accursedUnutterablePerformIO)
import GHC.Int (Int(..), Int32)
import GHC.Word (Word32(..), byteSwap32)
import Foreign.Storable (peek)
import GHC.Ptr (castPtr, plusPtr, Ptr)
import GHC.Exts
import GHC.IO (IO(..), unsafePerformIO)
import Data.Array.Base
import Data.Array.IO.Internals (unsafeFreezeIOUArray)
import Control.DeepSeq (NFData(..))
import GHC.Storable
import GHC.ByteOrder

#include <MachDeps.h>

-- | Construct a `UArray Word32 Word32` from a ByteString of 32bit big endian
-- words.
--
-- Note: If using `unsafePerformIO`, be sure to force the result of running the
-- IO action right away... (e.g. see calls to beToLe in StringTable)
beToLe :: (Integral i, Num i) => i
       -- ^ The total array length (the number of 32bit words in the array)
       -> BS.ByteString
       -- ^ The bytestring from which the UArray is constructed.
       -- The content must start in the first byte! (i.e. the meta-data words
       -- that shouldn't be part of the array, must have been dropped already)
       -> IO (UArray i Word32)
beToLe lenArr (BS fptr _) = do
  unsafeWithForeignPtr fptr $ \ptr -> do
    let ptr' = castPtr ptr :: Ptr Word32
        !(I# lenBytes#) = fromIntegral (lenArr * 4)

    -- In spirit, the following does this, but we can't use `newGenArray`
    -- because it only has been introduced in later versions of array:
    -- @@
    -- unsafeFreezeIOUArray =<<
    --   newGenArray (0, lenArr - 1) (\offset -> do
    --     byteSwap32 <$> peek (ptr' `plusPtr` (fromIntegral offset * 4)))
    -- @@
    IO $ \rw0 ->
      case newByteArray# lenBytes# rw0 of
        (# rw1, mba# #) ->

          let loop :: Int -> State# RealWorld -> State# RealWorld
              loop !offset st
                | offset < fromIntegral lenArr
                = let IO getV = readWord32OffPtrBE ptr' offset
                      !(I# o#) = offset
                   in case getV st of
                    (# st', W32# v# #) ->
                      loop (offset + 1) (writeWord32Array# mba# o# v# st')
                | otherwise = st

           in case unsafeFreezeByteArray# mba# (loop 0 rw1) of
             (# rw2, ba# #) -> (# rw2, UArray 0 (lenArr - 1) (fromIntegral lenArr) ba# #)

{-# SPECIALISE beToLe :: Word32 -> BS.ByteString -> IO (UArray Word32 Word32) #-}
{-# SPECIALISE beToLe :: Int32 -> BS.ByteString -> IO (UArray Int32 Word32) #-}

readInt32BE :: BS.ByteString -> Int -> Int32
readInt32BE bs i = fromIntegral (readWord32BE bs i)
{-# INLINE readInt32BE #-}

readWord32OffPtrBE :: Ptr Word32 -> Int -> IO Word32
readWord32OffPtrBE ptr i = do
#if defined(WORDS_BIGENDIAN)
  readWord32OffPtr ptr i
#else
  byteSwap32 <$> readWord32OffPtr ptr i
#endif

readWord32BE :: BS.ByteString -> Int -> Word32
readWord32BE (BS fptr len) i =
    assert (i >= 0 && i+3 <= len - 1) $
    accursedUnutterablePerformIO $
      unsafeWithForeignPtr fptr $ \ptr -> do
        readWord32OffPtrBE (castPtr ptr) i
{-# INLINE readWord32BE #-}
