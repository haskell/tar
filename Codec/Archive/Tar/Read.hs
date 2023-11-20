{-# LANGUAGE CPP, DeriveDataTypeable, BangPatterns #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Codec.Archive.Tar.Read
-- Copyright   :  (c) 2007 Bjorn Bringert,
--                    2008 Andrea Vezzosi,
--                    2008-2009 Duncan Coutts,
--                    2011 Max Bolingbroke
-- License     :  BSD3
--
-- Maintainer  :  duncan@community.haskell.org
-- Portability :  portable
--
-----------------------------------------------------------------------------
module Codec.Archive.Tar.Read (read, FormatError(..)) where

import Codec.Archive.Tar.Types

import Data.Char     (ord)
import Data.Int      (Int64)
import Data.Bits     (Bits(shiftL))
import Control.Exception (Exception(..))
import Data.Typeable (Typeable)
import Control.Applicative
import Control.Monad
import Control.DeepSeq

import qualified Data.ByteString        as BS
import qualified Data.ByteString.Char8  as BS.Char8
import qualified Data.ByteString.Unsafe as BS
import qualified Data.ByteString.Lazy   as LBS

import Prelude hiding (read)

-- | Errors that can be encountered when parsing a Tar archive.
data FormatError
  = TruncatedArchive
  | ShortTrailer
  | BadTrailer
  | TrailingJunk
  | ChecksumIncorrect
  | NotTarFormat
  | UnrecognisedTarFormat
  | HeaderBadNumericEncoding
  deriving (Eq, Show, Typeable)

instance Exception FormatError where
  displayException TruncatedArchive         = "truncated tar archive"
  displayException ShortTrailer             = "short tar trailer"
  displayException BadTrailer               = "bad tar trailer"
  displayException TrailingJunk             = "tar file has trailing junk"
  displayException ChecksumIncorrect        = "tar checksum error"
  displayException NotTarFormat             = "data is not in tar format"
  displayException UnrecognisedTarFormat    = "tar entry not in a recognised format"
  displayException HeaderBadNumericEncoding = "tar header is malformed (bad numeric encoding)"

instance NFData    FormatError where
  rnf !_ = () -- enumerations are fully strict by construction

-- | Convert a data stream in the tar file format into an internal data
-- structure. Decoding errors are reported by the 'Fail' constructor of the
-- 'Entries' type.
--
-- * The conversion is done lazily.
--
read :: LBS.ByteString -> Entries FormatError
read = unfoldEntries getEntry

getEntry :: LBS.ByteString -> Either FormatError (Maybe (Entry, LBS.ByteString))
getEntry bs
  | BS.length header < 512 = Left TruncatedArchive

  -- Tar files end with at least two blocks of all '0'. Checking this serves
  -- two purposes. It checks the format but also forces the tail of the data
  -- which is necessary to close the file if it came from a lazily read file.
  --
  -- It's tempting to fall into trailer parsing as soon as LBS.head bs == '\0',
  -- because, if interpreted as an 'Entry', it means that 'entryTarPath' is an empty
  -- string. Yet it's not a concern of this function: parse it as an 'Entry'
  -- and let further pipeline such as 'checkEntrySecurity' deal with it. After all,
  -- it might be a format extension with unknown semantics. Such somewhat malformed
  -- archives do exist in the wild, see https://github.com/haskell/tar/issues/73.
  --
  -- Only if an entire block is null, we assume that we are parsing a trailer.
  | LBS.all (== 0) (LBS.take 512 bs) = case LBS.splitAt 1024 bs of
      (end, trailing)
        | LBS.length end /= 1024        -> Left ShortTrailer
        | not (LBS.all (== 0) end)      -> Left BadTrailer
        | not (LBS.all (== 0) trailing) -> Left TrailingJunk
        | otherwise                     -> Right Nothing

  | otherwise  = do

  case (chksum_, format_) of
    (Right chksum, _ ) | correctChecksum header chksum -> return ()
    (Right _, Right _) -> Left ChecksumIncorrect
    _                  -> Left NotTarFormat

  -- These fields are partial, have to check them
  format   <- format_;   mode     <- mode_;
  uid      <- uid_;      gid      <- gid_;
  size     <- size_;     mtime    <- mtime_;
  devmajor <- devmajor_; devminor <- devminor_;

  let content = LBS.take size (LBS.drop 512 bs)
      padding = (512 - size) `mod` 512
      bs'     = LBS.drop (512 + size + padding) bs

      entry = Entry {
        entryTarPath     = TarPath name prefix,
        entryContent     = case typecode of
                   '\0' -> NormalFile      content size
                   '0'  -> NormalFile      content size
                   '1'  -> HardLink        (LinkTarget linkname)
                   '2'  -> SymbolicLink    (LinkTarget linkname)
                   _ | format == V7Format
                        -> OtherEntryType  typecode content size
                   '3'  -> CharacterDevice devmajor devminor
                   '4'  -> BlockDevice     devmajor devminor
                   '5'  -> Directory
                   '6'  -> NamedPipe
                   '7'  -> NormalFile      content size
                   _    -> OtherEntryType  typecode content size,
        entryPermissions = mode,
        entryOwnership   = Ownership (BS.Char8.unpack uname)
                                     (BS.Char8.unpack gname) uid gid,
        entryTime        = mtime,
        entryFormat      = format
    }

  return (Just (entry, bs'))

  where
   header = LBS.toStrict (LBS.take 512 bs)

   name       = getString   0 100 header
   mode_      = getOct    100   8 header
   uid_       = getOct    108   8 header
   gid_       = getOct    116   8 header
   size_      = getOct    124  12 header
   mtime_     = getOct    136  12 header
   chksum_    = getOct    148   8 header
   typecode   = getByte   156     header
   linkname   = getString 157 100 header
   magic      = getChars  257   8 header
   uname      = getString 265  32 header
   gname      = getString 297  32 header
   devmajor_  = getOct    329   8 header
   devminor_  = getOct    337   8 header
   prefix     = getString 345 155 header
-- trailing   = getBytes  500  12 header

   format_
     | magic == ustarMagic = return UstarFormat
     | magic == gnuMagic   = return GnuFormat
     | magic == v7Magic    = return V7Format
     | otherwise           = Left UnrecognisedTarFormat

v7Magic, ustarMagic, gnuMagic :: BS.ByteString
v7Magic    = BS.Char8.pack "\0\0\0\0\0\0\0\0"
ustarMagic = BS.Char8.pack "ustar\NUL00"
gnuMagic   = BS.Char8.pack "ustar  \NUL"

correctChecksum :: BS.ByteString -> Int -> Bool
correctChecksum header checksum = checksum == checksum'
  where
    -- sum of all 512 bytes in the header block,
    -- treating each byte as an 8-bit unsigned value
    sumchars  = BS.foldl' (\x y -> x + fromIntegral y) 0
    -- treating the 8 bytes of chksum as blank characters.
    checksum' = sumchars (BS.take 148 header)
              + 256 -- 256 = sumchars (BS.Char8.replicate 8 ' ')
              + sumchars (BS.drop 156 header)

-- * TAR format primitive input

{-# SPECIALISE getOct :: Int -> Int -> BS.ByteString -> Either FormatError Int   #-}
{-# SPECIALISE getOct :: Int -> Int -> BS.ByteString -> Either FormatError Int64 #-}
getOct :: (Integral a, Bits a) => Int -> Int -> BS.ByteString -> Either FormatError a
getOct off len = parseOct . getBytes off len
  where
    -- As a star extension, octal fields can hold a base-256 value if the high
    -- bit of the initial character is set. The initial character can be:
    --   0x80 ==> trailing characters hold a positive base-256 value
    --   0xFF ==> trailing characters hold a negative base-256 value
    --
    -- In both cases, there won't be a trailing NUL/space.
    --
    -- GNU tar seems to contain a half-implementation of code that deals with
    -- extra bits in the first character, but I don't think it works and the
    -- docs I can find on star seem to suggest that these will always be 0,
    -- which is what I will assume.
    parseOct s | BS.head s == 128 = return (readBytes (BS.tail s))
               | BS.head s == 255 = return (negate (readBytes (BS.tail s)))
    parseOct s
      | BS.null stripped = return 0
      | otherwise = case readOct stripped of
        Just x  -> return x
        Nothing -> Left HeaderBadNumericEncoding
     where
      stripped = BS.Char8.takeWhile (\c -> c /= '\NUL' && c /= ' ')
               $ BS.Char8.dropWhile (== ' ') s

    readBytes :: (Integral a, Bits a) => BS.ByteString -> a
    readBytes = BS.foldl' (\acc x -> acc `shiftL` 8 + fromIntegral x) 0

getBytes :: Int -> Int -> BS.ByteString -> BS.ByteString
getBytes off len = BS.take len . BS.drop off

getByte :: Int -> BS.ByteString -> Char
getByte off bs = BS.Char8.index bs off

getChars :: Int -> Int -> BS.ByteString -> BS.ByteString
getChars = getBytes

getString :: Int -> Int -> BS.ByteString -> BS.ByteString
getString off len = BS.copy . BS.Char8.takeWhile (/='\0') . getBytes off len

{-# SPECIALISE readOct :: BS.ByteString -> Maybe Int   #-}
{-# SPECIALISE readOct :: BS.ByteString -> Maybe Int64 #-}
readOct :: Integral n => BS.ByteString -> Maybe n
readOct = go 0 0
  where
    go :: Integral n => Int -> n -> BS.ByteString -> Maybe n
    go !i !n !bs = case BS.uncons bs of
      Nothing -> if i == 0 then Nothing else Just n
      Just (w, tl)
        | w >= 0x30 && w <= 0x39 ->
          go (i+1) (n * 8 + (fromIntegral w - 0x30)) tl
        | otherwise -> Nothing
