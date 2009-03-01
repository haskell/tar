-----------------------------------------------------------------------------
-- |
-- Module      :  Codec.Archive.Tar.Read
-- Copyright   :  (c) 2007 Bjorn Bringert,
--                    2008 Andrea Vezzosi,
--                    2008-2009 Duncan Coutts
-- License     :  BSD3
--
-- Maintainer  :  duncan@haskell.org
-- Portability :  portable
--
-----------------------------------------------------------------------------
module Codec.Archive.Tar.Read (read) where

import Codec.Archive.Tar.Types

import Data.Char     (ord)
import Data.Int      (Int64)
import Numeric       (readOct)
import Control.Monad (unless)

import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BS.Char8
import Data.ByteString.Lazy (ByteString)

import Prelude hiding (read)

-- | Convert a data stream in the tar file format into an internal data
-- structure. Decoding errors are reported by the 'Fail' constructor of the
-- 'Entries' type.
--
-- * The conversion is done lazily.
--
read :: ByteString -> Entries
read = unfoldEntries getEntry

getEntry :: ByteString -> Either String (Maybe (Entry, ByteString))
getEntry bs
  | BS.length header < 512 = Left "truncated tar archive"

  -- Tar files end with at least two blocks of all '0'. Checking this serves
  -- two purposes. It checks the format but also forces the tail of the data
  -- which is necessary to close the file if it came from a lazily read file.
  | BS.head bs == 0 = case BS.splitAt 1024 bs of
      (end, trailing)
        | BS.length end /= 1024        -> Left "short tar trailer"
        | not (BS.all (== 0) end)      -> Left "bad tar trailer"
        | not (BS.all (== 0) trailing) -> Left "tar file has trailing junk"
        | otherwise                    -> Right Nothing

  | otherwise  = partial $ do

  case (chksum_, format_) of
    (Ok chksum, _   ) | correctChecksum header chksum -> return ()
    (Ok _,      Ok _) -> fail "tar checksum error"
    _                 -> fail "data is not in tar format"

  -- These fields are partial, have to check them
  format   <- format_;   mode     <- mode_;
  uid      <- uid_;      gid      <- gid_;
  size     <- size_;     mtime    <- mtime_;
  devmajor <- devmajor_; devminor <- devminor_;

  let content = BS.take size (BS.drop 512 bs)
      padding = (512 - size) `mod` 512
      bs'     = BS.drop (512 + size + padding) bs

      entry = Entry {
        entryTarPath     = TarPath name prefix,
        entryContent     = case typecode of
                   '\0' -> NormalFile      content size
                   '0'  -> NormalFile      content size
                   '1'  -> HardLink        (LinkTarget linkname)
                   '2'  -> SymbolicLink    (LinkTarget linkname)
                   '3'  -> CharacterDevice devmajor devminor
                   '4'  -> BlockDevice     devmajor devminor
                   '5'  -> Directory
                   '6'  -> NamedPipe
                   '7'  -> NormalFile      content size
                   _    -> OtherEntryType  typecode content size,
        entryPermissions = mode,
        entryOwnership   = Ownership uname gname uid gid,
        entryTime        = mtime,
        entryFormat      = format
    }

  return (Just (entry, bs'))

  where
   header = BS.take 512 bs

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

   format_ = case magic of
    "\0\0\0\0\0\0\0\0" -> return V7Format
    "ustar\NUL00"      -> return UstarFormat
    "ustar  \NUL"      -> return GnuFormat
    _                  -> fail "tar entry not in a recognised format"

correctChecksum :: ByteString -> Int -> Bool
correctChecksum header checksum = checksum == checksum'
  where
    -- sum of all 512 bytes in the header block,
    -- treating each byte as an 8-bit unsigned value
    checksum' = BS.Char8.foldl' (\x y -> x + ord y) 0 header'
    -- treating the 8 bytes of chksum as blank characters.
    header'   = BS.concat [BS.take 148 header,
                           BS.Char8.replicate 8 ' ',
                           BS.drop 156 header]

-- * TAR format primitive input

getOct :: Integral a => Int64 -> Int64 -> ByteString -> Partial a
getOct off len = parseOct
               . BS.Char8.unpack
               . BS.Char8.takeWhile (\c -> c /= '\NUL' && c /= ' ')
               . BS.Char8.dropWhile (== ' ')
               . getBytes off len
  where
    parseOct "" = return 0
    parseOct ('\128':_) = fail "tar header uses non-standard number encoding"
    parseOct s  = case readOct s of
      [(x,[])] -> return x
      _        -> fail "tar header is malformatted (bad numeric encoding)"

getBytes :: Int64 -> Int64 -> ByteString -> ByteString
getBytes off len = BS.take len . BS.drop off

getByte :: Int64 -> ByteString -> Char
getByte off bs = BS.Char8.index bs off

getChars :: Int64 -> Int64 -> ByteString -> String
getChars off len = BS.Char8.unpack . getBytes off len

getString :: Int64 -> Int64 -> ByteString -> String
getString off len = BS.Char8.unpack . BS.Char8.takeWhile (/='\0') . getBytes off len

data Partial a = Error String | Ok a

partial :: Partial a -> Either String a
partial (Error msg) = Left msg
partial (Ok x)      = Right x

instance Monad Partial where
    return        = Ok
    Error m >>= _ = Error m
    Ok    x >>= k = k x
    fail          = Error
