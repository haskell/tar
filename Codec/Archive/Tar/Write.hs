{-# LANGUAGE PackageImports #-}
{-# OPTIONS_HADDOCK hide #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Codec.Archive.Tar.Write
-- Copyright   :  (c) 2007 Bjorn Bringert,
--                    2008 Andrea Vezzosi,
--                    2008-2009 Duncan Coutts
-- License     :  BSD3
--
-- Maintainer  :  duncan@community.haskell.org
-- Portability :  portable
--
-----------------------------------------------------------------------------
module Codec.Archive.Tar.Write (write) where

import Codec.Archive.Tar.PackAscii
import Codec.Archive.Tar.Types

import Data.Bits
import Data.Char     (chr,ord)
import Data.Int
import Data.List     (foldl')
import Data.Monoid   (mempty)
import Numeric       (showOct)

import qualified Data.ByteString             as BS
import qualified Data.ByteString.Char8       as BS.Char8
import qualified Data.ByteString.Lazy        as LBS
import qualified Data.ByteString.Lazy.Char8  as LBS.Char8
import "os-string" System.OsString.Posix (PosixString)
import qualified "os-string" System.OsString.Posix as PS

-- | Create the external representation of a tar archive by serialising a list
-- of tar entries.
--
-- * The conversion is done lazily.
--
write :: [Entry] -> LBS.ByteString
write es = LBS.concat $ map putEntry es ++ [LBS.replicate (512*2) 0]

putEntry :: Entry -> LBS.ByteString
putEntry entry = case entryContent entry of
  NormalFile       content size
    -- size field is 12 bytes long, so in octal format (see 'putOct')
    -- it can hold numbers up to 8Gb
    | size >= 1 `shiftL` (3 * (12 -1))
    , entryFormat entry == V7Format
    -> error "putEntry: support for files over 8Gb is a Ustar extension"
    | otherwise -> LBS.concat [ header, content, padding size ]
  OtherEntryType 'K' _ _
    | entryFormat entry /= GnuFormat -> error "putEntry: long symlink support is a GNU extension"
  OtherEntryType 'L' _ _
    | entryFormat entry /= GnuFormat -> error "putEntry: long filename support is a GNU extension"
  OtherEntryType _ content size -> LBS.concat [ header, content, padding size ]
  _                             -> header
  where
    header       = putHeader entry
    padding size = LBS.replicate paddingSize 0
      where paddingSize = fromIntegral (negate size `mod` 512)

putHeader :: Entry -> LBS.ByteString
putHeader entry =
     LBS.fromStrict
   $ BS.take 148 block
  <> putOct 7 checksum
  <> BS.Char8.cons ' ' (BS.drop 156 block)
  where
    block    = putHeaderNoChkSum entry
    checksum = BS.Char8.foldl' (\x y -> x + ord y) 0 block

putHeaderNoChkSum :: Entry -> BS.ByteString
putHeaderNoChkSum Entry {
    entryTarPath     = TarPath name prefix,
    entryContent     = content,
    entryPermissions = permissions,
    entryOwnership   = ownership,
    entryTime        = modTime,
    entryFormat      = format
  } =

  BS.concat
    [ putPosixString 100 name
    , putOct       8 permissions
    , putOct       8 $ ownerId ownership
    , putOct       8 $ groupId ownership
    , numField    12 contentSize
    , putOct      12 modTime
    , BS.Char8.replicate 8 ' ' -- dummy checksum
    , putChar8       typeCode
    , putPosixString 100 linkTarget
    ] <>
  case format of
  V7Format    ->
      BS.Char8.replicate 255 '\NUL'
  UstarFormat -> BS.Char8.concat
    [ putBString   8 ustarMagic
    , putString   32 $ ownerName ownership
    , putString   32 $ groupName ownership
    , putOct       8 deviceMajor
    , putOct       8 deviceMinor
    , putPosixString 155 prefix
    , BS.Char8.replicate   12 '\NUL'
    ]
  GnuFormat -> BS.Char8.concat
    [ putBString   8 gnuMagic
    , putString   32 $ ownerName ownership
    , putString   32 $ groupName ownership
    , putGnuDev    8 deviceMajor
    , putGnuDev    8 deviceMinor
    , putPosixString 155 prefix
    , BS.Char8.replicate   12 '\NUL'
    ]
  where
    numField :: FieldWidth -> Int64 -> BS.Char8.ByteString
    numField w n
      | n >= 0 && n < 1 `shiftL` (3 * (w - 1))
      = putOct w n
      | otherwise
      = putLarge w n

    (typeCode, contentSize, linkTarget,
     deviceMajor, deviceMinor) = case content of
       NormalFile      _ size            -> ('0' , size, mempty, 0,     0)
       Directory                         -> ('5' , 0,    mempty, 0,     0)
       SymbolicLink    (LinkTarget link) -> ('2' , 0,    link,   0,     0)
       HardLink        (LinkTarget link) -> ('1' , 0,    link,   0,     0)
       CharacterDevice major minor       -> ('3' , 0,    mempty, major, minor)
       BlockDevice     major minor       -> ('4' , 0,    mempty, major, minor)
       NamedPipe                         -> ('6' , 0,    mempty, 0,     0)
       OtherEntryType  code _ size       -> (code, size, mempty, 0,     0)

    putGnuDev w n = case content of
      CharacterDevice _ _ -> putOct w n
      BlockDevice     _ _ -> putOct w n
      _                   -> BS.Char8.replicate w '\NUL'

ustarMagic, gnuMagic :: BS.ByteString
ustarMagic = BS.Char8.pack "ustar\NUL00"
gnuMagic   = BS.Char8.pack "ustar  \NUL"

-- * TAR format primitive output

type FieldWidth = Int

putBString :: FieldWidth -> BS.ByteString -> BS.ByteString
putBString n s = BS.take n s <> BS.Char8.replicate (n - BS.length s) '\NUL'

putPosixString :: FieldWidth -> PosixString -> BS.ByteString
putPosixString n s = posixToByteString (PS.take n s) <> BS.Char8.replicate (n - PS.length s) '\NUL'

putString :: FieldWidth -> String -> BS.ByteString
putString n s = BS.take n (packAscii s) <> BS.Char8.replicate (n - length s) '\NUL'

{-# SPECIALISE putLarge :: FieldWidth -> Int64 -> BS.ByteString #-}
putLarge :: (Bits a, Integral a) => FieldWidth -> a -> BS.ByteString
putLarge n0 x0 = BS.Char8.pack $ '\x80' : reverse (go (n0-1) x0)
  where go 0 _ = []
        go n x = chr (fromIntegral (x .&. 0xff)) : go (n-1) (x `shiftR` 8)

putOct :: (Integral a, Show a) => FieldWidth -> a -> BS.ByteString
putOct n x =
  let octStr = BS.take (n-1) $ BS.Char8.pack $ showOct x ""
   in BS.Char8.replicate (n - BS.length octStr - 1) '0'
   <> octStr
   <> putChar8 '\NUL'

putChar8 :: Char -> BS.ByteString
putChar8 = BS.Char8.singleton
