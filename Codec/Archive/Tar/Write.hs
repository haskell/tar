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
import Data.Char     (chr)
import Data.Int
import Numeric       (showOct)

import qualified Data.ByteString             as BS
import qualified Data.ByteString.Char8       as BS.Char8
import qualified Data.ByteString.Lazy        as LBS
import Data.ByteString.Internal (c2w)

import qualified System.OsString.Posix as PS



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
putHeader entry = LBS.fromStrict $
     BS.take 148 block
  <> putOct 7 checksum
  <> BS.Char8.singleton ' '
  <> BS.drop 156 block
  where
    block    = putHeaderNoChkSum entry
    checksum = BS.foldl' (\x y -> x + fromIntegral y) (0 :: Int) block

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
    [ putPString 100 name
    , putOct       8 permissions
    , putOct       8 $ ownerId ownership
    , putOct       8 $ groupId ownership
    , numField    12 contentSize
    , putOct      12 modTime
    , BS.replicate    8 (c2w ' ') -- dummy checksum
    , putChar8'      typeCode
    , putPString 100 linkTarget
    ] <>
  case format of
    V7Format    ->
        BS.replicate 255 0
    UstarFormat -> BS.concat
      [ putBString   8 ustarMagic
      , putPString   32 $ ownerName ownership
      , putPString   32 $ groupName ownership
      , putOct       8 deviceMajor
      , putOct       8 deviceMinor
      , putPString 155 prefix
      , BS.replicate   12 0
      ]
    GnuFormat -> BS.concat
      [ putBString   8 gnuMagic
      , putPString   32 $ ownerName ownership
      , putPString   32 $ groupName ownership
      , putGnuDev    8 deviceMajor
      , putGnuDev    8 deviceMinor
      , putPString 155 prefix
      , BS.replicate   12 0
      ]
  where
    numField :: FieldWidth -> Int64 -> BS.ByteString
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
      _                   -> BS.replicate w 0

ustarMagic, gnuMagic :: BS.ByteString
ustarMagic = BS.Char8.pack "ustar\NUL00"
gnuMagic   = BS.Char8.pack "ustar  \NUL"

-- * TAR format primitive output

type FieldWidth = Int

putBString :: FieldWidth -> BS.ByteString -> BS.ByteString
putBString n s = BS.take n s <> BS.replicate (n - BS.length s) 0

putPString :: FieldWidth -> PS.PosixString -> BS.ByteString
putPString n s = (posixToByteString $ PS.take n s) <> BS.replicate (n - PS.length s) 0

{-# SPECIALISE putLarge :: FieldWidth -> Int64 -> BS.ByteString #-}
putLarge :: (Bits a, Integral a) => FieldWidth -> a -> BS.ByteString
putLarge n0 x0 = BS.Char8.pack $ '\x80' : reverse (go (n0-1) x0)
  where go 0 _ = []
        go n x = chr (fromIntegral (x .&. 0xff)) : go (n-1) (x `shiftR` 8)

putOct :: (Integral a, Show a) => FieldWidth -> a -> BS.ByteString
putOct n x =
  let octStr = BS.Char8.pack $ take (n-1) $ showOct x ""
   in BS.replicate (n - BS.length octStr - 1) (c2w '0')
      <> octStr
      <> BS.singleton 0

putChar8' :: Char -> BS.ByteString
putChar8' c = BS.Char8.pack [c]

