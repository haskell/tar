-----------------------------------------------------------------------------
-- |
-- Module      :  Codec.Archive.Tar.Write
-- Copyright   :  (c) 2007 Bjorn Bringert,
--                    2008 Andrea Vezzosi,
--                    2008-2009 Duncan Coutts
-- License     :  BSD3
--
-- Maintainer  :  duncan@haskell.org
-- Portability :  portable
--
-----------------------------------------------------------------------------
module Codec.Archive.Tar.Write (write) where

import Codec.Archive.Tar.Types

import Data.Char     (ord)
import Data.List     (foldl')
import Numeric       (showOct)

import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BS.Char8
import Data.ByteString.Lazy (ByteString)


-- | Create the external representation of a tar archive by serialising a list
-- of tar entries.
--
-- The conversion is done lazily.
--
write :: [Entry] -> ByteString
write es = BS.concat $ map putEntry es ++ [BS.replicate (512*2) 0]

putEntry :: Entry -> ByteString
putEntry entry = BS.concat [ header, content, padding ]
  where
    header  = putHeader entry
    content = fileContent entry
    padding = BS.replicate paddingSize 0
    paddingSize = fromIntegral $ negate (fileSize entry) `mod` 512

putHeader :: Entry -> ByteString
putHeader entry =
     BS.Char8.pack $ take 148 block
  ++ putOct 7 checksum
  ++ ' ' : drop 156 block
--  ++ putOct 8 checksum
--  ++ drop 156 block
  where
    block    = putHeaderNoChkSum entry
    checksum = foldl' (\x y -> x + ord y) 0 block

putHeaderNoChkSum :: Entry -> String
putHeaderNoChkSum entry = concat
    [ putString  100 $ name
    , putOct       8 $ fileMode entry
    , putOct       8 $ ownerId entry
    , putOct       8 $ groupId entry
    , putOct      12 $ fileSize entry
    , putOct      12 $ modTime entry
    , fill         8 $ ' ' -- dummy checksum
    , putChar8       $ toFileTypeCode (fileType entry)
    , putString  100 $ linkTarget entry
    ] ++
  case headerExt entry of
  V7Header          ->
      fill 255 '\NUL'
  ext@UstarHeader {}-> concat
    [ putString    8 $ "ustar\NUL00"
    , putString   32 $ ownerName ext
    , putString   32 $ groupName ext
    , putOct       8 $ deviceMajor ext
    , putOct       8 $ deviceMinor ext
    , putString  155 $ prefix
    , fill        12 $ '\NUL'
    ]
  ext@GnuHeader {} -> concat
    [ putString    8 $ "ustar  \NUL"
    , putString   32 $ ownerName ext
    , putString   32 $ groupName ext
    , putGnuDev    8 $ deviceMajor ext
    , putGnuDev    8 $ deviceMinor ext
    , putString  155 $ prefix
    , fill        12 $ '\NUL'
    ]
  where
    TarPath name prefix = filePath entry
    putGnuDev w n = case fileType entry of
      CharacterDevice -> putOct w n
      BlockDevice     -> putOct w n
      _               -> replicate w '\NUL'


-- * TAR format primitive output

type FieldWidth = Int

putString :: FieldWidth -> String -> String
putString n s = take n s ++ fill (n - length s) '\NUL'

putOct :: Integral a => FieldWidth -> a -> String
putOct n x =
  let octStr = take (n-1) $ showOct x ""
   in fill (n - length octStr - 1) '0'
   ++ octStr
   ++ putChar8 '\NUL'

putChar8 :: Char -> String
putChar8 c = [c]

fill :: FieldWidth -> Char -> String
fill n c = replicate n c
