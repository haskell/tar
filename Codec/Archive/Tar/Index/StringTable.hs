{-# LANGUAGE CPP, BangPatterns, DeriveDataTypeable #-}

module Codec.Archive.Tar.Index.StringTable (

    StringTable(..),
    lookup,
    index,
    construct,

#ifdef TESTS
    prop_valid,
#endif
 ) where

import Data.Typeable (Typeable)

import Prelude hiding (lookup)
import qualified Data.List as List
import Data.Word (Word32)
import Data.Int  (Int32)

import qualified Data.Array.Unboxed as A
import           Data.Array.Unboxed ((!))
import qualified Data.ByteString.Char8  as BS
import qualified Data.ByteString.Unsafe as BS



-- | An effecient mapping from strings to a dense set of integers.
--
data StringTable id = StringTable
                        {-# UNPACK #-} !BS.ByteString          -- all the strings concatenated
                        {-# UNPACK #-} !(A.UArray Int Word32)  -- offset table
  deriving (Eq, Show, Typeable)

-- | Look up a string in the token table. If the string is present, return
-- its corresponding index.
--
lookup :: Enum id => StringTable id -> BS.ByteString -> Maybe id
lookup (StringTable bs offsets) str =
    binarySearch 0 (topBound-1) str
  where
    (0, topBound) = A.bounds offsets

    binarySearch !a !b !key
      | a > b     = Nothing
      | otherwise = case compare key (index' bs offsets mid) of
          LT -> binarySearch a (mid-1) key
          EQ -> Just (toEnum mid)
          GT -> binarySearch (mid+1) b key
      where mid = (a + b) `div` 2

index' :: BS.ByteString -> A.UArray Int Word32 -> Int -> BS.ByteString
index' bs offsets i = BS.unsafeTake len . BS.unsafeDrop start $ bs
  where
    start, end, len :: Int
    start = fromIntegral (offsets ! i)
    end   = fromIntegral (offsets ! (i+1))
    len   = end - start


-- | Given the index of a string in the table, return the string.
--
index :: Enum id => StringTable id -> id -> BS.ByteString
index (StringTable bs offsets) = index' bs offsets . fromEnum


-- | Given a list of strings, construct a 'StringTable' mapping those strings
-- to a dense set of integers.
--
construct :: Enum id => [BS.ByteString] -> StringTable id
construct strs = StringTable bs tbl
  where
    bs      = BS.concat strs'
    tbl     = A.array (0, length strs') (zip [0..] offsets)
    offsets = scanl (\off str -> off + fromIntegral (BS.length str)) 0 strs'
    strs'   = map head . List.group . List.sort $ strs


#ifdef TESTS

prop_valid :: [BS.ByteString] -> Bool
prop_valid strs =
     all lookupIndex (enumStrings tbl)
  && all indexLookup (enumIds tbl)

  where
    tbl :: StringTable Int
    tbl = construct strs

    lookupIndex str = index tbl ident == str
      where Just ident = lookup tbl str

    indexLookup ident = lookup tbl str == Just ident
      where str       = index tbl ident

enumStrings :: Enum id => StringTable id -> [BS.ByteString]
enumStrings (StringTable bs offsets) = map (index' bs offsets) [0..h-1]
  where (0,h) = A.bounds offsets

enumIds :: Enum id => StringTable id -> [id]
enumIds (StringTable _ offsets) = [toEnum 0 .. toEnum (h-1)]
  where (0,h) = A.bounds offsets

#endif
