{-# LANGUAGE CPP #-}

module Codec.Archive.Tar.Index.StringTable.Tests (
    prop_valid,
    prop_sorted,
    prop_finalise_unfinalise,
    prop_serialise_deserialise,
    prop_serialiseSize,
 ) where

import Prelude hiding (lookup)
import Codec.Archive.Tar.Index.StringTable
import Test.Tasty.QuickCheck

import Data.List hiding (lookup, insert)
import qualified Data.Array.Unboxed as A
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Lazy   as LBS
#if MIN_VERSION_bytestring(0,10,2) || defined(MIN_VERSION_bytestring_builder)
import Data.ByteString.Builder          as BS
import Data.ByteString.Builder.Extra    as BS (byteStringCopy)
#else
import Data.ByteString.Lazy.Builder     as BS
import Data.ByteString.Lazy.Builder.Extras as BS (byteStringCopy)
#endif

prop_valid :: [BS.ByteString] -> Property
prop_valid strs =
       conjoin (map lookupIndex (enumStrings tbl))
  .&&. conjoin (map indexLookup (enumIds tbl))

  where
    tbl :: StringTable Int
    tbl = construct strs

    lookupIndex :: BS.ByteString -> Property
    lookupIndex str = index tbl ident === str
      where Just ident = lookup tbl str

    indexLookup :: Int -> Property
    indexLookup ident = lookup tbl str === Just ident
      where str       = index tbl ident

-- this is important so we can use Map.fromAscList
prop_sorted :: [BS.ByteString] -> Property
prop_sorted strings = property $
    isSorted [ index' strs offsets ix
             | ix <- A.range (A.bounds ids) ]
  where
    _tbl :: StringTable Int
    _tbl@(StringTable strs offsets ids _ixs) = construct strings
    isSorted xs = and (zipWith (<) xs (drop 1 xs))

prop_finalise_unfinalise :: [BS.ByteString] -> Property
prop_finalise_unfinalise strs =
    builder === unfinalise (finalise builder)
  where
    builder :: StringTableBuilder Int
    builder = foldl' (\tbl s -> fst (insert s tbl)) empty strs

prop_serialise_deserialise :: [BS.ByteString] -> Property
prop_serialise_deserialise strs =
    Just (strtable, BS.empty) === (deserialiseV2
                                . LBS.toStrict . BS.toLazyByteString
                                . serialise) strtable
  where
    strtable :: StringTable Int
    strtable = construct strs

prop_serialiseSize :: [BS.ByteString] -> Property
prop_serialiseSize strs =
    (fromIntegral . LBS.length . BS.toLazyByteString . serialise) strtable
 === serialiseSize strtable
  where
    strtable :: StringTable Int
    strtable = construct strs

enumStrings :: Enum id => StringTable id -> [BS.ByteString]
enumStrings (StringTable bs offsets _ _) = map (index' bs offsets) [0..h-1]
  where (0,h) = A.bounds offsets

enumIds :: Enum id => StringTable id -> [id]
enumIds (StringTable _ offsets _ _) = [toEnum 0 .. toEnum (fromIntegral (h-1))]
  where (0,h) = A.bounds offsets
