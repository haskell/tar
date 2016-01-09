{-# LANGUAGE CPP, BangPatterns, PatternGuards, DeriveDataTypeable #-}

module Codec.Archive.Tar.Index.StringTable (

    StringTable,
    lookup,
    index,
    construct,

    StringTableBuilder,
    empty,
    insert,
    inserts,
    finalise,
    unfinalise,

    serialise,
    serialiseSize,
    deserialiseV1,
    deserialiseV2,

#ifdef TESTS
    prop_valid,
    prop_sorted,
    prop_finalise_unfinalise,
    prop_serialise_deserialise,
    prop_serialiseSize,
#endif
 ) where

import Data.Typeable (Typeable)

import Prelude   hiding (lookup, id)
import Data.List hiding (lookup, insert)
import Data.Function (on)
import Data.Word (Word32)
import Data.Int  (Int32)
import Data.Bits
import Data.Monoid (Monoid(..))
#if (MIN_VERSION_base(4,5,0))
import Data.Monoid ((<>))
#endif
import Control.Exception (assert)

import qualified Data.Array.Unboxed as A
import           Data.Array.Unboxed ((!))
#if MIN_VERSION_containers(0,5,0)
import qualified Data.Map.Strict        as Map
import           Data.Map.Strict (Map)
#else
import qualified Data.Map               as Map
import           Data.Map (Map)
#endif
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Unsafe as BS
import qualified Data.ByteString.Lazy   as LBS
#if MIN_VERSION_bytestring(0,10,2) || defined(MIN_VERSION_bytestring_builder)
import Data.ByteString.Builder          as BS
import Data.ByteString.Builder.Extra    as BS (byteStringCopy)
#else
import Data.ByteString.Lazy.Builder     as BS
import Data.ByteString.Lazy.Builder.Extras as BS (byteStringCopy)
#endif


-- | An effecient mapping from strings to a dense set of integers.
--
data StringTable id = StringTable
         {-# UNPACK #-} !BS.ByteString           -- all strings concatenated
         {-# UNPACK #-} !(A.UArray Int32 Word32) -- string offset table
         {-# UNPACK #-} !(A.UArray Int32 Int32)  -- string index to id table
         {-# UNPACK #-} !(A.UArray Int32 Int32)  -- string id to index table
  deriving (Show, Typeable)

instance (Eq id, Enum id) => Eq (StringTable id) where
  tbl1 == tbl2 = unfinalise tbl1 == unfinalise tbl2

-- | Look up a string in the token table. If the string is present, return
-- its corresponding index.
--
lookup :: Enum id => StringTable id -> BS.ByteString -> Maybe id
lookup (StringTable bs offsets ids _ixs) str =
    binarySearch 0 (topBound-1) str
  where
    (0, topBound) = A.bounds offsets

    binarySearch !a !b !key
      | a > b     = Nothing
      | otherwise = case compare key (index' bs offsets mid) of
          LT -> binarySearch a (mid-1) key
          EQ -> Just $! toEnum (fromIntegral (ids ! mid))
          GT -> binarySearch (mid+1) b key
      where mid = (a + b) `div` 2

index' :: BS.ByteString -> A.UArray Int32 Word32 -> Int32 -> BS.ByteString
index' bs offsets i = BS.unsafeTake len . BS.unsafeDrop start $ bs
  where
    start, end, len :: Int
    start = fromIntegral (offsets ! i)
    end   = fromIntegral (offsets ! (i+1))
    len   = end - start


-- | Given the index of a string in the table, return the string.
--
index :: Enum id => StringTable id -> id -> BS.ByteString
index (StringTable bs offsets _ids ixs) =
    index' bs offsets . (ixs !) . fromIntegral . fromEnum


-- | Given a list of strings, construct a 'StringTable' mapping those strings
-- to a dense set of integers. Also return the ids for all the strings used
-- in the construction.
--
construct :: Enum id => [BS.ByteString] -> StringTable id
construct = finalise . foldl' (\tbl s -> fst (insert s tbl)) empty


data StringTableBuilder id = StringTableBuilder
                                              !(Map BS.ByteString id)
                               {-# UNPACK #-} !Word32
  deriving (Eq, Show, Typeable)

empty :: StringTableBuilder id
empty = StringTableBuilder Map.empty 0

insert :: Enum id => BS.ByteString -> StringTableBuilder id -> (StringTableBuilder id, id)
insert str builder@(StringTableBuilder smap nextid) =
    case Map.lookup str smap of
      Just id -> (builder, id)
      Nothing -> let !id   = toEnum (fromIntegral nextid)
                     !smap' = Map.insert str id smap
                   in (StringTableBuilder smap' (nextid+1), id)

inserts :: Enum id => [BS.ByteString] -> StringTableBuilder id -> (StringTableBuilder id, [id])
inserts bss builder = mapAccumL (flip insert) builder bss

finalise :: Enum id => StringTableBuilder id -> StringTable id
finalise (StringTableBuilder smap _) =
    (StringTable strs offsets ids ixs)
  where
    strs    = BS.concat (Map.keys smap)
    offsets = A.listArray (0, fromIntegral (Map.size smap))
            . scanl (\off str -> off + fromIntegral (BS.length str)) 0
            $ Map.keys smap
    ids     = A.listArray (0, fromIntegral (Map.size smap) - 1)
            . map (fromIntegral . fromEnum)
            $ Map.elems smap
    ixs     = A.array (A.bounds ids) [ (id,ix) | (ix,id) <- A.assocs ids ]

unfinalise :: Enum id => StringTable id -> StringTableBuilder id
unfinalise (StringTable strs offsets ids _) =
    StringTableBuilder smap nextid
  where
    smap   = Map.fromAscList
               [ (index' strs offsets ix, toEnum (fromIntegral (ids ! ix)))
               | ix <- [0..h] ]
    (0,h)  = A.bounds ids
    nextid = fromIntegral (h+1)


-------------------------
-- (de)serialisation
--

serialise :: StringTable id -> BS.Builder
serialise (StringTable strs offs ids ixs) =
      let (_, !ixEnd) = A.bounds offs in

      BS.word32BE (fromIntegral (BS.length strs))
   <> BS.word32BE (fromIntegral ixEnd + 1)
   <> BS.byteStringCopy strs
   <> foldr (\n r -> BS.word32BE n <> r) mempty (A.elems offs)
   <> foldr (\n r -> BS.int32BE  n <> r) mempty (A.elems ids)
   <> foldr (\n r -> BS.int32BE  n <> r) mempty (A.elems ixs)

serialiseSize :: StringTable id -> Int
serialiseSize (StringTable strs offs _ids _ixs) =
    let (_, !ixEnd) = A.bounds offs
     in 4 * 2
      + BS.length strs
      + 4 * (fromIntegral ixEnd + 1)
      + 8 *  fromIntegral ixEnd

deserialiseV1 :: BS.ByteString -> Maybe (StringTable id, BS.ByteString)
deserialiseV1 bs
  | BS.length bs >= 8
  , let lenStrs = fromIntegral (readWord32BE bs 0)
        lenArr  = fromIntegral (readWord32BE bs 4)
        lenTotal= 8 + lenStrs + 4 * lenArr
  , BS.length bs >= lenTotal
  , let strs = BS.take lenStrs (BS.drop 8 bs)
        arr  = A.array (0, fromIntegral lenArr - 1)
                       [ (i, readWord32BE bs off)
                       | (i, off) <- zip [0 .. fromIntegral lenArr - 1]
                                         [offArrS,offArrS+4 .. offArrE]
                       ]
        ids  = A.array (0, fromIntegral lenArr - 1)
                       [ (i,i) | i <- [0 .. fromIntegral lenArr - 1] ]
        ixs  = ids -- two identity mappings
        offArrS = 8 + lenStrs
        offArrE = offArrS + 4 * lenArr - 1
        !stringTable = StringTable strs arr ids ixs
        !bs'         = BS.drop lenTotal bs
  = Just (stringTable, bs')

  | otherwise
  = Nothing

deserialiseV2 :: BS.ByteString -> Maybe (StringTable id, BS.ByteString)
deserialiseV2 bs
  | BS.length bs >= 8
  , let lenStrs = fromIntegral (readWord32BE bs 0)
        lenArr  = fromIntegral (readWord32BE bs 4)
        lenTotal= 8                   -- the two length prefixes
                + lenStrs
                + 4 * lenArr
                +(4 * (lenArr - 1)) * 2 -- offsets array is 1 longer
  , BS.length bs >= lenTotal
  , let strs = BS.take lenStrs (BS.drop 8 bs)
        offs = A.listArray (0, fromIntegral lenArr - 1)
                           [ readWord32BE bs off
                           | off <- offsets offsOff ]
        -- the second two arrays are 1 shorter
        ids  = A.listArray (0, fromIntegral lenArr - 2)
                           [ readInt32BE bs off
                           | off <- offsets idsOff ]
        ixs  = A.listArray (0, fromIntegral lenArr - 2)
                           [ readInt32BE bs off
                           | off <- offsets ixsOff ]
        offsOff = 8 + lenStrs
        idsOff  = offsOff + 4 * lenArr
        ixsOff  = idsOff  + 4 * (lenArr-1)
        offsets from = [from,from+4 .. from + 4 * (lenArr - 1)]
        !stringTable = StringTable strs offs ids ixs
        !bs'         = BS.drop lenTotal bs
  = Just (stringTable, bs')

  | otherwise
  = Nothing

readInt32BE :: BS.ByteString -> Int -> Int32
readInt32BE bs i = fromIntegral (readWord32BE bs i)

readWord32BE :: BS.ByteString -> Int -> Word32
readWord32BE bs i =
    assert (i >= 0 && i+3 <= BS.length bs - 1) $
    fromIntegral (BS.unsafeIndex bs (i + 0)) `shiftL` 24
  + fromIntegral (BS.unsafeIndex bs (i + 1)) `shiftL` 16
  + fromIntegral (BS.unsafeIndex bs (i + 2)) `shiftL` 8
  + fromIntegral (BS.unsafeIndex bs (i + 3))

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

-- this is important so we can use Map.fromAscList
prop_sorted :: [BS.ByteString] -> Bool
prop_sorted strings =
    isSorted [ index' strs offsets ix
             | ix <- A.range (A.bounds ids) ]
  where
    _tbl :: StringTable Int
    _tbl@(StringTable strs offsets ids _ixs) = construct strings
    isSorted xs = and (zipWith (<) xs (tail xs))

prop_finalise_unfinalise :: [BS.ByteString] -> Bool
prop_finalise_unfinalise strs =
    builder == unfinalise (finalise builder)
  where
    builder :: StringTableBuilder Int
    builder = foldl' (\tbl s -> fst (insert s tbl)) empty strs

prop_serialise_deserialise :: [BS.ByteString] -> Bool
prop_serialise_deserialise strs =
    Just (strtable, BS.empty) == (deserialiseV2
                                . toStrict . BS.toLazyByteString
                                . serialise) strtable
  where
    strtable :: StringTable Int
    strtable = construct strs

prop_serialiseSize :: [BS.ByteString] -> Bool
prop_serialiseSize strs =
    (fromIntegral . LBS.length . BS.toLazyByteString . serialise) strtable
 == serialiseSize strtable
  where
    strtable :: StringTable Int
    strtable = construct strs

enumStrings :: Enum id => StringTable id -> [BS.ByteString]
enumStrings (StringTable bs offsets _ _) = map (index' bs offsets) [0..h-1]
  where (0,h) = A.bounds offsets

enumIds :: Enum id => StringTable id -> [id]
enumIds (StringTable _ offsets _ _) = [toEnum 0 .. toEnum (fromIntegral (h-1))]
  where (0,h) = A.bounds offsets

toStrict :: LBS.ByteString -> BS.ByteString
#if MIN_VERSION_bytestring(0,10,0)
toStrict = LBS.toStrict
#else
toStrict = BS.concat . LBS.toChunks
#endif

#endif

#if !(MIN_VERSION_base(4,5,0))
(<>) :: Monoid m => m -> m -> m
(<>) = mappend
#endif
