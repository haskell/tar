{-# LANGUAGE CPP, BangPatterns, DeriveDataTypeable #-}

module Codec.Archive.Tar.Index.StringTable (

    StringTable(..),
    lookup,
    index,
    construct,

    StringTableBuilder,
    empty,
    insert,
    inserts,
    finalise,
    unfinalise,

#ifdef TESTS
    prop_valid,
#endif
 ) where

import Data.Typeable (Typeable)

import Prelude   hiding (lookup, id)
import Data.List hiding (lookup, insert)
import Data.Function (on)
import Data.Word (Word32)
import Data.Int  (Int32)

import qualified Data.Array.Unboxed as A
import           Data.Array.Unboxed ((!))
import qualified Data.Map.Strict        as Map
import           Data.Map.Strict (Map)
import qualified Data.ByteString.Char8  as BS
import qualified Data.ByteString.Unsafe as BS



-- | An effecient mapping from strings to a dense set of integers.
--
data StringTable id = StringTable
         {-# UNPACK #-} !BS.ByteString           -- all strings concatenated
         {-# UNPACK #-} !(A.UArray Int32 Word32) -- string offset table
         {-# UNPACK #-} !(A.UArray Int32 Int32)  -- string index to id table
         {-# UNPACK #-} !(A.UArray Int32 Int32)  -- string id to index table
  deriving (Eq, Show, Typeable)

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
enumStrings (StringTable bs offsets _ _) = map (index' bs offsets) [0..h-1]
  where (0,h) = A.bounds offsets

enumIds :: Enum id => StringTable id -> [id]
enumIds (StringTable _ offsets _ _) = [toEnum 0 .. toEnum (fromIntegral (h-1))]
  where (0,h) = A.bounds offsets

#endif
