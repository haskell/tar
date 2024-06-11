{-# LANGUAGE CPP, BangPatterns, PatternGuards #-}
{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_HADDOCK hide #-}

module Codec.Archive.Tar.Index.IntTrie (

  IntTrie(..),
  construct,
  toList,

  IntTrieBuilder(..),
  empty,
  insert,
  finalise,
  unfinalise,

  lookup,
  TrieLookup(..),

  serialise,
  serialiseSize,
  deserialise,

  TrieNode(..),
  Completions,
  inserts,
  completionsFrom,
  flattenTrie,
  tagLeaf,
  tagNode,

  Key(..),
  Value(..),
  ) where

import Prelude hiding (lookup)

import Data.Typeable (Typeable)

import qualified Data.Array.Unboxed as A
import Data.Array.IArray  ((!))
import qualified Data.Bits as Bits
import Data.Word (Word32)
import Data.Bits
import Data.Monoid (Monoid(..))
import Data.Monoid ((<>))
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Lazy   as LBS
import qualified Data.ByteString.Unsafe as BS
import Data.ByteString.Builder          as BS
import Control.Exception (assert)
import qualified Data.Map.Strict        as Map
import qualified Data.IntMap.Strict     as IntMap
import Data.IntMap.Strict (IntMap)

import Data.List hiding (lookup, insert)
import Data.Function (on)
import GHC.IO

import Codec.Archive.Tar.Index.Utils

-- | A compact mapping from sequences of nats to nats.
--
-- NOTE: The tries in this module have values /only/ at the leaves (which
-- correspond to files), they do not have values at the branch points (which
-- correspond to directories).
newtype IntTrie = IntTrie (A.UArray Word32 Word32)
    deriving (Eq, Show, Typeable)

-- | The most significant bit is used for tagging,
-- see 'tagLeaf' / 'tagNode' below, so morally it's Word31 only.
newtype Key = Key { unKey :: Word32 }
  deriving (Eq, Ord, Show)

newtype Value = Value { unValue :: Word32 }
  deriving (Eq, Ord, Show)

-- Compact, read-only implementation of a trie. It's intended for use with file
-- paths, but we do that via string ids.

-- Each node has a size and a sequence of keys followed by an equal length
-- sequence of corresponding entries. Since we're going to flatten this into
-- a single array then we will need to replace the trie structure with pointers
-- represented as array offsets.

-- Each node is a pair of arrays, one of keys and one of Either value pointer.
-- We need to distinguish values from internal pointers. We use a tag bit:
--
tagLeaf, tagNode, untag :: Word32 -> Word32
tagLeaf = id
tagNode = flip Bits.setBit   31
untag   = flip Bits.clearBit 31

isNode :: Word32 -> Bool
isNode = flip Bits.testBit 31

-------------------------------------
-- Decoding the trie array form
--

completionsFrom :: IntTrie -> Word32 -> Completions
completionsFrom trie@(IntTrie arr) nodeOff =
    [ (Key (untag key), next)
    | keyOff <- [keysStart..keysEnd]
    , let key   = arr ! keyOff
          entry = arr ! (keyOff + nodeSize)
          next | isNode key = Completions (completionsFrom trie entry)
               | otherwise  = Entry (Value entry)
    ]
  where
    nodeSize  = arr ! nodeOff
    keysStart = nodeOff + 1
    keysEnd   = nodeOff + nodeSize

-- | Convert the trie to a list
--
-- This is the left inverse to 'construct' (modulo ordering).
toList :: IntTrie -> [([Key], Value)]
toList = concatMap (aux []) . (`completionsFrom` 0)
  where
    aux :: [Key] -> (Key, TrieLookup) -> [([Key], Value)]
    aux ks (k, Entry v)        = [(reverse (k:ks), v)]
    aux ks (k, Completions cs) = concatMap (aux (k:ks)) cs

-------------------------------------
-- Toplevel trie array construction
--

-- So constructing the t'IntTrie' as a whole is just a matter of stringing
-- together all the bits

-- | Build an t'IntTrie' from a bunch of (key, value) pairs, where the keys
-- are sequences.
--
construct :: [([Key], Value)] -> IntTrie
construct = finalise . flip inserts empty


---------------------------------
-- Looking up in the trie array
--

data TrieLookup = Entry !Value | Completions Completions
  deriving (Eq, Ord, Show)

type Completions = [(Key, TrieLookup)]

lookup :: IntTrie -> [Key] -> Maybe TrieLookup
lookup trie@(IntTrie arr) = go 0
  where
    go :: Word32 -> [Key] -> Maybe TrieLookup
    go nodeOff []     = Just (completions nodeOff)
    go nodeOff (k:ks) = case search nodeOff (tagLeaf k') of
      Just entryOff
        | null ks   -> Just (entry entryOff)
        | otherwise -> Nothing
      Nothing       -> case search nodeOff (tagNode k') of
        Nothing       -> Nothing
        Just entryOff -> go (arr ! entryOff) ks
      where
        k' = unKey k

    entry       entryOff = Entry (Value (arr ! entryOff))
    completions nodeOff  = Completions (completionsFrom trie nodeOff)

    search :: Word32 -> Word32 -> Maybe Word32
    search nodeOff key = fmap (+nodeSize) (bsearch keysStart keysEnd key)
      where
        nodeSize  = arr ! nodeOff
        keysStart = nodeOff + 1
        keysEnd   = nodeOff + nodeSize

    bsearch :: Word32 -> Word32 -> Word32 -> Maybe Word32
    bsearch a b key
      | a > b     = Nothing
      | otherwise = case compare key (arr ! mid) of
          LT -> bsearch a (mid-1) key
          EQ -> Just mid
          GT -> bsearch (mid+1) b key
      where mid = (a + b) `div` 2

-------------------------
-- Building Tries
--

newtype IntTrieBuilder = IntTrieBuilder (IntMap TrieNode)
  deriving (Show, Eq)

data TrieNode = TrieLeaf {-# UNPACK #-} !Word32
              | TrieNode !IntTrieBuilder
  deriving (Show, Eq)

empty :: IntTrieBuilder
empty = IntTrieBuilder IntMap.empty

insert :: [Key] -> Value
       -> IntTrieBuilder -> IntTrieBuilder
insert []    _v t = t
insert (k:ks) v t = insertTrie
  (fromIntegral (unKey k) :: Int)
  (map (fromIntegral . unKey) ks :: [Int])
  (unValue v)
  t

insertTrie :: Int -> [Int] -> Word32
           -> IntTrieBuilder -> IntTrieBuilder
insertTrie k ks v (IntTrieBuilder t) =
    IntTrieBuilder $
      IntMap.alter (\t' -> Just $! maybe (freshTrieNode  ks v)
                                         (insertTrieNode ks v) t')
                   k t

insertTrieNode :: [Int] -> Word32 -> TrieNode -> TrieNode
insertTrieNode []     v  _           = TrieLeaf v
insertTrieNode (k:ks) v (TrieLeaf _) = TrieNode (freshTrie  k ks v)
insertTrieNode (k:ks) v (TrieNode t) = TrieNode (insertTrie k ks v t)

freshTrie :: Int -> [Int] -> Word32 -> IntTrieBuilder
freshTrie k []      v =
  IntTrieBuilder (IntMap.singleton k (TrieLeaf v))
freshTrie k (k':ks) v =
  IntTrieBuilder (IntMap.singleton k (TrieNode (freshTrie k' ks v)))

freshTrieNode :: [Int] -> Word32 -> TrieNode
freshTrieNode []     v = TrieLeaf v
freshTrieNode (k:ks) v = TrieNode (freshTrie k ks v)

inserts :: [([Key], Value)]
        -> IntTrieBuilder -> IntTrieBuilder
inserts kvs t = foldl' (\t' (ks, v) -> insert ks v t') t kvs

finalise :: IntTrieBuilder -> IntTrie
finalise trie =
    IntTrie $
      A.listArray (0, fromIntegral (flatTrieLength trie) - 1)
                  (flattenTrie trie)

unfinalise :: IntTrie -> IntTrieBuilder
unfinalise trie =
    go (completionsFrom trie 0)
  where
    go kns =
      IntTrieBuilder $
        IntMap.fromList
          [ (fromIntegral (unKey k) :: Int, t)
          | (k, n) <- kns
          , let t = case n of
                      Entry       v    -> TrieLeaf (unValue v)
                      Completions kns' -> TrieNode (go kns')
          ]

---------------------------------
-- Flattening Tries
--

type Offset = Int

flatTrieLength :: IntTrieBuilder -> Int
flatTrieLength (IntTrieBuilder tns) =
    1
  + 2 * IntMap.size tns
  + sum [ flatTrieLength n | TrieNode n <- IntMap.elems tns ]

-- This is a breadth-first traversal. We keep a list of the tries that we are
-- to write out next. Each of these have an offset allocated to them at the
-- time we put them into the list. We keep a running offset so we know where
-- to allocate next.
--
flattenTrie :: IntTrieBuilder -> [Word32]
flattenTrie trie = go (queue [trie]) (size trie)
  where
    size (IntTrieBuilder tns) = 1 + 2 * IntMap.size tns

    go :: Q IntTrieBuilder -> Offset -> [Word32]
    go todo !offset =
      case dequeue todo of
        Nothing                   -> []
        Just (IntTrieBuilder tnodes, tries) ->
            flat ++ go tries' offset'
          where
            !count = IntMap.size tnodes
            flat   = fromIntegral count
                   : Map.keys  keysValues
                  ++ Map.elems keysValues
            (!offset', !keysValues, !tries') =
              IntMap.foldlWithKey' accumNodes
                                   (offset, Map.empty, tries)
                                   tnodes

    accumNodes :: (Offset, Map.Map Word32 Word32, Q IntTrieBuilder)
               -> Int -> TrieNode
               -> (Offset, Map.Map Word32 Word32, Q IntTrieBuilder)
    accumNodes (!off, !kvs, !tries) !k (TrieLeaf v) =
        (off, kvs', tries)
      where
        kvs' = Map.insert (tagLeaf (int2Word32 k)) v kvs

    accumNodes (!off, !kvs, !tries) !k (TrieNode t) =
        (off + size t, kvs', tries')
      where
        kvs'   = Map.insert (tagNode (int2Word32 k)) (int2Word32 off) kvs
        tries' = enqueue tries t

data Q a = Q [a] [a]

queue :: [a] -> Q a
queue xs = Q xs []

enqueue :: Q a -> a -> Q a
enqueue (Q front  back) x = Q front (x : back)

dequeue :: Q a -> Maybe (a, Q a)
dequeue (Q (x:xs) back)    = Just (x, Q xs back)
dequeue (Q []     back)    = case reverse back of
                               x:xs -> Just (x, Q xs [])
                               []   -> Nothing

int2Word32 :: Int -> Word32
int2Word32 = fromIntegral


-------------------------
-- (de)serialisation
--

serialise :: IntTrie -> BS.Builder
serialise (IntTrie arr) =
    let (_, !ixEnd) = A.bounds arr in
    BS.word32BE (ixEnd+1)
 <> foldr (\n r -> BS.word32BE n <> r) mempty (A.elems arr)

serialiseSize :: IntTrie -> Int
serialiseSize (IntTrie arr) =
    let (_, ixEnd) = A.bounds arr in
    4
  + 4 * (fromIntegral ixEnd + 1)

deserialise :: BS.ByteString -> Maybe (IntTrie, BS.ByteString)
deserialise bs
  | BS.length bs >= 4
  , let lenArr   = readWord32BE bs 0
        lenTotal = 4 + 4 * fromIntegral lenArr
  , BS.length bs >= 4 + 4 * fromIntegral lenArr
  , let !bs_without_len = BS.unsafeDrop 4 bs
        !bs_remaining = BS.unsafeDrop lenTotal bs
        !arr = unsafePerformIO $ beToLe lenArr bs_without_len
  = Just (IntTrie arr, bs_remaining)

  | otherwise
  = Nothing

