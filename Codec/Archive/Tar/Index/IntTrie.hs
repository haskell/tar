{-# LANGUAGE CPP, BangPatterns, PatternGuards #-}
{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables #-}

module Codec.Archive.Tar.Index.IntTrie (

  IntTrie,
  construct,
  toList,

  IntTrieBuilder,
  empty,
  insert,
  finalise,
  unfinalise,

  lookup,
  TrieLookup(..),

  serialise,
  serialiseSize,
  deserialise,

#ifdef TESTS
  test1, test2, test3,
  ValidPaths(..),
  prop_lookup,
  prop_completions,
  prop_lookup_mono,
  prop_completions_mono,
  prop_construct_toList,
  prop_finalise_unfinalise,
  prop_serialise_deserialise,
  prop_serialiseSize,
#endif
 ) where

import Prelude hiding (lookup)

import Data.Typeable (Typeable)

import qualified Data.Array.Unboxed as A
import Data.Array.IArray  ((!))
import qualified Data.Bits as Bits
import Data.Word (Word32)
import Data.Bits
import Data.Monoid (Monoid(..))
#if (MIN_VERSION_base(4,5,0))
import Data.Monoid ((<>))
#endif
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Lazy   as LBS
import qualified Data.ByteString.Unsafe as BS
#if MIN_VERSION_bytestring(0,10,2) || defined(MIN_VERSION_bytestring_builder)
import Data.ByteString.Builder          as BS
#else
import Data.ByteString.Lazy.Builder     as BS
#endif
import Control.Exception (assert)
#if MIN_VERSION_containers(0,5,0)
import qualified Data.Map.Strict        as Map
import qualified Data.IntMap.Strict     as IntMap
import Data.IntMap.Strict (IntMap)
#else
import qualified Data.Map               as Map
import qualified Data.IntMap            as IntMap
import Data.IntMap (IntMap)
#endif

import Data.List hiding (lookup, insert)
import Data.Function (on)

#ifdef TESTS
import Test.QuickCheck
import Control.Applicative ((<$>), (<*>))
#endif


-- | A compact mapping from sequences of nats to nats.
--
-- NOTE: The tries in this module have values /only/ at the leaves (which
-- correspond to files), they do not have values at the branch points (which
-- correspond to directories).
newtype IntTrie k v = IntTrie (A.UArray Word32 Word32)
    deriving (Eq, Show, Typeable)


-- Compact, read-only implementation of a trie. It's intended for use with file
-- paths, but we do that via string ids.

#ifdef TESTS
-- Example mapping:
--
example0 :: [(FilePath, Int)]
example0 =
  [("foo-1.0/foo-1.0.cabal", 512)   -- tar block 1
  ,("foo-1.0/LICENSE",       2048)  -- tar block 4
  ,("foo-1.0/Data/Foo.hs",   4096)] -- tar block 8

-- After converting path components to integers this becomes:
--
example1 :: [([Word32], Word32)]
example1 =
  [([1,2],   512)
  ,([1,3],   2048)
  ,([1,4,5], 4096)]

-- As a trie this looks like:

--  [ (1, *) ]
--        |
--        [ (2, 512), (3, 1024), (4, *) ]
--                                   |
--                                   [ (5, 4096) ]

-- We use an intermediate trie representation

mktrie :: [(Int, TrieNode k v)] -> IntTrieBuilder k v
mkleaf :: (Enum k, Enum v) => k -> v                  -> (Int, TrieNode k v)
mknode ::  Enum k          => k -> IntTrieBuilder k v -> (Int, TrieNode k v)

mktrie = IntTrieBuilder . IntMap.fromList
mkleaf k v = (fromEnum k, TrieLeaf (enumToWord32 v))
mknode k t = (fromEnum k, TrieNode t) 

example2 :: IntTrieBuilder Word32 Word32
example2 = mktrie [ mknode 1 t1 ]
  where
    t1   = mktrie [ mkleaf 2 512, mkleaf 3 2048, mknode 4 t2 ]
    t2   = mktrie [ mkleaf 5 4096 ]


example2' :: IntTrieBuilder Word32 Word32
example2' = mktrie [ mknode 0 t1 ]
  where
    t1   = mktrie [ mknode 3 t2 ]
    t2   = mktrie [ mknode 1 t3, mknode 2 t4 ]
    t3   = mktrie [ mkleaf 4 10608 ]
    t4   = mktrie [ mkleaf 4 10612 ]
{-
0: [1,N0,3]

  3: [1,N3,6]

   6: [2,N1,N2,11,12]

     11: [1,4,10608]
     14: [1,4,10612]
-}

example2'' :: IntTrieBuilder Word32 Word32
example2'' = mktrie [ mknode 1 t1, mknode 2 t2 ]
  where
    t1   = mktrie [ mkleaf 4 10608 ]
    t2   = mktrie [ mkleaf 4 10612 ]

example2''' :: IntTrieBuilder Word32 Word32
example2''' = mktrie [ mknode 0 t3 ]
  where
    t3  = mktrie [ mknode 4 t8, mknode 6 t11 ]
    t8  = mktrie [ mknode 1 t14 ]
    t11 = mktrie [ mkleaf 5 10605 ]
    t14 = mktrie [ mknode 2 t19, mknode 3 t22 ]
    t19 = mktrie [ mkleaf 7 10608 ]
    t22 = mktrie [ mkleaf 7 10612 ]
{-
 0: [1,N0,3]
 3: [2,N4,N6,8,11]
 8: [1,N1,11]
11: [1,5,10605]
14: [2,N2,N3,16,19]
19: [1,7,10608]
22: [1,7,10612]
-}

-- We convert from the 'Paths' to the 'IntTrieBuilder' using 'inserts':
--
test1 = example2 == inserts example1 empty
#endif

-- Each node has a size and a sequence of keys followed by an equal length
-- sequnce of corresponding entries. Since we're going to flatten this into
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

-- So the overall array form of the above trie is:
--
-- offset:   0   1    2    3   4  5  6    7    8     9     10  11  12
-- array:  [ 1 | N1 | 3 ][ 3 | 2, 3, N4 | 512, 2048, 10 ][ 1 | 5 | 4096 ]
--                     \__/                           \___/

#ifdef TESTS
example3 :: [Word32]
example3 =
 [1, tagNode 1,
     3,
  3, tagLeaf 2, tagLeaf 3, tagNode 4,
     512,       2048,      10,
  1, tagLeaf 5,
     4096
 ]

-- We get the array form by using flattenTrie:

test2 = example3 == flattenTrie example2

example4 :: IntTrie Int Int
example4 = IntTrie (mkArray example3)

mkArray :: [Word32] -> A.UArray Word32 Word32
mkArray xs = A.listArray (0, fromIntegral (length xs) - 1) xs

test3 = case lookup example4 [1] of
          Just (Completions [(2,_),(3,_),(4,_)]) -> True
          _                          -> False

test1, test2, test3 :: Bool
#endif

-------------------------------------
-- Decoding the trie array form
--

completionsFrom :: (Enum k, Enum v) => IntTrie k v -> Word32 -> Completions k v
completionsFrom trie@(IntTrie arr) nodeOff =
    [ (word32ToEnum (untag key), next)
    | keyOff <- [keysStart..keysEnd]
    , let key   = arr ! keyOff
          entry = arr ! (keyOff + nodeSize)
          next | isNode key = Completions (completionsFrom trie entry)
               | otherwise  = Entry (word32ToEnum entry)
    ]
  where
    nodeSize  = arr ! nodeOff
    keysStart = nodeOff + 1
    keysEnd   = nodeOff + nodeSize

-- | Convert the trie to a list
--
-- This is the left inverse to 'construct' (modulo ordering).
toList :: forall k v. (Enum k, Enum v) => IntTrie k v -> [([k], v)]
toList = concatMap (aux []) . (`completionsFrom` 0)
  where
    aux :: [k] -> (k, TrieLookup k v) -> [([k], v)]
    aux ks (k, Entry v)        = [(reverse (k:ks), v)]
    aux ks (k, Completions cs) = concatMap (aux (k:ks)) cs

-------------------------------------
-- Toplevel trie array construction
--

-- So constructing the 'IntTrie' as a whole is just a matter of stringing
-- together all the bits

-- | Build an 'IntTrie' from a bunch of (key, value) pairs, where the keys
-- are sequences.
--
construct :: (Enum k, Enum v) => [([k], v)] -> IntTrie k v
construct = finalise . flip inserts empty


---------------------------------
-- Looking up in the trie array
--

data TrieLookup  k v = Entry !v | Completions (Completions k v) deriving Show
type Completions k v = [(k, TrieLookup k v)]

lookup :: forall k v. (Enum k, Enum v) => IntTrie k v -> [k] -> Maybe (TrieLookup k v)
lookup trie@(IntTrie arr) = go 0
  where
    go :: Word32 -> [k] -> Maybe (TrieLookup k v)
    go nodeOff []     = Just (completions nodeOff)
    go nodeOff (k:ks) = case search nodeOff (tagLeaf k') of
      Just entryOff
        | null ks   -> Just (entry entryOff)
        | otherwise -> Nothing
      Nothing       -> case search nodeOff (tagNode k') of
        Nothing       -> Nothing
        Just entryOff -> go (arr ! entryOff) ks
      where
        k' = enumToWord32 k

    entry       entryOff = Entry (word32ToEnum (arr ! entryOff))
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


enumToWord32 :: Enum n => n -> Word32
enumToWord32 = fromIntegral . fromEnum

word32ToEnum :: Enum n => Word32 -> n
word32ToEnum = toEnum . fromIntegral


-------------------------
-- Building Tries
--

newtype IntTrieBuilder k v = IntTrieBuilder (IntMap (TrieNode k v))
  deriving (Show, Eq)

data TrieNode k v = TrieLeaf {-# UNPACK #-} !Word32
                  | TrieNode !(IntTrieBuilder k v)
  deriving (Show, Eq)

empty :: IntTrieBuilder k v
empty = IntTrieBuilder IntMap.empty

insert :: (Enum k, Enum v) => [k] -> v
       -> IntTrieBuilder k v -> IntTrieBuilder k v
insert []    _v t = t
insert (k:ks) v t = insertTrie (fromEnum k) (map fromEnum ks) (enumToWord32 v) t

insertTrie :: Int -> [Int] -> Word32
           -> IntTrieBuilder k v -> IntTrieBuilder k v
insertTrie k ks v (IntTrieBuilder t) =
    IntTrieBuilder $
      IntMap.alter (\t' -> Just $! maybe (freshTrieNode  ks v)
                                         (insertTrieNode ks v) t')
                   k t

insertTrieNode :: [Int] -> Word32 -> TrieNode k v -> TrieNode k v
insertTrieNode []     v  _           = TrieLeaf v
insertTrieNode (k:ks) v (TrieLeaf _) = TrieNode (freshTrie  k ks v)
insertTrieNode (k:ks) v (TrieNode t) = TrieNode (insertTrie k ks v t)

freshTrie :: Int -> [Int] -> Word32 -> IntTrieBuilder k v
freshTrie k []      v =
  IntTrieBuilder (IntMap.singleton k (TrieLeaf v))
freshTrie k (k':ks) v =
  IntTrieBuilder (IntMap.singleton k (TrieNode (freshTrie k' ks v)))

freshTrieNode :: [Int] -> Word32 -> TrieNode k v
freshTrieNode []     v = TrieLeaf v
freshTrieNode (k:ks) v = TrieNode (freshTrie k ks v)

inserts :: (Enum k, Enum v) => [([k], v)]
        -> IntTrieBuilder k v -> IntTrieBuilder k v
inserts kvs t = foldl' (\t' (ks, v) -> insert ks v t') t kvs

finalise :: IntTrieBuilder k v -> IntTrie k v
finalise trie =
    IntTrie $
      A.listArray (0, fromIntegral (flatTrieLength trie) - 1)
                  (flattenTrie trie)

unfinalise :: (Enum k, Enum v) => IntTrie k v -> IntTrieBuilder k v
unfinalise trie =
    go (completionsFrom trie 0)
  where
    go kns =
      IntTrieBuilder $
        IntMap.fromList
          [ (fromEnum k, t)
          | (k, n) <- kns
          , let t = case n of
                      Entry       v    -> TrieLeaf (enumToWord32 v)
                      Completions kns' -> TrieNode (go kns')
          ]

---------------------------------
-- Flattening Tries
--

type Offset = Int

flatTrieLength :: IntTrieBuilder k v -> Int
flatTrieLength (IntTrieBuilder tns) =
    1
  + 2 * IntMap.size tns
  + sum [ flatTrieLength n | TrieNode n <- IntMap.elems tns ]

-- This is a breadth-first traversal. We keep a list of the tries that we are
-- to write out next. Each of these have an offset allocated to them at the
-- time we put them into the list. We keep a running offset so we know where
-- to allocate next.
--
flattenTrie :: IntTrieBuilder k v -> [Word32]
flattenTrie trie = go (queue [trie]) (size trie)
  where
    size (IntTrieBuilder tns) = 1 + 2 * IntMap.size tns

    go :: Q (IntTrieBuilder k v) -> Offset -> [Word32]
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
#if MIN_VERSION_containers(0,4,2)
              IntMap.foldlWithKey' accumNodes
                                   (offset, Map.empty, tries)
                                   tnodes
#else
              foldl' (\a (k,v) -> accumNodes a k v)
                     (offset, Map.empty, tries)
                     (IntMap.toList tnodes)
#endif

    accumNodes :: (Offset, Map.Map Word32 Word32, Q (IntTrieBuilder k v))
               -> Int -> TrieNode k v
               -> (Offset, Map.Map Word32 Word32, Q (IntTrieBuilder k v))
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

serialise :: IntTrie k v -> BS.Builder
serialise (IntTrie arr) =
    let (_, !ixEnd) = A.bounds arr in
    BS.word32BE (ixEnd+1)
 <> foldr (\n r -> BS.word32BE n <> r) mempty (A.elems arr)

serialiseSize :: IntTrie k v -> Int
serialiseSize (IntTrie arr) =
    let (_, ixEnd) = A.bounds arr in
    4
  + 4 * (fromIntegral ixEnd + 1)

deserialise :: BS.ByteString -> Maybe (IntTrie k v, BS.ByteString)
deserialise bs
  | BS.length bs >= 4
  , let lenArr   = readWord32BE bs 0
        lenTotal = 4 + 4 * fromIntegral lenArr
  , BS.length bs >= 4 + 4 * fromIntegral lenArr
  , let !arr = A.array (0, lenArr-1)
                      [ (i, readWord32BE bs off)
                      | (i, off) <- zip [0..lenArr-1] [4,8 .. lenTotal - 4] ]
        !bs' = BS.drop lenTotal bs
  = Just (IntTrie arr, bs')

  | otherwise
  = Nothing

readWord32BE :: BS.ByteString -> Int -> Word32
readWord32BE bs i =
    assert (i >= 0 && i+3 <= BS.length bs - 1) $
    fromIntegral (BS.unsafeIndex bs (i + 0)) `shiftL` 24
  + fromIntegral (BS.unsafeIndex bs (i + 1)) `shiftL` 16
  + fromIntegral (BS.unsafeIndex bs (i + 2)) `shiftL` 8
  + fromIntegral (BS.unsafeIndex bs (i + 3))


-------------------------
-- Correctness property
--

#ifdef TESTS

prop_lookup :: (Ord k, Enum k, Eq v, Enum v, Show k, Show v)
            => [([k], v)] -> Bool
prop_lookup paths =
  flip all paths $ \(key, value) ->
    case lookup trie key of
      Just (Entry value') | value' == value -> True
      Just (Entry value')   -> error $ "IntTrie: " ++ show (key, value, value')
      Nothing               -> error $ "IntTrie: didn't find " ++ show key
      Just (Completions xs) -> error $ "IntTrie: " ++ show xs

  where
    trie = construct paths

prop_completions :: forall k v. (Ord k, Enum k, Eq v, Enum v) => [([k], v)] -> Bool
prop_completions paths =
    inserts paths empty 
 == convertCompletions (completionsFrom (construct paths) 0)
  where
    convertCompletions :: Ord k => Completions k v -> IntTrieBuilder k v
    convertCompletions kls =
      IntTrieBuilder $
        IntMap.fromList
          [ case l of
              Entry v          -> mkleaf k v
              Completions kls' -> mknode k (convertCompletions kls')
          | (k, l) <- sortBy (compare `on` fst) kls ]


prop_lookup_mono :: ValidPaths -> Bool
prop_lookup_mono (ValidPaths paths) = prop_lookup paths

prop_completions_mono :: ValidPaths -> Bool
prop_completions_mono (ValidPaths paths) = prop_completions paths

prop_construct_toList :: ValidPaths -> Bool
prop_construct_toList (ValidPaths paths) =
       sortBy (compare `on` fst) (toList (construct paths))
    == sortBy (compare `on` fst) paths

prop_finalise_unfinalise :: ValidPaths -> Bool
prop_finalise_unfinalise (ValidPaths paths) =
    builder == unfinalise (finalise builder)
  where
    builder :: IntTrieBuilder Char Char
    builder = inserts paths empty

prop_serialise_deserialise :: ValidPaths -> Bool
prop_serialise_deserialise (ValidPaths paths) =
    Just (trie, BS.empty) == (deserialise
                            . toStrict . BS.toLazyByteString
                            . serialise) trie
  where
    trie :: IntTrie Char Char
    trie = construct paths

prop_serialiseSize :: ValidPaths -> Bool
prop_serialiseSize (ValidPaths paths) =
    (fromIntegral . LBS.length . BS.toLazyByteString . serialise) trie
 == serialiseSize trie
  where
    trie :: IntTrie Char Char
    trie = construct paths

newtype ValidPaths = ValidPaths [([Char], Char)] deriving Show

instance Arbitrary ValidPaths where
  arbitrary =
      ValidPaths . makeNoPrefix <$> listOf ((,) <$> listOf1 arbitrary <*> arbitrary)
    where
      makeNoPrefix [] = []
      makeNoPrefix ((k,v):kvs)
        | all (\(k', _) -> not (isPrefixOfOther k k')) kvs
                     = (k,v) : makeNoPrefix kvs
        | otherwise  =         makeNoPrefix kvs

  shrink (ValidPaths kvs) =
      map ValidPaths . filter noPrefix . filter nonEmpty . shrink $ kvs
    where
      noPrefix []           = True
      noPrefix ((k,_):kvs') = all (\(k', _) -> not (isPrefixOfOther k k')) kvs'
                           && noPrefix kvs'
      nonEmpty = all (not . null . fst)

isPrefixOfOther a b = a `isPrefixOf` b || b `isPrefixOf` a

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

