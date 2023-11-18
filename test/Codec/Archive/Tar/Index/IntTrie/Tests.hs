{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Codec.Archive.Tar.Index.IntTrie.Tests (
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
 ) where

import Prelude hiding (lookup)
import Codec.Archive.Tar.Index.IntTrie

import qualified Data.Array.Unboxed as A
import Data.Char
import Data.Function (on)
import Data.List hiding (lookup, insert)
import Data.Word (Word32)
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Lazy   as LBS
#if MIN_VERSION_bytestring(0,10,2) || defined(MIN_VERSION_bytestring_builder)
import Data.ByteString.Builder          as BS
#else
import Data.ByteString.Lazy.Builder     as BS
#endif
#if MIN_VERSION_containers(0,5,0)
import qualified Data.IntMap.Strict     as IntMap
import Data.IntMap.Strict (IntMap)
#else
import qualified Data.IntMap            as IntMap
import Data.IntMap (IntMap)
#endif

import Test.QuickCheck
import Control.Applicative ((<$>), (<*>))
import Data.Bits
import Data.Int

-- Example mapping:
--
example0 :: [(FilePath, Int)]
example0 =
  [("foo-1.0/foo-1.0.cabal", 512)   -- tar block 1
  ,("foo-1.0/LICENSE",       2048)  -- tar block 4
  ,("foo-1.0/Data/Foo.hs",   4096)] -- tar block 8

-- After converting path components to integers this becomes:
--
example1 :: [([Key], Value)]
example1 =
  [([Key 1, Key 2], Value 512)
  ,([Key 1, Key 3], Value 2048)
  ,([Key 1, Key 4, Key 5], Value 4096)]

-- As a trie this looks like:

--  [ (1, *) ]
--        |
--        [ (2, 512), (3, 1024), (4, *) ]
--                                   |
--                                   [ (5, 4096) ]

-- We use an intermediate trie representation

mktrie :: [(Int, TrieNode)] -> IntTrieBuilder
mkleaf :: Key -> Value          -> (Int, TrieNode)
mknode :: Key -> IntTrieBuilder -> (Int, TrieNode)

mktrie = IntTrieBuilder . IntMap.fromList
mkleaf k v = (fromIntegral $ unKey k, TrieLeaf (unValue v))
mknode k t = (fromIntegral $ unKey k, TrieNode t)

example2 :: IntTrieBuilder
example2 = mktrie [ mknode (Key 1) t1 ]
  where
    t1   = mktrie [ mkleaf (Key 2) (Value 512), mkleaf (Key 3) (Value 2048), mknode (Key 4) t2 ]
    t2   = mktrie [ mkleaf (Key 5) (Value 4096) ]


example2' :: IntTrieBuilder
example2' = mktrie [ mknode (Key 0) t1 ]
  where
    t1   = mktrie [ mknode (Key 3) t2 ]
    t2   = mktrie [ mknode (Key 1) t3, mknode (Key 2) t4 ]
    t3   = mktrie [ mkleaf (Key 4) (Value 10608) ]
    t4   = mktrie [ mkleaf (Key 4) (Value 10612) ]
{-
0: [1,N0,3]

  3: [1,N3,6]

   6: [2,N1,N2,11,12]

     11: [1,4,10608]
     14: [1,4,10612]
-}

example2'' :: IntTrieBuilder
example2'' = mktrie [ mknode (Key 1) t1, mknode (Key 2) t2 ]
  where
    t1   = mktrie [ mkleaf (Key 4) (Value 10608) ]
    t2   = mktrie [ mkleaf (Key 4) (Value 10612) ]

example2''' :: IntTrieBuilder
example2''' = mktrie [ mknode (Key 0) t3 ]
  where
    t3  = mktrie [ mknode (Key 4) t8, mknode (Key 6) t11 ]
    t8  = mktrie [ mknode (Key 1) t14 ]
    t11 = mktrie [ mkleaf (Key 5) (Value 10605) ]
    t14 = mktrie [ mknode (Key 2) t19, mknode (Key 3) t22 ]
    t19 = mktrie [ mkleaf (Key 7) (Value 10608) ]
    t22 = mktrie [ mkleaf (Key 7) (Value 10612) ]
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
test1 = example2 === inserts example1 empty

-- So the overall array form of the above trie is:
--
-- offset:   0   1    2    3   4  5  6    7    8     9     10  11  12
-- array:  [ 1 | N1 | 3 ][ 3 | 2, 3, N4 | 512, 2048, 10 ][ 1 | 5 | 4096 ]
--                     \__/                           \___/

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

test2 = example3 === flattenTrie example2

example4 :: IntTrie
example4 = IntTrie (mkArray example3)

mkArray :: [Word32] -> A.UArray Word32 Word32
mkArray xs = A.listArray (0, fromIntegral (length xs) - 1) xs

test3 = case lookup example4 [Key 1] of
          Just (Completions [(Key 2,_),(Key 3,_),(Key 4,_)]) -> True
          _                          -> False

test1 :: Property

prop_lookup :: [([Key], Value)] -> Property
prop_lookup paths =
  conjoin $ flip map paths $ \(key, value) ->
    counterexample (show (trie, key)) $
      lookup trie key === Just (Entry value)
  where
    trie = construct paths

prop_completions :: [([Key], Value)] -> Property
prop_completions paths =
    inserts paths empty
 === convertCompletions (completionsFrom (construct paths) 0)
  where
    convertCompletions :: Completions -> IntTrieBuilder
    convertCompletions kls =
      IntTrieBuilder $
        IntMap.fromList
          [ case l of
              Entry v          -> mkleaf k v
              Completions kls' -> mknode k (convertCompletions kls')
          | (k, l) <- sortBy (compare `on` fst) kls ]


prop_lookup_mono :: ValidPaths -> Property
prop_lookup_mono (ValidPaths paths) = prop_lookup paths

prop_completions_mono :: ValidPaths -> Property
prop_completions_mono (ValidPaths paths) = prop_completions paths

prop_construct_toList :: ValidPaths -> Property
prop_construct_toList (ValidPaths paths) =
       sortBy (compare `on` fst) (toList (construct paths))
    === sortBy (compare `on` fst) paths

prop_finalise_unfinalise :: ValidPaths -> Property
prop_finalise_unfinalise (ValidPaths paths) =
    builder === unfinalise (finalise builder)
  where
    builder :: IntTrieBuilder
    builder = inserts paths empty

prop_serialise_deserialise :: ValidPaths -> Property
prop_serialise_deserialise (ValidPaths paths) =
    Just (trie, BS.empty) === (deserialise
                            . LBS.toStrict . BS.toLazyByteString
                            . serialise) trie
  where
    trie :: IntTrie
    trie = construct paths

prop_serialiseSize :: ValidPaths -> Property
prop_serialiseSize (ValidPaths paths) =
    (fromIntegral . LBS.length . BS.toLazyByteString . serialise) trie
 === serialiseSize trie
  where
    trie :: IntTrie
    trie = construct paths

newtype ValidPaths = ValidPaths [([Key], Value)] deriving Show

instance Arbitrary ValidPaths where
  arbitrary =
      ValidPaths . makeNoPrefix <$> listOf ((,)
        -- Key is actually Word31
        <$> listOf1 (fmap (Key . fromIntegral @Int32 . getNonNegative) arbitrary)
        <*> fmap Value arbitrary)
    where
      makeNoPrefix :: [([Key], Value)] -> [([Key], Value)]
      makeNoPrefix [] = []
      makeNoPrefix ((ks, v) : ksvs)
        | all (\(ks', _) -> not (isPrefixOfOther ks ks')) ksvs
                     = (ks, v) : makeNoPrefix ksvs
        | otherwise  =           makeNoPrefix ksvs

  shrink (ValidPaths kvs)
      = map ValidPaths . filter noPrefix . filter nonEmpty . map (map (\(ks, v) -> (map Key ks, Value v)))
      . shrink
      . map (\(ks, v) -> (map unKey ks, unValue v)) $ kvs
    where
      noPrefix :: [([Key], Value)] -> Bool
      noPrefix []           = True
      noPrefix ((k,_):kvs') = all (\(k', _) -> not (isPrefixOfOther k k')) kvs'
                           && noPrefix kvs'
      nonEmpty = all (not . null . fst)

isPrefixOfOther :: [Key] -> [Key] -> Bool
isPrefixOfOther a b = a `isPrefixOf` b || b `isPrefixOf` a
