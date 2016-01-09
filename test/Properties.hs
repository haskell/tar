module Main where

import qualified Codec.Archive.Tar.Index as Index
import qualified Codec.Archive.Tar.Index.IntTrie as IntTrie
import qualified Codec.Archive.Tar.Index.StringTable as StringTable
import qualified Codec.Archive.Tar       as Tar

import qualified Data.ByteString as BS

import Test.Tasty
import Test.Tasty.QuickCheck

main :: IO ()
main =
  defaultMain $
    testGroup "tar tests" [

      testGroup "write/read" [
        testProperty "ustar format" Tar.prop_write_read_ustar,
        testProperty "gnu format"   Tar.prop_write_read_gnu,
        testProperty "v7 format"    Tar.prop_write_read_v7
      ]

    , testGroup "string table" [
        testProperty "construction" StringTable.prop_valid,
        testProperty "sorted"       StringTable.prop_sorted,
        testProperty "serialise"    StringTable.prop_serialise_deserialise,
        testProperty "size"         StringTable.prop_serialiseSize,
        testProperty "unfinalise"   StringTable.prop_finalise_unfinalise
      ]

    , testGroup "int trie" [
        testProperty "unit 1"      IntTrie.test1,
        testProperty "unit 2"      IntTrie.test2,
        testProperty "unit 3"      IntTrie.test3,
        testProperty "lookups"     IntTrie.prop_lookup_mono,
        testProperty "completions" IntTrie.prop_completions_mono,
        testProperty "toList"      IntTrie.prop_construct_toList,
        testProperty "serialise"   IntTrie.prop_serialise_deserialise,
        testProperty "size"        IntTrie.prop_serialiseSize,
        testProperty "unfinalise"  IntTrie.prop_finalise_unfinalise
      ]

    , testGroup "index" [
        testProperty "lookup"      Index.prop_lookup,
        testProperty "valid"       Index.prop_valid,
        testProperty "toList"      Index.prop_toList,
        testProperty "serialise"   Index.prop_serialise_deserialise,
        testProperty "size"        Index.prop_serialiseSize,
        testProperty "matches tar" Index.prop_index_matches_tar,
        testProperty "unfinalise"  Index.prop_finalise_unfinalise
      ]
    ]

