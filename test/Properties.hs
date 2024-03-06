{-# LANGUAGE CPP #-}

module Main where

import qualified Codec.Archive.Tar.Index.Tests as Index
import qualified Codec.Archive.Tar.Index.IntTrie.Tests as IntTrie
import qualified Codec.Archive.Tar.Index.StringTable.Tests as StringTable
import qualified Codec.Archive.Tar.Pack.Tests  as Pack
import qualified Codec.Archive.Tar.Tests       as Tar
import qualified Codec.Archive.Tar.Types.Tests as Types
import qualified Codec.Archive.Tar.Unpack.Tests as Unpack

import Test.Tasty
import Test.Tasty.QuickCheck

main :: IO ()
main =
  defaultMain $
    testGroup "tar tests" [

      testGroup "fromTarPath" [
        testProperty "fromTarPath" Types.prop_fromTarPath,
        testProperty "fromTarPathToPosixPath" Types.prop_fromTarPathToPosixPath,
        testProperty "fromTarPathToWindowsPath" Types.prop_fromTarPathToWindowsPath
      ]

    , testGroup "write/read" [
        testProperty "ustar format" Tar.prop_write_read_ustar,
        testProperty "gnu format"   Tar.prop_write_read_gnu,
        testProperty "v7 format"    Tar.prop_write_read_v7,
        testProperty "large filesize" Tar.prop_large_filesize
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
#ifdef MIN_VERSION_bytestring_handle
        testProperty "matches tar" Index.prop_index_matches_tar,
#endif
        testProperty "unfinalise"  Index.prop_finalise_unfinalise
      ]

    , testGroup "pack" [
      adjustOption (\(QuickCheckMaxRatio n) -> QuickCheckMaxRatio (max n 100)) $
      testProperty "roundtrip" Pack.prop_roundtrip,
      testProperty "unicode" Pack.unit_roundtrip_unicode,
      testProperty "symlink" Pack.unit_roundtrip_symlink,
      testProperty "long filepath" Pack.unit_roundtrip_long_filepath,
      testProperty "long symlink" Pack.unit_roundtrip_long_symlink
      ]

    , testGroup "unpack" [
      testProperty "modtime 1970-01-01" Unpack.case_modtime_1970
      ]
    ]
