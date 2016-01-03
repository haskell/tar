module Main where

import qualified Codec.Archive.Tar       as Tar
import qualified Codec.Archive.Tar.Index as TarIndex

import qualified Data.ByteString.Lazy    as BS
import Control.Exception

import Criterion
import Criterion.Main

main = defaultMain benchmarks

benchmarks :: [Benchmark]
benchmarks =
  [ env loadTarFile $ \tarfile ->
      bench "read" (nf Tar.read tarfile)

  , env loadTarEntriesList $ \entries ->
      bench "write" (nf Tar.write entries)

  , env loadTarEntries $ \entries ->
      bench "index build" (nf TarIndex.build entries)

  , env loadTarIndex $ \entries ->
      bench "index rebuild" (nf (TarIndex.finalise . TarIndex.unfinalise) entries)
  ]

loadTarFile :: IO BS.ByteString
loadTarFile =
    BS.readFile "01-index.tar"

loadTarEntries :: IO (Tar.Entries Tar.FormatError)
loadTarEntries =
    fmap Tar.read loadTarFile

loadTarEntriesList :: IO [Tar.Entry]
loadTarEntriesList =
    fmap (Tar.foldEntries (:) [] throw) loadTarEntries

loadTarIndex :: IO TarIndex.TarIndex
loadTarIndex =
    fmap (either throw id . TarIndex.build)
         loadTarEntries

