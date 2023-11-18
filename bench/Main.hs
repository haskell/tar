module Main where

import qualified Codec.Archive.Tar       as Tar
import qualified Codec.Archive.Tar.Index as TarIndex

import qualified Data.ByteString.Lazy    as BS
import Control.Exception
import System.Directory

import Test.Tasty.Bench

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
loadTarFile = do
    let tarFile = "01-index.tar"
    exists <- doesFileExist tarFile
    if exists
      then BS.readFile tarFile
      else error "01-index.tar does not exist, copy it from ~/.cabal/packages/hackage.haskell.org/01-index.tar"

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

