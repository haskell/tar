module Main where

import qualified Codec.Archive.Tar       as Tar
import qualified Codec.Archive.Tar.Index as TarIndex

import qualified Data.ByteString.Lazy    as BS
import Data.Maybe
import Control.Exception
import System.Directory
import System.Environment
import System.IO.Temp

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

  , env loadTarEntries $ \entries ->
      bench "unpack" (nfIO $ withSystemTempDirectory "tar-bench" $ \baseDir -> Tar.unpack baseDir entries)

  , env (fmap TarIndex.serialise  loadTarIndex) $ \tarfile ->
      bench "deserialise index" (nf TarIndex.deserialise tarfile)
  ]

loadTarFile :: IO BS.ByteString
loadTarFile = do
    mTarFile <- lookupEnv "TAR_TEST_FILE"
    let tarFile = fromMaybe "01-index.tar" mTarFile
    exists <- doesFileExist tarFile
    if exists
      then BS.readFile tarFile
      else case mTarFile of
             Just _ -> error $ tarFile <> " does not exist"
             Nothing -> error "01-index.tar does not exist, copy it from ~/.cabal/packages/hackage.haskell.org/01-index.tar"

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

