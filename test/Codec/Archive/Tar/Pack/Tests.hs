{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Codec.Archive.Tar.Pack.Tests
  ( prop_roundtrip
  , unit_roundtrip_symlink
  , unit_roundtrip_long_symlink
  , unit_roundtrip_long_filepath
  ) where

import Control.DeepSeq
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.FileEmbed
import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Tar.Pack as Pack
import Codec.Archive.Tar.Types (GenEntries(..), Entries)
import qualified Codec.Archive.Tar.Unpack as Unpack
import Control.Exception
import Data.List.NonEmpty (NonEmpty(..))
import System.Directory
import System.FilePath
import qualified System.Info
import System.IO.Temp
import Test.Tasty.QuickCheck

-- | Write a single file, deeply buried within nested folders;
-- pack and unpack; read back and compare results.
prop_roundtrip :: [ASCIIString] -> ASCIIString -> Property
prop_roundtrip xss (ASCIIString cnt)
  | x : xs <- filter (not . null) $ map mkFilePath xss
  = ioProperty $ withSystemTempDirectory "tar-test" $ \baseDir -> do
    file : dirs <- pure $ trimUpToMaxPathLength baseDir (x : xs)

    let relDir = joinPath dirs
        absDir = baseDir </> relDir
        relFile = relDir </> file
        absFile = absDir </> file
    createDirectoryIfMissing True absDir
    writeFile absFile cnt
    -- Forcing the result, otherwise lazy IO misbehaves.
    !entries <- Pack.pack baseDir [relFile] >>= evaluate . force

    -- Try hard to clean up
    removeFile absFile
    writeFile absFile "<should be overwritten>"
    case dirs of
      [] -> pure ()
      d : _ -> removeDirectoryRecursive (baseDir </> d)

    -- Unpack back
    Unpack.unpack baseDir (foldr Next Done entries :: Entries IOException)
    cnt' <- readFile absFile
    pure $ cnt === cnt'

  | otherwise = discard

mkFilePath :: ASCIIString -> FilePath
mkFilePath (ASCIIString xs) = makeValid $
  filter (\c -> not $ isPathSeparator c || c `elem` [' ', '.', ':']) xs

trimUpToMaxPathLength :: FilePath -> [FilePath] -> [FilePath]
trimUpToMaxPathLength baseDir = go (maxPathLength - length baseDir - 1)
  where
    go :: Int -> [FilePath] -> [FilePath]
    go cnt [] = []
    go cnt (x : xs)
      | cnt <= 0 = []
      | cnt <= length x = [take cnt x]
      | otherwise = x : go (cnt - length x - 1) xs

maxPathLength :: Int
maxPathLength = case System.Info.os of
  "mingw32" -> 255
  _ -> 1023 -- macOS does not like longer names

unit_roundtrip_symlink :: Property
unit_roundtrip_symlink =
  let tar :: BL.ByteString = BL.fromStrict $(embedFile "test/data/symlink.tar")
      entries = Tar.foldEntries (:) [] (const []) (Tar.read tar)
  in Tar.write entries === tar

unit_roundtrip_long_filepath :: Property
unit_roundtrip_long_filepath =
  let tar :: BL.ByteString = BL.fromStrict $(embedFile "test/data/long-filepath.tar")
      entries = Tar.foldEntries (:) [] (const []) (Tar.read tar)
  in Tar.write entries === tar

unit_roundtrip_long_symlink :: Property
unit_roundtrip_long_symlink =
  let tar :: BL.ByteString = BL.fromStrict $(embedFile "test/data/long-symlink.tar")
      entries = Tar.foldEntries (:) [] (const []) (Tar.read tar)
  in Tar.write entries === tar
