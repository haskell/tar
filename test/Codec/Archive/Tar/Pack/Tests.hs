{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Codec.Archive.Tar.Pack.Tests
  ( prop_roundtrip
  , unit_roundtrip_symlink
  , unit_roundtrip_long_symlink
  , unit_roundtrip_long_filepath
  , unit_roundtrip_unicode
  ) where

import Data.Maybe
import Control.DeepSeq
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as B8
import Data.Char
import Data.FileEmbed
import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Tar.Pack as Pack
import Codec.Archive.Tar.Types (GenEntries(..), Entries, GenEntry (entryTarPath), toFSPosixPath')
import qualified Codec.Archive.Tar.Unpack as Unpack
import Control.Exception
import GHC.IO.Encoding
import System.Directory.OsPath
import System.FilePath
import qualified System.Info
import System.IO.Temp
import System.IO.Unsafe
import Test.Tasty.QuickCheck

import qualified System.OsString as OS

import System.OsPath (OsPath)
import qualified System.OsPath as OSP
import qualified System.File.OsPath as OSP
import qualified System.OsString.Posix as PS
import qualified System.OsPath.Posix as PFP

supportsUnicode :: Bool
supportsUnicode = unsafePerformIO $ do
  -- Normally getFileSystemEncoding returns a Unicode encoding,
  -- but if it is ASCII, we should not generate Unicode filenames.
  enc <- getFileSystemEncoding
  pure $ case textEncodingName enc of
    "ASCII"          -> False
    "ANSI_X3.4-1968" -> False
    _                -> True
{-# NOINLINE supportsUnicode #-}

-- | Write a single file, deeply buried within nested folders;
-- pack and unpack; read back and compare results.
prop_roundtrip :: [String] -> String -> Property
prop_roundtrip xss cnt
  | x : xs <- filter (not . OS.null) $ map mkFilePath xss
  = ioProperty $ withSystemTempDirectory "tar-test" $ \baseDir' -> do
    baseDir <- OSP.encodeFS baseDir'
    file : dirs <- pure $ trimUpToMaxPathLength baseDir (x : xs)

    let relDir = OSP.joinPath dirs
        absDir = baseDir OSP.</> relDir
        relFile = relDir OSP.</> file
        absFile = absDir OSP.</> file
        errMsg = "relDir  = " ++ fromJust (OSP.decodeUtf relDir) ++
               "\nabsDir  = " ++ fromJust (OSP.decodeUtf absDir) ++
               "\nrelFile = " ++ fromJust (OSP.decodeUtf relFile) ++
               "\nabsFile = " ++ fromJust (OSP.decodeUtf absFile)

    -- Not all filesystems allow paths to contain arbitrary Unicode.
    -- E. g., at the moment of writing Apple FS does not support characters
    -- introduced in Unicode 15.0.
    canCreateDirectory <- try (createDirectoryIfMissing True absDir)
    case canCreateDirectory of
      Left (_ :: IOException) -> discard
      Right () -> do
        canWriteFile <- try (OSP.writeFile absFile $ B8.pack cnt)
        case canWriteFile of
          Left (_ :: IOException) -> discard
          Right () -> counterexample errMsg <$> do

            -- Forcing the result, otherwise lazy IO misbehaves.
            !entries <- Pack.pack baseDir [relFile] >>= evaluate . force

            let fileNames
                  = map (PS.map (\c -> if c == PFP.pathSeparator then PFP.pathSeparator else c))
                  $ Tar.foldEntries ((:) . entryTarPath) [] undefined
                  -- decodeLongNames produces FilePath with POSIX path separators
                  $ Tar.decodeLongNames $ foldr Next Done entries

            let relFile' = toFSPosixPath' relFile
            if [relFile'] /= fileNames then pure ([relFile'] === fileNames) else do

              -- Try hard to clean up
              removeFile absFile
              OSP.writeFile absFile "<should be overwritten>"
              case dirs of
                [] -> pure ()
                d : _ -> removeDirectoryRecursive (baseDir OSP.</> d)

              -- Unpack back
              Unpack.unpack baseDir (foldr Next Done entries :: Entries IOException)
              exist <- doesFileExist absFile
              if exist then do
                cnt' <- OSP.readFile absFile >>= evaluate . force
                pure $ B8.pack cnt === cnt'
              else do
                -- Forcing the result, otherwise lazy IO misbehaves.
                recFiles <- Pack.getDirectoryContentsRecursive baseDir >>= evaluate . force
                pure $ counterexample ("File " ++ fromJust (OSP.decodeUtf absFile)
                                      ++ " does not exist; instead found\n" ++ unlines (fmap (fromJust . OSP.decodeUtf) recFiles)) False

  | otherwise = discard

mkFilePath :: String -> OsPath
mkFilePath xs = fromJust $ OSP.encodeUtf $ makeValid $ filter isGood $
  map (if supportsUnicode then id else chr . (`mod` 128) . ord) xs
  where
    isGood c
      = not (isPathSeparator c)
      && c `notElem` [' ', '\n', '\r', '.', ':']
      && generalCategory c /= Surrogate
      && (supportsUnicode || isAscii c)

trimUpToMaxPathLength :: OsPath -> [OsPath] -> [OsPath]
trimUpToMaxPathLength baseDir = go (maxPathLength - OS.length baseDir - 1)
  where
    go :: Int -> [OsPath] -> [OsPath]
    go _ [] = []
    go cnt (x : xs)
      | cnt <= 0 = []
      | cnt <= OS.length x = [OS.take cnt x]
      | otherwise = x : go (cnt - OS.length x - 1) xs

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

unit_roundtrip_unicode :: Property
unit_roundtrip_unicode =
  let tar :: BL.ByteString = BL.fromStrict $(embedFile "test/data/unicode.tar")
      entries = Tar.foldEntries (:) [] (const []) (Tar.read tar)
  in Tar.write entries === tar

