{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid restricted function" #-}

module Codec.Archive.Tar.Pack.Tests
  ( prop_roundtrip
  , unit_roundtrip_unicode
  , unit_roundtrip_symlink
  , unit_roundtrip_long_symlink
  , unit_roundtrip_long_filepath
  ) where

import Control.DeepSeq
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Char
import Data.FileEmbed
import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Tar.Pack as Pack
import Codec.Archive.Tar.PackAscii (filePathToOsPath)
import qualified Codec.Archive.Tar.Read as Read
import Codec.Archive.Tar.Types (GenEntries(..), Entries, simpleEntry, toTarPath, GenEntry (entryTarPath))
import qualified Codec.Archive.Tar.Unpack as Unpack
import qualified Codec.Archive.Tar.Write as Write
import Control.Exception
import qualified Data.List as L
import Data.List.NonEmpty (NonEmpty(..))
import GHC.IO.Encoding
import System.Directory
import System.Directory.OsPath.Streaming (getDirectoryContentsRecursive)
import System.FilePath
import qualified System.FilePath.Posix as Posix
import qualified System.Info
import System.IO.Temp
import System.IO.Unsafe
import Test.Tasty.QuickCheck

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
prop_roundtrip :: Int -> [String] -> String -> Property
prop_roundtrip n' xss cnt
  | x : xs <- filter (not . null) $ map mkFilePath xss
  = ioProperty $ withSystemTempDirectory "tar-test" $ \baseDir -> do
    file : dirs <- pure $ trimUpToMaxPathLength baseDir (x : xs)

    let relDir = joinPath dirs
        absDir = baseDir </> relDir
        relFile = relDir </> file
        absFile = absDir </> file
        n       = n' `mod` (length dirs + 1)
        (target, expectedFileNames) = case n of
          0 -> (relFile, [relFile])
          _ -> (joinPath $ take (n - 1) dirs,
            map (addTrailingPathSeparator . joinPath)
              (drop (max 1 (n - 1)) $ L.inits dirs) ++ [relFile])
        errMsg = "relDir  = '" ++ relDir ++ "'" ++
               "\nabsDir  = '" ++ absDir ++ "'" ++
               "\nrelFile = '" ++ relFile ++ "'" ++
               "\nabsFile = '" ++ absFile ++ "'" ++
               "\ntarget  = '" ++ target ++ "'"

    -- Not all filesystems allow paths to contain arbitrary Unicode.
    -- E. g., at the moment of writing Apple FS does not support characters
    -- introduced in Unicode 15.0.
    canCreateDirectory <- try (createDirectoryIfMissing True absDir)
    case canCreateDirectory of
      Left (e :: IOException) -> discard
      Right () -> do
        canWriteFile <- try (writeFile absFile cnt)
        case canWriteFile of
          Left (e :: IOException) -> discard
          Right () -> counterexample errMsg <$> do
            -- Forcing the result, otherwise lazy IO misbehaves.
            !entries <- Pack.pack baseDir [target] >>= evaluate . force

            let fileNames
                  = map (map (\c -> if c == Posix.pathSeparator then pathSeparator else c))
                  $ Tar.foldEntries ((:) . entryTarPath) [] undefined
                  -- decodeLongNames produces FilePath with POSIX path separators
                  $ Tar.decodeLongNames $ foldr Next Done entries

            if expectedFileNames /= fileNames then pure (expectedFileNames === fileNames) else do

              -- Try hard to clean up
              removeFile absFile
              writeFile absFile "<should be overwritten>"
              case dirs of
                [] -> pure ()
                d : _ -> removeDirectoryRecursive (baseDir </> d)

              -- Unpack back
              Unpack.unpack baseDir (foldr Next Done entries :: Entries IOException)
              exist <- doesFileExist absFile
              if exist then do
                cnt' <- readFile absFile >>= evaluate . force
                pure $ cnt === cnt'
              else do
                -- Forcing the result, otherwise lazy IO misbehaves.
                recFiles <- getDirectoryContentsRecursive (filePathToOsPath baseDir) >>= evaluate . force
                pure $ counterexample ("File " ++ absFile ++ " does not exist; instead found\n" ++ unlines (map show recFiles)) False

  | otherwise = discard

mkFilePath :: String -> FilePath
mkFilePath xs = makeValid $ filter isGood $
  map (if supportsUnicode then id else chr . (`mod` 128) . ord) xs
  where
    isGood c
      = not (isPathSeparator c)
      && c `notElem` [' ', '\n', '\r', '.', ':']
      && generalCategory c /= Surrogate
      && (supportsUnicode || isAscii c)

trimUpToMaxPathLength :: FilePath -> [FilePath] -> [FilePath]
trimUpToMaxPathLength baseDir = go (maxPathLength - utf8Length baseDir - 1)
  where
    go :: Int -> [FilePath] -> [FilePath]
    go cnt [] = []
    go cnt (x : xs)
      | cnt < 4 = []
      | cnt <= utf8Length x = [take (cnt `quot` 4) x]
      | otherwise = x : go (cnt - utf8Length x - 1) xs

utf8Length :: String -> Int
utf8Length = sum . map charLength
  where
    charLength c
      | c < chr 0x80 = 1
      | c < chr 0x800 = 2
      | c < chr 0x10000 = 3
      | otherwise = 4

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
unit_roundtrip_unicode = do
  ioProperty $ withSystemTempDirectory "tar-test" $ \baseDir -> do
    let relFile = "TModula𐐀bA.hs"

    canWriteFile <- try (writeFile (baseDir </> relFile) "foo")
    case canWriteFile of
      Left (e :: IOException) -> pure $ property True
      Right () -> do
        entries <- Pack.pack baseDir [relFile]
        pure $ case Tar.foldlEntries (flip seq) () (Read.read (Write.write entries)) of
          Right{} -> property True
          Left (err, _) -> counterexample (show err) $ property False
