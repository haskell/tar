{-# LANGUAGE BangPatterns #-}

module Codec.Archive.Tar.Pack.Tests
  ( prop_roundtrip
  ) where

import qualified Codec.Archive.Tar.Pack as Pack
import Codec.Archive.Tar.Types (Entries(..))
import qualified Codec.Archive.Tar.Unpack as Unpack
import Control.Exception
import System.Directory
import System.FilePath
import System.IO.Temp
import Test.Tasty.QuickCheck

-- | Write a single file, deeply buried within nested folders;
-- pack and unpack; read back and compare results.
prop_roundtrip :: [ASCIIString] -> ASCIIString -> Property
prop_roundtrip xss (ASCIIString cnt)
  | file : dirs <- filter (not . null) $ map mkFilePath xss
  -- Filenames longer than 1024 characters throw
  -- "withFile: invalid argument (File name too long)",
  -- at least on Mac OS
  , length (joinPath dirs </> file) < 900
  = ioProperty $ withSystemTempDirectory "tar-test" $ \baseDir -> do
    let relDir = joinPath dirs
        absDir = baseDir </> relDir
        relFile = relDir </> file
        absFile = absDir </> file
    createDirectoryIfMissing True absDir
    writeFile absFile cnt
    -- Forcing the result, otherwise lazy IO misbehaves.
    !entries <- Pack.pack baseDir [relFile]

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
  filter (\c -> not $ isPathSeparator c || c == '.') xs
