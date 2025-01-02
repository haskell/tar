{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_HADDOCK hide #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid restricted function" #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Codec.Archive.Tar
-- Copyright   :  (c) 2007 Bjorn Bringert,
--                    2008 Andrea Vezzosi,
--                    2008-2009, 2012, 2016 Duncan Coutts
-- License     :  BSD3
--
-- Maintainer  :  duncan@community.haskell.org
-- Portability :  portable
--
-----------------------------------------------------------------------------
module Codec.Archive.Tar.Pack (
    pack,
    packAndCheck,
    packFileEntry,
    packDirectoryEntry,
    packSymlinkEntry,
    longLinkEntry,
  ) where

import Codec.Archive.Tar.LongNames
import Codec.Archive.Tar.PackAscii (filePathToOsPath, osPathToFilePath)
import Codec.Archive.Tar.Types

import Data.Bifunctor (bimap)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Foldable
import System.File.OsPath
import System.OsPath
         ( OsPath, (</>) )
import qualified System.OsPath as FilePath.Native
         ( addTrailingPathSeparator, hasTrailingPathSeparator )
import System.Directory.OsPath
         ( doesDirectoryExist, getModificationTime
         , pathIsSymbolicLink, getSymbolicLinkTarget
         , Permissions(..), getPermissions, getFileSize )
import qualified System.Directory.OsPath.Types as FT
import System.Directory.OsPath.Streaming (getDirectoryContentsRecursive)
import Data.Time.Clock.POSIX
         ( utcTimeToPOSIXSeconds )
import System.IO
         ( IOMode(ReadMode), hFileSize )
import System.IO.Unsafe (unsafeInterleaveIO)
import Control.Exception (throwIO, SomeException)

-- | Creates a tar archive from a list of directory or files. Any directories
-- specified will have their contents included recursively. Paths in the
-- archive will be relative to the given base directory.
--
-- This is a portable implementation of packing suitable for portable archives.
-- In particular it only constructs 'NormalFile', 'Directory' and 'SymbolicLink'
-- entries. Hard links are treated like ordinary files. Special files like
-- FIFOs (named pipes), sockets or device files will cause problems.
--
-- * This function returns results lazily. Subdirectories are scanned
-- and files are read one by one as the list of entries is consumed.
-- Do not change their contents before the output of 'Codec.Archive.Tar.pack' was consumed in full.
--
pack
  :: FilePath   -- ^ Base directory
  -> [FilePath] -- ^ Files and directories to pack, relative to the base dir
  -> IO [Entry]
pack = packAndCheck (const Nothing)

-- | Like 'Codec.Archive.Tar.pack', but allows to specify additional sanity/security
-- checks on the input filenames. This is useful if you know which
-- check will be used on client side
-- in 'Codec.Archive.Tar.unpack' / 'Codec.Archive.Tar.unpackAndCheck'.
--
-- @since 0.6.0.0
packAndCheck
  :: (GenEntry FilePath FilePath -> Maybe SomeException)
  -> FilePath   -- ^ Base directory
  -> [FilePath] -- ^ Files and directories to pack, relative to the base dir
  -> IO [Entry]
packAndCheck secCB (filePathToOsPath -> baseDir) (map filePathToOsPath -> relpaths) = do
  paths <- preparePaths baseDir relpaths
  entries' <- packPaths baseDir paths
  let entries = map (bimap osPathToFilePath osPathToFilePath) entries'
  traverse_ (maybe (pure ()) throwIO . secCB) entries
  pure $ concatMap encodeLongNames entries

preparePaths :: OsPath -> [OsPath] -> IO [OsPath]
preparePaths baseDir = fmap concat . interleave . map go
  where
    go :: OsPath -> IO [OsPath]
    go relpath = do
      let abspath = baseDir </> relpath
      isDir  <- doesDirectoryExist abspath
      isSymlink <- pathIsSymbolicLink abspath
      if isDir && not isSymlink then do
        entries <- getDirectoryContentsRecursive abspath
        let entries' = map ((relpath </>) . addSeparatorIfDir) entries
        return $ if relpath == mempty
          then entries'
          else FilePath.Native.addTrailingPathSeparator relpath : entries'
      else return [relpath]

    addSeparatorIfDir (fn, ty) = case ty of
      FT.Directory{} -> FilePath.Native.addTrailingPathSeparator fn
      _ -> fn

-- | Pack paths while accounting for overlong filepaths.
packPaths
  :: OsPath
  -> [OsPath]
  -> IO [GenEntry OsPath OsPath]
packPaths baseDir paths = interleave $ flip map paths $ \relpath -> do
  let isDir = FilePath.Native.hasTrailingPathSeparator abspath
      abspath = baseDir </> relpath
  isSymlink <- pathIsSymbolicLink abspath
  let mkEntry
        | isSymlink = packSymlinkEntry'
        | isDir = packDirectoryEntry'
        | otherwise = packFileEntry'
  mkEntry abspath relpath

interleave :: [IO a] -> IO [a]
interleave = unsafeInterleaveIO . go
  where
    go []     = return []
    go (x:xs) = do
      x'  <- x
      xs' <- interleave xs
      return (x':xs')

-- | Construct a tar entry based on a local file.
--
-- This sets the entry size, the data contained in the file and the file's
-- modification time. If the file is executable then that information is also
-- preserved. File ownership and detailed permissions are not preserved.
--
-- * The file contents is read lazily.
--
packFileEntry
  :: FilePath -- ^ Full path to find the file on the local disk
  -> tarPath  -- ^ Path to use for the tar 'GenEntry' in the archive
  -> IO (GenEntry tarPath linkTarget)
packFileEntry = packFileEntry' . filePathToOsPath

packFileEntry'
  :: OsPath  -- ^ Full path to find the file on the local disk
  -> tarPath -- ^ Path to use for the tar 'GenEntry' in the archive
  -> IO (GenEntry tarPath linkTarget)
packFileEntry' filepath tarpath = do
  mtime   <- getModTime filepath
  perms   <- getPermissions filepath
  -- Get file size without opening it.
  approxSize <- getFileSize filepath

  (content, size) <- if approxSize < 131072
    -- If file is short enough, just read it strictly
    -- so that no file handle dangles around indefinitely.
    then do
      cnt <- readFile' filepath
      pure (BL.fromStrict cnt, fromIntegral $ B.length cnt)
    else do
      hndl <- openBinaryFile filepath ReadMode
      -- File size could have changed between measuring approxSize
      -- and here. Measuring again.
      sz <- hFileSize hndl
      -- Lazy I/O at its best: once cnt is forced in full,
      -- BL.hGetContents will close the handle.
      cnt <- BL.hGetContents hndl
      -- It would be wrong to return (cnt, BL.length sz):
      -- NormalFile constructor below forces size which in turn
      -- allocates entire cnt in memory at once.
      pure (cnt, fromInteger sz)

  pure (simpleEntry tarpath (NormalFile content size))
    { entryPermissions =
      if executable perms then executableFilePermissions else ordinaryFilePermissions
    , entryTime = mtime
    }

-- | Construct a tar entry based on a local directory (but not its contents).
--
-- The only attribute of the directory that is used is its modification time.
-- Directory ownership and detailed permissions are not preserved.
--
packDirectoryEntry
  :: FilePath -- ^ Full path to find the file on the local disk
  -> tarPath  -- ^ Path to use for the tar 'GenEntry' in the archive
  -> IO (GenEntry tarPath linkTarget)
packDirectoryEntry = packDirectoryEntry' . filePathToOsPath

packDirectoryEntry'
  :: OsPath  -- ^ Full path to find the file on the local disk
  -> tarPath -- ^ Path to use for the tar 'GenEntry' in the archive
  -> IO (GenEntry tarPath linkTarget)
packDirectoryEntry' filepath tarpath = do
  mtime   <- getModTime filepath
  return (directoryEntry tarpath) {
    entryTime = mtime
  }

-- | Construct a tar entry based on a local symlink.
--
-- @since 0.6.0.0
packSymlinkEntry
  :: FilePath -- ^ Full path to find the file on the local disk
  -> tarPath  -- ^ Path to use for the tar 'GenEntry' in the archive
  -> IO (GenEntry tarPath FilePath)
packSymlinkEntry = ((fmap (fmap osPathToFilePath) .) . packSymlinkEntry') . filePathToOsPath

packSymlinkEntry'
  :: OsPath  -- ^ Full path to find the file on the local disk
  -> tarPath -- ^ Path to use for the tar 'GenEntry' in the archive
  -> IO (GenEntry tarPath OsPath)
packSymlinkEntry' filepath tarpath = do
  linkTarget <- getSymbolicLinkTarget filepath
  pure $ symlinkEntry tarpath linkTarget

getModTime :: OsPath -> IO EpochTime
getModTime path = do
  -- The directory package switched to the new time package
  t <- getModificationTime path
  return . floor . utcTimeToPOSIXSeconds $ t
