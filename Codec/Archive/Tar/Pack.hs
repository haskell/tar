{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_HADDOCK hide #-}
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

    getDirectoryContentsRecursive,
  ) where

import Codec.Archive.Tar.LongNames
import Codec.Archive.Tar.Types
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Foldable
import System.Directory.OsPath
         ( listDirectory, doesDirectoryExist, getModificationTime
         , pathIsSymbolicLink, getSymbolicLinkTarget
         , Permissions(..), getPermissions, getFileSize )
import Data.Time.Clock.POSIX
         ( utcTimeToPOSIXSeconds )
import System.IO
         ( IOMode(ReadMode), hFileSize )
import System.IO.Unsafe (unsafeInterleaveIO)
import Control.Exception (throwIO, SomeException)

import System.OsPath         (OsPath)
import System.OsPath.Posix   (PosixPath)

import qualified System.File.OsPath as OSP
import qualified System.OsPath as OSP
import qualified System.OsString as OS


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
-- Do not change their contents before the output of 'pack' was consumed in full.
--
pack
  :: OsPath   -- ^ Base directory
  -> [OsPath] -- ^ Files and directories to pack, relative to the base dir
  -> IO [Entry]
pack = packAndCheck (const Nothing)

-- | Like 'pack', but allows to specify additional sanity/security
-- checks on the input filenames. This is useful if you know which
-- check will be used on client side
-- in 'Codec.Archive.Tar.unpack' / 'Codec.Archive.Tar.unpackAndCheck'.
--
-- @since 0.6.0.0
packAndCheck
  :: (GenEntry PosixPath PosixPath -> Maybe SomeException)
  -> OsPath   -- ^ Base directory
  -> [OsPath] -- ^ Files and directories to pack, relative to the base dir
  -> IO [Entry]
packAndCheck secCB baseDir relpaths = do
  paths <- preparePaths baseDir relpaths
  entries <- packPaths baseDir paths
  traverse_ (maybe (pure ()) throwIO . secCB) entries
  pure $ concatMap encodeLongNames entries

preparePaths :: OsPath -> [OsPath] -> IO [OsPath]
preparePaths baseDir = fmap concat . interleave . map go
  where
    go :: OsPath -> IO [OsPath]
    go relpath = do
      let abspath = baseDir OSP.</> relpath
      isDir  <- doesDirectoryExist abspath
      isSymlink <- pathIsSymbolicLink abspath
      if isDir && not isSymlink then do
        entries <- getDirectoryContentsRecursive abspath
        let entries' = map (relpath OSP.</>) entries
        return $ if OS.null relpath
          then entries'
          else OSP.addTrailingPathSeparator relpath : entries'
      else return [relpath]

-- | Pack paths while accounting for overlong filepaths.
packPaths
  :: OsPath
  -> [OsPath]
  -> IO [GenEntry PosixPath PosixPath]
packPaths baseDir paths = interleave $ flip map paths $ \relpath' -> do
  let isDir = OSP.hasTrailingPathSeparator abspath
      abspath = baseDir OSP.</> relpath'
  isSymlink <- pathIsSymbolicLink abspath
  let mkEntry
        | isSymlink = packSymlinkEntry
        | isDir = packDirectoryEntry
        | otherwise = packFileEntry
  let relpath = toFSPosixPath' relpath'
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
  :: OsPath -- ^ Full path to find the file on the local disk
  -> tarPath  -- ^ Path to use for the tar 'Entry' in the archive
  -> IO (GenEntry tarPath linkTarget)
packFileEntry filepath tarpath = do
  mtime   <- getModTime filepath
  perms   <- getPermissions filepath
  -- Get file size without opening it.
  approxSize <- getFileSize filepath

  (content, size) <- if approxSize < 131072
    -- If file is short enough, just read it strictly
    -- so that no file handle dangles around indefinitely.
    then do
      cnt <- OSP.readFile' filepath
      pure (BL.fromStrict cnt, fromIntegral $ B.length cnt)
    else do
      hndl <- OSP.openBinaryFile filepath ReadMode
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
  :: OsPath   -- ^ Full path to find the file on the local disk
  -> tarPath  -- ^ Path to use for the tar 'Entry' in the archive
  -> IO (GenEntry tarPath linkTarget)
packDirectoryEntry filepath tarpath = do
  mtime   <- getModTime filepath
  return (directoryEntry tarpath) {
    entryTime = mtime
  }

-- | Construct a tar entry based on a local symlink.
--
-- This automatically checks symlink safety via 'checkEntrySecurity'.
--
-- @since 0.6.0.0
packSymlinkEntry
  :: OsPath   -- ^ Full path to find the file on the local disk
  -> tarPath  -- ^ Path to use for the tar 'Entry' in the archive
  -> IO (GenEntry tarPath PosixPath)
packSymlinkEntry filepath tarpath = do
  linkTarget' <- getSymbolicLinkTarget filepath
  let linkTarget = toFSPosixPath' linkTarget'
  pure $ symlinkEntry tarpath linkTarget

-- | This is a utility function, much like 'listDirectory'. The
-- difference is that it includes the contents of subdirectories.
--
-- The paths returned are all relative to the top directory. Directory paths
-- are distinguishable by having a trailing path separator
-- (see 'FilePath.Native.hasTrailingPathSeparator').
--
-- All directories are listed before the files that they contain. Amongst the
-- contents of a directory, subdirectories are listed after normal files. The
-- overall result is that files within a directory will be together in a single
-- contiguous group. This tends to improve file layout and IO performance when
-- creating or extracting tar archives.
--
-- * This function returns results lazily. Subdirectories are not scanned
-- until the files entries in the parent directory have been consumed.
-- If the source directory structure changes before the result is used in full,
-- the behaviour is undefined.
--
getDirectoryContentsRecursive :: OsPath -> IO [OsPath]
getDirectoryContentsRecursive dir0 =
  fmap tail (recurseDirectories dir0 [[OS.osstr||]])

recurseDirectories :: OsPath -> [OsPath] -> IO [OsPath]
recurseDirectories _    []         = return []
recurseDirectories base (dir:dirs) = unsafeInterleaveIO $ do
  (files, dirs') <- collect [] [] =<< listDirectory (base OSP.</> dir)

  files' <- recurseDirectories base (dirs' ++ dirs)
  return (dir : files ++ files')

  where
    collect files dirs' []              = return (reverse files, reverse dirs')
    collect files dirs' (entry:entries) = do
      let dirEntry  = dir OSP.</> entry
          dirEntry' = OSP.addTrailingPathSeparator dirEntry
      isDirectory <- doesDirectoryExist (base OSP.</> dirEntry)
      isSymlink <- pathIsSymbolicLink (base OSP.</> dirEntry)
      if isDirectory && not isSymlink
        then collect files (dirEntry':dirs') entries
        else collect (dirEntry:files) dirs' entries

getModTime :: OsPath -> IO EpochTime
getModTime path = do
  -- The directory package switched to the new time package
  t <- getModificationTime path
  return . floor . utcTimeToPOSIXSeconds $ t
