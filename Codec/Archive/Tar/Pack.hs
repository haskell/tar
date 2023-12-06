{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
    packWith,
    packFileEntry,
    packDirectoryEntry,
    packSymlinkEntry,
    longLinkEntry,

    getDirectoryContentsRecursive,
  ) where

import Codec.Archive.Tar.LongNames
import Codec.Archive.Tar.Types
import Control.Monad (join, when, forM, (>=>))
import qualified Data.ByteString as BSS
import qualified Data.ByteString.Lazy as BS
import System.FilePath
         ( (</>) )
import qualified System.FilePath as FilePath.Native
         ( addTrailingPathSeparator, hasTrailingPathSeparator, splitDirectories )
import System.Directory
         ( listDirectory, doesDirectoryExist, getModificationTime
         , pathIsSymbolicLink, getSymbolicLinkTarget
         , Permissions(..), getPermissions )
import Data.Time.Clock
         ( UTCTime )
import Data.Time.Clock.POSIX
         ( utcTimeToPOSIXSeconds )
import System.IO
         ( IOMode(ReadMode), withBinaryFile, hFileSize )
import System.IO.Unsafe (unsafeInterleaveIO)
import Control.Exception (throwIO, SomeException)
import Codec.Archive.Tar.Check.Internal (checkSecurity)

-- | Creates a tar archive from a list of directory or files. Any directories
-- specified will have their contents included recursively. Paths in the
-- archive will be relative to the given base directory.
--
-- This is a portable implementation of packing suitable for portable archives.
-- In particular it only constructs 'NormalFile', 'Directory' and 'SymbolicLink'
-- entries. Hard links are treated like ordinary files. Special files like
-- FIFOs (named pipes), sockets or device files will cause problems.
--
-- An exception will be thrown for any file names that are too long to
-- represent as a 'TarPath'.
--
-- * This function returns results lazily. Subdirectories are scanned
-- and files are read one by one as the list of entries is consumed.
--
pack :: FilePath   -- ^ Base directory
     -> [FilePath] -- ^ Files and directories to pack, relative to the base dir
     -> IO [Entry]
pack = packWith checkSecurity

-- | Like 'pack', but does not perform any sanity/security checks on the input.
-- You can do so yourself, e.g.: @packWith@ 'checkSecurity' @dir@ @files@.
--
-- @since 0.6.0.0
packWith :: CheckSecurityCallback
         -> FilePath   -- ^ Base directory
         -> [FilePath] -- ^ Files and directories to pack, relative to the base dir
         -> IO [Entry]
packWith secCB baseDir =
  preparePaths baseDir >=>
  packPaths secCB baseDir >=>
  (pure . concatMap encodeLongNames)

preparePaths :: FilePath -> [FilePath] -> IO [FilePath]
preparePaths baseDir = fmap concat . interleave . map go
  where
    go relpath = do
      let abspath = baseDir </> relpath
      isDir  <- doesDirectoryExist abspath
      isSymlink <- pathIsSymbolicLink abspath
      if isDir && not isSymlink then do
        entries <- getDirectoryContentsRecursive abspath
        let entries' = map (relpath </>) entries
        return $ if null relpath
          then entries'
          else FilePath.Native.addTrailingPathSeparator relpath : entries'
      else return [relpath]

-- | Pack paths while accounting for overlong filepaths.
packPaths :: CheckSecurityCallback -> FilePath -> [FilePath] -> IO [GenEntry FilePath FilePath]
packPaths secCB baseDir paths = interleave $ flip map paths $ \relpath -> do
  let isDir = FilePath.Native.hasTrailingPathSeparator abspath
      abspath = baseDir </> relpath
  isSymlink <- pathIsSymbolicLink abspath
  let mkEntry = if isSymlink then packSymlinkEntry else
        (if isDir then packDirectoryEntry else packFileEntry)
  e <- mkEntry abspath relpath
  secCB e
  pure e

interleave :: [IO a] -> IO [a]
interleave = unsafeInterleaveIO . go
  where
    go []     = return []
    go (x:xs) = do
      x'  <- x
      xs' <- interleave xs
      return (x':xs')

-- | Construct a tar 'Entry' based on a local file.
--
-- This sets the entry size, the data contained in the file and the file's
-- modification time. If the file is executable then that information is also
-- preserved. File ownership and detailed permissions are not preserved.
--
-- * The file contents is read lazily.
--
packFileEntry :: FilePath -- ^ Full path to find the file on the local disk
              -> tarPath -- ^ Path to use for the tar Entry in the archive
              -> IO (GenEntry tarPath linkTarget)
packFileEntry filepath tarpath = do
  mtime   <- getModTime filepath
  perms   <- getPermissions filepath
  content <- BS.readFile filepath
  let size = BS.length content
  return (simpleEntry tarpath (NormalFile content (fromIntegral size))) {
         entryPermissions = if executable perms then executableFilePermissions
                                                else ordinaryFilePermissions,
         entryTime = mtime
         }

-- | Construct a tar 'Entry' based on a local directory (but not its contents).
--
-- The only attribute of the directory that is used is its modification time.
-- Directory ownership and detailed permissions are not preserved.
--
packDirectoryEntry :: FilePath -- ^ Full path to find the file on the local disk
                   -> tarPath  -- ^ Path to use for the tar Entry in the archive
                   -> IO (GenEntry tarPath linkTarget)
packDirectoryEntry filepath tarpath = do
  mtime   <- getModTime filepath
  return (directoryEntry tarpath) {
    entryTime = mtime
  }

-- | Construct a tar 'Entry' based on a local symlink.
--
-- This automatically checks symlink safety via 'checkEntrySecurity'.
--
-- @since 0.6.0.0
packSymlinkEntry :: FilePath -- ^ Full path to find the file on the local disk
                 -> tarPath  -- ^ Path to use for the tar Entry in the archive
                 -> IO (GenEntry tarPath FilePath)
packSymlinkEntry filepath tarpath = do
  linkTarget <- getSymbolicLinkTarget filepath
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
-- If the source directory structure changes before the result is used,
-- the behaviour is undefined.
--
getDirectoryContentsRecursive :: FilePath -> IO [FilePath]
getDirectoryContentsRecursive dir0 =
  fmap tail (recurseDirectories dir0 [""])

recurseDirectories :: FilePath -> [FilePath] -> IO [FilePath]
recurseDirectories _    []         = return []
recurseDirectories base (dir:dirs) = unsafeInterleaveIO $ do
  (files, dirs') <- collect [] [] =<< listDirectory (base </> dir)

  files' <- recurseDirectories base (dirs' ++ dirs)
  return (dir : files ++ files')

  where
    collect files dirs' []              = return (reverse files, reverse dirs')
    collect files dirs' (entry:entries) = do
      let dirEntry  = dir </> entry
          dirEntry' = FilePath.Native.addTrailingPathSeparator dirEntry
      isDirectory <- doesDirectoryExist (base </> dirEntry)
      isSymlink <- pathIsSymbolicLink (base </> dirEntry)
      if isDirectory && not isSymlink
        then collect files (dirEntry':dirs') entries
        else collect (dirEntry:files) dirs' entries

getModTime :: FilePath -> IO EpochTime
getModTime path = do
  -- The directory package switched to the new time package
  t <- getModificationTime path
  return . floor . utcTimeToPOSIXSeconds $ t
