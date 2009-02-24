-----------------------------------------------------------------------------
-- |
-- Module      :  Codec.Archive.Tar
-- Copyright   :  (c) 2007 Bjorn Bringert,
--                    2008 Andrea Vezzosi,
--                    2008-2009 Duncan Coutts
-- License     :  BSD3
--
-- Maintainer  :  duncan@haskell.org
-- Portability :  portable
--
-----------------------------------------------------------------------------
module Codec.Archive.Tar.Pack (
    pack,
    packFileEntry,
    packDirectoryEntry,

    getDirectoryContentsRecursive,
  ) where

import Codec.Archive.Tar.Types

import qualified Data.ByteString.Lazy as BS
import System.FilePath
         ( (</>) )
import qualified System.FilePath as FilePath.Native
         ( makeRelative, addTrailingPathSeparator, hasTrailingPathSeparator )
import System.Directory
         ( getDirectoryContents, doesDirectoryExist, getModificationTime
         , Permissions(..), getPermissions )
import System.Posix.Types
         ( FileMode )
import System.Time
         ( ClockTime(..) )
import System.IO
         ( IOMode(ReadMode), openBinaryFile, hFileSize )
import System.IO.Unsafe (unsafeInterleaveIO)

-- | Creates a tar archive from a directory of files, the paths in the archive
-- will be relative to the given base directory.
--
-- This is a portable implementation of packing suitable for portable archives.
-- In particular it only constructs 'NormalFile' and 'Directory' entries. Hard
-- links and symbolic links are treated like ordinary files. It cannot be used
-- to pack directories containing recursive symbolic links. Special files like
-- FIFOs (named pipes), sockets or device files will also cause problems.
--
pack :: FilePath    -- ^ Base directory
     -> FilePath    -- ^ Directory to pack, relative to the base dir
     -> IO [Entry]
pack baseDir sourceDir = do
  files <- getDirectoryContentsRecursive (baseDir </> sourceDir)
  sequence --FIXME: add IO interleaving
    [ do tarpath <- either fail return (toTarPath ftype relPath)
         if isDir then packDirectoryEntry filepath tarpath
                  else packFileEntry      filepath tarpath
    | filepath <- files
    , let isDir   = FilePath.Native.hasTrailingPathSeparator filepath
          ftype   = if isDir then Directory else NormalFile
          relPath = FilePath.Native.makeRelative baseDir filepath ]

-- | Construct a tar 'Entry' based on a local file.
--
-- This sets the entry size, the data contained in the file and the file's
-- modification time. If the file is executable then that information is also
-- preserved. File ownership and detailed permissions are not preserved.
--
-- * Note: the file contents is read lazily.
--
packFileEntry :: FilePath -- ^ Full path to find the file on the local disk
              -> TarPath  -- ^ Path to use for the tar Entry in the archive
              -> IO Entry
packFileEntry filepath tarpath = do
  mtime   <- getModTime filepath
  mode    <- getFileMode filepath
  file    <- openBinaryFile filepath ReadMode
  size    <- hFileSize file
  content <- BS.hGetContents file
  return (fileEntry tarpath content) {
    fileMode    = mode,
    modTime     = mtime,

    -- Note: we explicitly set the fileSize here, even though fileEntry sets it
    -- too. The reason is that fileEntry does it using BS.length and we would
    -- rather not force the whole file into memory immediately.
    fileSize    = fromIntegral size
  }

-- | Construct a tar 'Entry' based on a local directory (but not its contents).
--
-- The only attribute of the directory that is used is its modification time.
-- Directory ownership and detailed permissions are not preserved.
--
packDirectoryEntry :: FilePath -- ^ Full path to find the file on the local disk
                   -> TarPath  -- ^ Path to use for the tar Entry in the archive
                   -> IO Entry
packDirectoryEntry filepath tarpath = do
  mtime   <- getModTime filepath
  return (directoryEntry tarpath) {
    modTime     = mtime
  }

-- | This is a utility function, much like 'getDirectoryContents'. The
-- difference is that it includes the contents of subdirectories.
--
-- The paths returned are all relative to the top directory. Directory paths
-- are distinguishable by having a trailing path separator
-- (see 'FilePath.Native.hasTrailingPathSeparator').
--
-- All directories are listed before the files that they contain. Amongst the
-- contents of a directory, subdirectories are listed after normal files. The
-- overall result is that files within a directory will be together in a single
-- contiguous group. This tends to improve file layout an IO performance when
-- creating or extracting tar archives.
--
-- * Note: this function returns results lazily. Subdirectories are not scanned
-- until the files entries in the parent directory have been consumed.
--
getDirectoryContentsRecursive :: FilePath -> IO [FilePath]
getDirectoryContentsRecursive dir0 =
  recurseDirectories [FilePath.Native.addTrailingPathSeparator dir0]

recurseDirectories :: [FilePath] -> IO [FilePath]
recurseDirectories []         = return []
recurseDirectories (dir:dirs) = unsafeInterleaveIO $ do
  (files, dirs') <- collect [] [] =<< getDirectoryContents dir

  files' <- recurseDirectories (dirs' ++ dirs)
  return (dir : files ++ files')

  where
    collect files dirs' []              = return (reverse files, reverse dirs')
    collect files dirs' (entry:entries) | ignore entry
                                        = collect files dirs' entries
    collect files dirs' (entry:entries) = do
      let dirEntry  = dir </> entry
          dirEntry' = FilePath.Native.addTrailingPathSeparator dirEntry
      isDirectory <- doesDirectoryExist dirEntry
      if isDirectory
        then collect files (dirEntry':dirs') entries
        else collect (dirEntry:files) dirs' entries

    ignore ['.']      = True
    ignore ['.', '.'] = True
    ignore _          = False

-- | We can't be precise because of portability, so we default to rw-r--r-- for
-- normal files and rwxr-xr-x for executables.
--
getFileMode :: FilePath -> IO FileMode
getFileMode path = do
  perms <- getPermissions path
  return $! if executable perms then executableFileMode else ordinaryFileMode

getModTime :: FilePath -> IO EpochTime
getModTime path =
    do (TOD s _) <- getModificationTime path
       return $! fromIntegral s
