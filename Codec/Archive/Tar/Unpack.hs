{-# LANGUAGE ViewPatterns #-}
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
module Codec.Archive.Tar.Unpack (
  unpack,
  ) where

import Codec.Archive.Tar.Types
import Codec.Archive.Tar.Check

import Data.Bits
         ( testBit )
import Data.List (partition)
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy as BS
import System.FilePath
         ( (</>) )
import qualified System.FilePath as FilePath.Native
         ( takeDirectory )
import System.Directory
         ( createDirectoryIfMissing, copyFile, setPermissions )
import Control.Exception
         ( Exception, throwIO )
import System.Directory
         ( setModificationTime, emptyPermissions, setOwnerReadable, setOwnerWritable
         , setOwnerExecutable, setOwnerSearchable )
import Data.Time.Clock.POSIX
         ( posixSecondsToUTCTime )
import Control.Exception as Exception
         ( catch )
import System.IO.Error
         ( isPermissionError )

-- | Create local files and directories based on the entries of a tar archive.
--
-- This is a portable implementation of unpacking suitable for portable
-- archives. It handles 'NormalFile' and 'Directory' entries and has simulated
-- support for 'SymbolicLink' and 'HardLink' entries. Links are implemented by
-- copying the target file. This therefore works on Windows as well as Unix.
-- All other entry types are ignored, that is they are not unpacked and no
-- exception is raised.
--
-- If the 'Entries' ends in an error then it is raised an an exception. Any
-- files or directories that have been unpacked before the error was
-- encountered will not be deleted. For this reason you may want to unpack
-- into an empty directory so that you can easily clean up if unpacking fails
-- part-way.
--
-- On its own, this function only checks for security (using 'checkSecurity').
-- You can do other checks by applying checking functions to the 'Entries' that
-- you pass to this function. For example:
--
-- > unpack dir (checkTarbomb expectedDir entries)
--
-- If you care about the priority of the reported errors then you may want to
-- use 'checkSecurity' before 'checkTarbomb' or other checks.
--
unpack :: Exception e => FilePath -> Entries e -> IO ()
unpack baseDir entries = do
  uEntries <- unpackEntries [] (checkSecurity entries)
  let (hardlinks, symlinks) = partition (\(_, _, x) -> x) uEntries
  -- emulate hardlinks first, in case a symlink points to it
  emulateLinks hardlinks
  emulateLinks symlinks

  where
    -- We're relying here on 'checkSecurity' to make sure we're not scribbling
    -- files all over the place.

    unpackEntries _     (Fail err)      = either throwIO throwIO err
    unpackEntries links Done            = return links
    unpackEntries links (Next entry es) = case entryContent entry of
      NormalFile file _ -> do
        extractFile (entryPermissions entry) (entryPath entry) file (entryTime entry)
        unpackEntries links es
      Directory         -> do
        extractDir (entryPath entry) (entryTime entry)
        unpackEntries links es
      HardLink     link -> (unpackEntries $! saveLink True (entryPath entry) link links) es
      SymbolicLink link -> (unpackEntries $! saveLink False (entryPath entry) link links) es
      OtherEntryType 'L' (Char8.unpack . BS.toStrict -> fn) _ ->
        case es of
             (Next entry' es') -> case entryContent entry' of
               NormalFile file _ -> do
                extractFile (entryPermissions entry') fn file (entryTime entry')
                unpackEntries links es'
               Directory         -> extractDir fn (entryTime entry')
                                 >> unpackEntries links es'
               HardLink     link -> (unpackEntries $! saveLink True fn link links) es'
               SymbolicLink link -> (unpackEntries $! saveLink False fn link links) es'
               OtherEntryType 'L' _ _ -> throwIO $ userError "Two subsequent OtherEntryType 'L'"
               _ -> unpackEntries links es'
             (Fail err)      -> either throwIO throwIO err
             Done            -> throwIO $ userError "././@LongLink without a subsequent entry"
      _ -> unpackEntries links es --ignore other file types

    extractFile permissions path content mtime = do
      -- Note that tar archives do not make sure each directory is created
      -- before files they contain, indeed we may have to create several
      -- levels of directory.
      createDirectoryIfMissing True absDir
      BS.writeFile absPath content
      setOwnerPermissions absPath permissions
      setModTime absPath mtime
      where
        absDir  = baseDir </> FilePath.Native.takeDirectory path
        absPath = baseDir </> path

    extractDir path mtime = do
      createDirectoryIfMissing True absPath
      setModTime absPath mtime
      where
        absPath = baseDir </> path

    saveLink isHardLink path link links = seq (length path)
                                        $ seq (length link')
                                        $ (path, link', isHardLink):links
      where link' = fromLinkTarget link

    emulateLinks = mapM_ $ \(relPath, relLinkTarget, isHardLink) ->
      let absPath   = baseDir </> relPath
          -- hard links link targets are always "absolute" paths in
          -- the context of the tar root
          absTarget = if isHardLink then baseDir </> relLinkTarget else FilePath.Native.takeDirectory absPath </> relLinkTarget
      in copyFile absTarget absPath

setModTime :: FilePath -> EpochTime -> IO ()
setModTime path t =
    setModificationTime path (posixSecondsToUTCTime (fromIntegral t))
      `Exception.catch` \e ->
        if isPermissionError e then return () else throwIO e

setOwnerPermissions :: FilePath -> Permissions -> IO ()
setOwnerPermissions path permissions =
  setPermissions path ownerPermissions
  where
    -- | Info on Permission bits can be found here:
    -- https://www.gnu.org/software/libc/manual/html_node/Permission-Bits.html
    ownerPermissions =
      setOwnerReadable   (testBit permissions 8) $
      setOwnerWritable   (testBit permissions 7) $
      setOwnerExecutable (testBit permissions 6) $
      setOwnerSearchable (testBit permissions 6) $
      emptyPermissions
