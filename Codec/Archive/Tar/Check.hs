-----------------------------------------------------------------------------
-- |
-- Module      :  Codec.Archive.Tar
-- Copyright   :  (c) 2008-2009 Duncan Coutts
-- License     :  BSD3
--
-- Maintainer  :  duncan@haskell.org
-- Portability :  portable
--
-- Perform various checks on tar file entries.
--
-----------------------------------------------------------------------------
module Codec.Archive.Tar.Check (
  checkSecurity,
  checkTarbomb,
  checkPortability,
  ) where

import Codec.Archive.Tar.Types

import Control.Monad (MonadPlus(mplus))
import qualified System.FilePath as FilePath.Native
         ( splitDirectories, isAbsolute, isValid )

import qualified System.FilePath.Windows as FilePath.Windows
import qualified System.FilePath.Posix   as FilePath.Posix

-- | This function checks a sequence of tar entries for file name security
-- problems. It checks that:
--
-- * file paths are not absolute
--
-- * file paths do not contain any path components that are \"@..@\"
--
-- * file names are valid
--
-- These checks are from the perspective of the current OS. That means we check
-- for \"@C:\blah@\" files on Windows and \"\/blah\" files on Unix. For archive
-- entry types 'HardLink' and 'SymbolicLink' the same checks are done for the
-- link target. A failure in any entry terminates the sequence of entries with
-- an error.
--
checkSecurity :: Entries -> Entries
checkSecurity = checkEntries checkEntrySecurity

checkTarbomb :: FilePath -> Entries -> Entries
checkTarbomb expectedTopDir = checkEntries (checkEntryTarbomb expectedTopDir)

checkPortability :: Entries -> Entries
checkPortability = checkEntries checkEntryPortability


checkEntrySecurity :: Entry -> Maybe String
checkEntrySecurity entry = case entryContent entry of
    HardLink     link -> check (entryPath entry)
                 `mplus` check (fromLinkTarget link)
    SymbolicLink link -> check (entryPath entry)
                 `mplus` check (fromLinkTarget link)
    _                 -> check (entryPath entry)

  where
    check name
      | FilePath.Native.isAbsolute name
      = Just $ "Absolute file name in tar archive: " ++ show name

      | not (FilePath.Native.isValid name)
      = Just $ "Invalid file name in tar archive: " ++ show name

      | any (=="..") (FilePath.Native.splitDirectories name)
      = Just $ "Invalid file name in tar archive: " ++ show name

      | otherwise = Nothing

checkEntryTarbomb :: FilePath -> Entry -> Maybe String
checkEntryTarbomb expectedTopDir entry =
  case FilePath.Native.splitDirectories (entryPath entry) of
    (topDir:_) | topDir == expectedTopDir -> Nothing
    _ -> Just $ "File in tar archive is not in the expected directory "
             ++ show expectedTopDir

checkEntryPortability :: Entry -> Maybe String
checkEntryPortability entry
  | entryFormat entry == V7Format
  = Just "Archive is in the old Unix V7 tar format"

  | entryFormat entry == GnuFormat
  = Just "Archive is in the GNU tar format"

  | not (portableFileType (entryContent entry))
  = Just "Non-portable file type in archive"

  | not (all portableChar posixPath)
  = Just $ "Non-portable character in archive entry name: " ++ show posixPath

  | not (FilePath.Posix.isValid posixPath)
  = Just $ "Invalid unix file name in tar archive: " ++ show posixPath
  | not (FilePath.Windows.isValid windowsPath)
  = Just $ "Invalid windows file name in tar archive: " ++ show windowsPath

  | FilePath.Posix.isAbsolute posixPath
  = Just $ "Absolute unix file name in tar archive: " ++ show posixPath
  | FilePath.Windows.isAbsolute windowsPath
  = Just $ "Absolute windows file name in tar archive: " ++ show windowsPath

  | any (=="..") (FilePath.Posix.splitDirectories posixPath)
  = Just $ "Invalid unix file name in tar archive: " ++ show posixPath
  | any (=="..") (FilePath.Windows.splitDirectories windowsPath)
  = Just $ "Invalid windows file name in tar archive: " ++ show windowsPath

  | otherwise = Nothing

  where
    posixPath   = fromTarPathToPosixPath   (entryTarPath entry)
    windowsPath = fromTarPathToWindowsPath (entryTarPath entry)

    portableFileType ftype = case ftype of
      NormalFile   {} -> True
      HardLink     {} -> True
      SymbolicLink {} -> True
      Directory       -> True
      _               -> False

    portableChar c = c <= '\127'


checkEntries :: (Entry -> Maybe String) -> Entries -> Entries
checkEntries checkEntry =
  mapEntries (\entry -> maybe (Right entry) Left (checkEntry entry))
