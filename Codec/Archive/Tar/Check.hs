-----------------------------------------------------------------------------
-- |
-- Module      :  Codec.Archive.Tar
-- Copyright   :  (c) 2008-2009 Duncan Coutts
-- License     :  BSD3
--
-- Maintainer  :  duncan@haskell.org
-- Portability :  portable
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

-- | This is pretty important. A maliciously constructed tar archives could
-- contain entries that specify bad file names. It could specify absolute file
-- names like \"@/etc/passwd@\" or relative files outside of the archive like
-- \"../../../something\".
--
-- If we did not check for file names like these when 'unpack'ing an archive
-- then we could create a security problem commonly called a \"directory
-- traversal vulnerability\". Historically, such vulnerabilites have been
-- common in packages handling tar archives.
--
-- This function checks a sequence of tar entries for such problems. It checks
-- that:
--
-- * file paths are not absolute
--
-- * file paths do not contain any path components that are \"@..@\"
--
-- * file names are valid
--
-- These checks are from the perspective of the current OS. That means we check
-- for \"@C:\blah@\" files on Windows and \"\/blah\" files on unix. For archive
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
checkEntrySecurity entry = case fileType entry of
    HardLink     -> check (fileName entry) `mplus` check (linkTarget entry)
    SymbolicLink -> check (fileName entry) `mplus` check (linkTarget entry)
    _            -> check (fileName entry)

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
  case FilePath.Native.splitDirectories (fileName entry) of
    (topDir:_) | topDir == expectedTopDir -> Nothing
    _ -> Just $ "File in tar archive is not in the expected directory "
             ++ show expectedTopDir

checkEntryPortability :: Entry -> Maybe String
checkEntryPortability entry
  | isV7 (headerExt entry)
  = Just "Archive is in the old Unix V7 tar format"

  | isGNU (headerExt entry)
  = Just "Archive is in the GNU tar format"

  | not (portableFileType (fileType entry))
  = Just "Non-portable file type in archive"

  | not (all portableChar posixPath)
  = Just $ "Non-portable character in archive entry name: " ++ show posixPath

  | not (FilePath.Posix.isValid posixPath)
  = Just $ "Invalid unix file name in tar archive: " ++ show posixPath
  | not (FilePath.Windows.isValid windowsPath)
  = Just $ "Invalid windows file name in tar archive: " ++ show windowsPath

  | not (FilePath.Posix.isAbsolute posixPath)
  = Just $ "Absolute unix file name in tar archive: " ++ show posixPath
  | not (FilePath.Windows.isAbsolute windowsPath)
  = Just $ "Absolute windows file name in tar archive: " ++ show windowsPath

  | any (=="..") (FilePath.Posix.splitDirectories posixPath)
  = Just $ "Invalid unix file name in tar archive: " ++ show posixPath
  | any (=="..") (FilePath.Windows.splitDirectories windowsPath)
  = Just $ "Invalid windows file name in tar archive: " ++ show windowsPath

  | otherwise = Nothing

  where
    posixPath   = fromTarPathToPosixPath   (filePath entry)
    windowsPath = fromTarPathToWindowsPath (filePath entry)

    isV7  V7Header  {} = True
    isV7  _            = False
    isGNU GnuHeader {} = True
    isGNU _            = False

    portableFileType ftype = case ftype of
      NormalFile   -> True
      HardLink     -> True
      SymbolicLink -> True
      Directory    -> True
      _            -> False

    portableChar c = c <= '\127'


checkEntries :: (Entry -> Maybe String) -> Entries -> Entries
checkEntries checkEntry =
  mapEntries (\entry -> maybe (Right entry) Left (checkEntry entry))
