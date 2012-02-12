-----------------------------------------------------------------------------
-- |
-- Module      :  Codec.Archive.Tar
-- Copyright   :  (c) 2008-2009 Duncan Coutts
-- License     :  BSD3
--
-- Maintainer  :  duncan@community.haskell.org
-- Portability :  portable
--
-- Perform various checks on tar file entries.
--
-----------------------------------------------------------------------------
module Codec.Archive.Tar.Check (
  TarBombError(..),
  FileNameError(..),
  PortabilityError(..),

  checkSecurity,
  checkTarbomb,
  checkPortability,
  ) where

import Codec.Archive.Tar.Types

import Data.Typeable (Typeable)
import Control.Exception (Exception)
import Control.Monad (MonadPlus(mplus))
import qualified System.FilePath as FilePath.Native
         ( splitDirectories, isAbsolute, isValid )

import qualified System.FilePath.Windows as FilePath.Windows
import qualified System.FilePath.Posix   as FilePath.Posix


-- | An error that occurs if a tar file is a "tar bomb", designed to extract
-- files outside of the intended directory
data TarBombError = TarBombError FilePath
                  deriving (Typeable)

instance Exception TarBombError

instance Show TarBombError where
  show (TarBombError expectedTopDir)
    = "File in tar archive is not in the expected directory " ++ show expectedTopDir


-- | Errors arising from tar file names being in some way invalid or dangerous
data FileNameError
  = InvalidFileName FilePath
  | AbsoluteFileName FilePath
  deriving (Typeable)

instance Show FileNameError where
  show = showFileNameError Nothing

instance Exception FileNameError

showFileNameError :: Maybe PortabilityPlatform -> FileNameError -> String
showFileNameError mb_plat err = case err of
    InvalidFileName  path -> "Invalid"  ++ plat ++ " file name in tar archive: " ++ show path
    AbsoluteFileName path -> "Absolute" ++ plat ++ " file name in tar archive: " ++ show path
  where plat = maybe "" (' ':) mb_plat


-- | The name of a platform that portability issues arise from
type PortabilityPlatform = String

-- | Potential portability issues in a tar archive
data PortabilityError
  = NonPortableFormat Format
  | NonPortableFileType
  | NonPortableEntryNameChar FilePath
  | NonPortableFileName PortabilityPlatform FileNameError
  deriving (Typeable)

instance Exception PortabilityError

instance Show PortabilityError where
  show (NonPortableFormat format) = "Archive is in the " ++ fmt ++ " format"
    where fmt = case format of V7Format    -> "old Unix V7 tar"
                               UstarFormat -> "ustar" -- I never generate this but a user might
                               GnuFormat   -> "GNU tar"
  show NonPortableFileType        = "Non-portable file type in archive"
  show (NonPortableEntryNameChar posixPath)
    = "Non-portable character in archive entry name: " ++ show posixPath
  show (NonPortableFileName platform err)
    = showFileNameError (Just platform) err


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
checkSecurity :: Entries e -> Entries (Either e FileNameError)
checkSecurity = checkEntries checkEntrySecurity

checkTarbomb :: FilePath -> Entries e -> Entries (Either e TarBombError)
checkTarbomb expectedTopDir = checkEntries (checkEntryTarbomb expectedTopDir)

checkPortability :: Entries e -> Entries (Either e PortabilityError)
checkPortability = checkEntries checkEntryPortability


checkEntrySecurity :: Entry -> Maybe FileNameError
checkEntrySecurity entry = case entryContent entry of
    HardLink     link -> check (entryPath entry)
                 `mplus` check (fromLinkTarget link)
    SymbolicLink link -> check (entryPath entry)
                 `mplus` check (fromLinkTarget link)
    _                 -> check (entryPath entry)

  where
    check name
      | FilePath.Native.isAbsolute name
      = Just $ AbsoluteFileName name

      | not (FilePath.Native.isValid name)
      = Just $ InvalidFileName name

      | any (=="..") (FilePath.Native.splitDirectories name)
      = Just $ InvalidFileName name

      | otherwise = Nothing

checkEntryTarbomb :: FilePath -> Entry -> Maybe TarBombError
checkEntryTarbomb expectedTopDir entry =
  case FilePath.Native.splitDirectories (entryPath entry) of
    (topDir:_) | topDir == expectedTopDir -> Nothing
    _ -> Just $ TarBombError expectedTopDir

checkEntryPortability :: Entry -> Maybe PortabilityError
checkEntryPortability entry
  | entryFormat entry `elem` [V7Format, GnuFormat]
  = Just $ NonPortableFormat (entryFormat entry)

  | not (portableFileType (entryContent entry))
  = Just NonPortableFileType

  | not (all portableChar posixPath)
  = Just $ NonPortableEntryNameChar posixPath

  | not (FilePath.Posix.isValid posixPath)
  = Just $ NonPortableFileName "unix"    (InvalidFileName posixPath)
  | not (FilePath.Windows.isValid windowsPath)
  = Just $ NonPortableFileName "windows" (InvalidFileName windowsPath)

  | FilePath.Posix.isAbsolute posixPath
  = Just $ NonPortableFileName "unix"    (AbsoluteFileName posixPath)
  | FilePath.Windows.isAbsolute windowsPath
  = Just $ NonPortableFileName "windows" (AbsoluteFileName windowsPath)

  | any (=="..") (FilePath.Posix.splitDirectories posixPath)
  = Just $ NonPortableFileName "unix"    (InvalidFileName posixPath)
  | any (=="..") (FilePath.Windows.splitDirectories windowsPath)
  = Just $ NonPortableFileName "windows" (InvalidFileName windowsPath)

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


checkEntries :: (Entry -> Maybe e') -> Entries e -> Entries (Either e e')
checkEntries checkEntry =
  mapEntries (\entry -> maybe (Right entry) Left (checkEntry entry))
