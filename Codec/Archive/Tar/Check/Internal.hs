{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_HADDOCK hide #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Codec.Archive.Tar.Check.Internal
-- Copyright   :  (c) 2008-2012 Duncan Coutts
--                    2011 Max Bolingbroke
-- License     :  BSD3
--
-- Maintainer  :  duncan@community.haskell.org
-- Portability :  portable
--
-- Perform various checks on tar file entries.
--
-----------------------------------------------------------------------------
module Codec.Archive.Tar.Check.Internal (

  -- * Security
  checkSecurity,
  checkEntrySecurity,
  FileNameError(..),

  -- * Tarbombs
  checkTarbomb,
  checkEntryTarbomb,
  TarBombError(..),

  -- * Portability
  checkPortability,
  checkEntryPortability,
  PortabilityError(..),
  PortabilityPlatform,
  ) where

import Codec.Archive.Tar.LongNames
import Codec.Archive.Tar.Types
import Control.Applicative ((<|>))
import qualified Data.ByteString.Lazy.Char8 as Char8
import Data.Maybe (fromMaybe)
import Data.Typeable (Typeable)
import Control.Exception (Exception(..))
import qualified System.FilePath as FilePath.Native
         ( splitDirectories, isAbsolute, isValid, (</>), takeDirectory, hasDrive )

import qualified System.FilePath.Windows as FilePath.Windows
import qualified System.FilePath.Posix   as FilePath.Posix


--------------------------
-- Security
--

-- | This function checks a sequence of tar entries for file name security
-- problems. It checks that:
--
-- * file paths are not absolute
--
-- * file paths do not refer outside of the archive
--
-- * file names are valid
--
-- These checks are from the perspective of the current OS. That means we check
-- for \"@C:\blah@\" files on Windows and \"\/blah\" files on Unix. For archive
-- entry types 'HardLink' and 'SymbolicLink' the same checks are done for the
-- link target. A failure in any entry terminates the sequence of entries with
-- an error.
--
-- Whenever possible, consider fusing 'Codec.Archive.Tar.Check.checkSecurity'
-- with packing / unpacking by using
-- 'Codec.Archive.Tar.packAndCheck' / 'Codec.Archive.Tar.unpackAndCheck'
-- with 'Codec.Archive.Tar.Check.checkEntrySecurity'.
-- Not only it is faster, but also alleviates issues with lazy I/O
-- such as exhaustion of file handlers.
checkSecurity
  :: Entries e
  -> GenEntries FilePath FilePath (Either (Either e DecodeLongNamesError) FileNameError)
checkSecurity = checkEntries checkEntrySecurity . decodeLongNames

-- | Worker of 'Codec.Archive.Tar.Check.checkSecurity'.
--
-- @since 0.6.0.0
checkEntrySecurity :: GenEntry FilePath FilePath -> Maybe FileNameError
checkEntrySecurity e =
  check (entryTarPath e) <|>
  case entryContent e of
    HardLink     link ->
      check link
    SymbolicLink link ->
      check (FilePath.Posix.takeDirectory (entryTarPath e) FilePath.Posix.</> link)
    _ -> Nothing
  where
    checkPosix name
      | FilePath.Posix.isAbsolute name
      = Just $ AbsoluteFileName name
      | not (FilePath.Posix.isValid name)
      = Just $ InvalidFileName name
      | not (isInsideBaseDir (FilePath.Posix.splitDirectories name))
      = Just $ UnsafeLinkTarget name
      | otherwise = Nothing

    checkNative (fromFilePathToNative -> name)
      | FilePath.Native.isAbsolute name || FilePath.Native.hasDrive name
      = Just $ AbsoluteFileName name
      | not (FilePath.Native.isValid name)
      = Just $ InvalidFileName name
      | not (isInsideBaseDir (FilePath.Native.splitDirectories name))
      = Just $ UnsafeLinkTarget name
      | otherwise = Nothing

    check name = checkPosix name <|> checkNative (fromFilePathToNative name)

isInsideBaseDir :: [FilePath] -> Bool
isInsideBaseDir = go 0
  where
    go :: Word -> [FilePath] -> Bool
    go !_ [] = True
    go 0 (".." : _) = False
    go lvl (".." : xs) = go (lvl - 1) xs
    go lvl ("." : xs) = go lvl xs
    go lvl (_ : xs) = go (lvl + 1) xs

-- | Errors arising from tar file names being in some way invalid or dangerous
data FileNameError
  = InvalidFileName FilePath
  | AbsoluteFileName FilePath
  | UnsafeLinkTarget FilePath
  -- ^ @since 0.6.0.0
  deriving (Typeable)

instance Show FileNameError where
  show = showFileNameError Nothing

instance Exception FileNameError

showFileNameError :: Maybe PortabilityPlatform -> FileNameError -> String
showFileNameError mb_plat err = case err of
    InvalidFileName  path -> "Invalid"  ++ plat ++ " file name in tar archive: " ++ show path
    AbsoluteFileName path -> "Absolute" ++ plat ++ " file name in tar archive: " ++ show path
    UnsafeLinkTarget path -> "Unsafe"   ++ plat ++ " link target in tar archive: " ++ show path
  where plat = maybe "" (' ':) mb_plat


--------------------------
-- Tarbombs
--

-- | This function checks a sequence of tar entries for being a \"tar bomb\".
-- This means that the tar file does not follow the standard convention that
-- all entries are within a single subdirectory, e.g. a file \"foo.tar\" would
-- usually have all entries within the \"foo/\" subdirectory.
--
-- Given the expected subdirectory, this function checks all entries are within
-- that subdirectroy.
--
-- Note: This check must be used in conjunction with 'Codec.Archive.Tar.Check.checkSecurity'
-- (or 'Codec.Archive.Tar.Check.checkPortability').
--
-- Whenever possible, consider fusing 'Codec.Archive.Tar.Check.checkTarbomb'
-- with packing / unpacking by using
-- 'Codec.Archive.Tar.packAndCheck' / 'Codec.Archive.Tar.unpackAndCheck'
-- with 'Codec.Archive.Tar.Check.checkEntryTarbomb'.
-- Not only it is faster, but also alleviates issues with lazy I/O
-- such as exhaustion of file handlers.
checkTarbomb
  :: FilePath
  -> Entries e
  -> GenEntries FilePath FilePath (Either (Either e DecodeLongNamesError) TarBombError)
checkTarbomb expectedTopDir
  = checkEntries (checkEntryTarbomb expectedTopDir)
  . decodeLongNames

-- | Worker of 'checkTarbomb'.
--
-- @since 0.6.0.0
checkEntryTarbomb :: FilePath -> GenEntry FilePath linkTarget -> Maybe TarBombError
checkEntryTarbomb expectedTopDir entry = do
  case entryContent entry of
    -- Global extended header aka XGLTYPE aka pax_global_header
    -- https://pubs.opengroup.org/onlinepubs/9699919799/utilities/pax.html#tag_20_92_13_02
    OtherEntryType 'g' _ _ -> Nothing
    -- Extended header referring to the next file in the archive aka XHDTYPE
    OtherEntryType 'x' _ _ -> Nothing
    _                      ->
      case FilePath.Posix.splitDirectories (entryTarPath entry) of
        (topDir:_) | topDir == expectedTopDir -> Nothing
        _ -> Just $ TarBombError expectedTopDir (entryTarPath entry)

-- | An error that occurs if a tar file is a \"tar bomb\" that would extract
-- files outside of the intended directory.
data TarBombError
  = TarBombError
    FilePath -- ^ Path inside archive.
             --
             -- @since 0.6.0.0
    FilePath -- ^ Expected top directory.
  deriving (Typeable)

instance Exception TarBombError

instance Show TarBombError where
  show (TarBombError expectedTopDir tarBombPath)
    = "File in tar archive, " ++ show tarBombPath ++
    ", is not in the expected directory " ++ show expectedTopDir

--------------------------
-- Portability
--

-- | This function checks a sequence of tar entries for a number of portability
-- issues. It will complain if:
--
-- * The old \"Unix V7\" or \"gnu\" formats are used. For maximum portability
--   only the POSIX standard \"ustar\" format should be used.
--
-- * A non-portable entry type is used. Only ordinary files, hard links,
--   symlinks and directories are portable. Device files, pipes and others are
--   not portable between all common operating systems.
--
-- * Non-ASCII characters are used in file names. There is no agreed portable
--   convention for Unicode or other extended character sets in file names in
--   tar archives.
--
-- * File names that would not be portable to both Unix and Windows. This check
--   includes characters that are valid in both systems and the \'/\' vs \'\\\'
--   directory separator conventions.
--
-- Whenever possible, consider fusing 'checkPortability' with packing / unpacking by using
-- 'Codec.Archive.Tar.packAndCheck' / 'Codec.Archive.Tar.unpackAndCheck'
-- with 'checkEntryPortability'.
-- Not only it is faster, but also alleviates issues with lazy I/O
-- such as exhaustion of file handlers.
checkPortability
  :: Entries e
  -> GenEntries FilePath FilePath (Either (Either e DecodeLongNamesError) PortabilityError)
checkPortability = checkEntries checkEntryPortability . decodeLongNames

-- | Worker of 'checkPortability'.
--
-- @since 0.6.0.0
checkEntryPortability :: GenEntry FilePath linkTarget -> Maybe PortabilityError
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
    posixPath   = entryTarPath entry
    windowsPath = fromFilePathToWindowsPath posixPath

    portableFileType ftype = case ftype of
      NormalFile   {} -> True
      HardLink     {} -> True
      SymbolicLink {} -> True
      Directory       -> True
      _               -> False

    portableChar c = c <= '\127'

-- | Portability problems in a tar archive
data PortabilityError
  = NonPortableFormat Format
  | NonPortableFileType
  | NonPortableEntryNameChar FilePath
  | NonPortableFileName PortabilityPlatform FileNameError
  deriving (Typeable)

-- | The name of a platform that portability issues arise from
type PortabilityPlatform = String

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

--------------------------
-- Utils

checkEntries
  :: (GenEntry tarPath linkTarget -> Maybe e')
  -> GenEntries tarPath linkTarget e
  -> GenEntries tarPath linkTarget (Either e e')
checkEntries checkEntry =
  mapEntries (\entry -> maybe (Right entry) Left (checkEntry entry))
