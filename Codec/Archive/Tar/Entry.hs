-----------------------------------------------------------------------------
-- |
-- Module      :  Codec.Archive.Tar.Entry
-- Copyright   :  (c) 2007 Bjorn Bringert,
--                    2008 Andrea Vezzosi,
--                    2008-2009 Duncan Coutts
-- License     :  BSD3
--
-- Maintainer  :  duncan@community.haskell.org
-- Portability :  portable
--
-- Types and functions to manipulate tar entries.
--
-- While the "Codec.Archive.Tar" module provides only the simple high level
-- API, this module provides full access to the details of tar entries. This
-- lets you inspect all the meta-data, construct entries and handle error cases
-- more precisely.
--
-- This module uses common names and so is designed to be imported qualified:
--
-- > import qualified Codec.Archive.Tar       as Tar
-- > import qualified Codec.Archive.Tar.Entry as Tar
--
-----------------------------------------------------------------------------

{-# LANGUAGE CPP #-}

module Codec.Archive.Tar.Entry (

  -- * Tar entry and associated types
  GenEntry(..),
  Entry,
  entryPath,
  GenEntryContent(..),
  EntryContent,
  Ownership(..),

  FileSize,
  Permissions,
  EpochTime,
  DevMajor,
  DevMinor,
  TypeCode,
  Format(..),

  -- * Constructing simple entry values
  simpleEntry,
  fileEntry,
  directoryEntry,
  longLinkEntry,
  longSymLinkEntry,

  -- * Standard file permissions
  -- | For maximum portability when constructing archives use only these file
  -- permissions.
  ordinaryFilePermissions,
  executableFilePermissions,
  directoryPermissions,

  -- * Constructing entries from disk files
  packFileEntry,
  packDirectoryEntry,
  packSymlinkEntry,
#if __GLASGOW_HASKELL__ >= 908
  {-# DEPRECATED "The re-export will be removed in future releases of tar, use directory-ospath-streaming package directly " #-}
#endif
  getDirectoryContentsRecursive,

  -- * TarPath type
  TarPath,
  toTarPath,
  fromTarPath,
  fromTarPathToPosixPath,
  fromTarPathToWindowsPath,

  -- * LinkTarget type
  LinkTarget,
  toLinkTarget,
  fromLinkTarget,
  fromLinkTargetToPosixPath,
  fromLinkTargetToWindowsPath,
  ) where

import Codec.Archive.Tar.Types
import Codec.Archive.Tar.Pack
import System.Directory.OsPath.Streaming (getDirectoryContentsRecursive)
