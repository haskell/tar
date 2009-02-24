-----------------------------------------------------------------------------
-- |
-- Module      :  Codec.Archive.Tar.Entry
-- Copyright   :  (c) 2007 Bjorn Bringert,
--                    2008 Andrea Vezzosi,
--                    2008-2009 Duncan Coutts
-- License     :  BSD3
--
-- Maintainer  :  duncan@haskell.org
-- Portability :  portable
--
-- Types and functions to manipulate tar entries.
--
-- While the "Codec.Archive.Tar" module provides only the simple high level
-- api, this module provides full access to the details of tar entries. This
-- lets you inspect all the meta-data, construct entries and handle error cases
-- more precisely.
--
-- This module uses common names and so is designed to be imported qualified:
--
-- > import qualified Codec.Archive.Tar       as Tar
-- > import qualified Codec.Archive.Tar.Entry as Tar
--
-----------------------------------------------------------------------------
module Codec.Archive.Tar.Entry (

  -- * Tar entry and associated types
  Entry(..),
  fileName,
  ExtendedHeader(..),
  FileSize,
  FileMode,
  EpochTime,
  UserId,
  GroupId,
  DevMajor,
  DevMinor,
  FileType(..),

  -- * Constructing simple entry values
  emptyEntry,
  fileEntry,
  directoryEntry,

  -- * Standard file modes
  -- | For maximum portability when constructing archives use only these file
  -- modes.
  ordinaryFileMode,
  executableFileMode,
  directoryFileMode,

  -- * Constructing entries from disk files
  packFileEntry,
  packDirectoryEntry,
  getDirectoryContentsRecursive,

  -- * TarPaths
  TarPath,
  toTarPath,
  fromTarPath,
  fromTarPathToPosixPath,
  fromTarPathToWindowsPath,

  ) where

import Codec.Archive.Tar.Types
import Codec.Archive.Tar.Pack
