-----------------------------------------------------------------------------
-- |
-- Module      :  Codec.Archive.Tar.Index
-- Copyright   :  (c) 2010-2015 Duncan Coutts
-- License     :  BSD3
--
-- Maintainer  :  duncan@community.haskell.org
-- Portability :  portable
--
-- Random access to the content of a @.tar@ archive.
--
-- This module uses common names and so is designed to be imported qualified:
--
-- > import qualified Codec.Archive.Tar.Index as TarIndex
--
-----------------------------------------------------------------------------
module Codec.Archive.Tar.Index (

    -- | The @tar@ format does not contain an index of files within the
    -- archive. Normally, @tar@ file have to be processed linearly. It is
    -- sometimes useful however to be able to get random access to files
    -- within the archive.
    --
    -- This module provides an index of a @tar@ file. A linear pass of the
    -- @tar@ file is needed to 'build' the t'TarIndex', but thereafter you can
    -- 'lookup' paths in the @tar@ file, and then use 'hReadEntry' to
    -- seek to the right part of the file and read the entry.
    --
    -- An index cannot be used to lookup 'Codec.Archive.Tar.Directory' entries in a tar file;
    -- instead, you will get 'TarDir' entry listing all the entries in the
    -- directory.

    -- * Index type
    TarIndex,

    -- * Index lookup
    lookup,
    TarIndexEntry(..),
    toList,

    -- ** I\/O operations
    TarEntryOffset,
    hReadEntry,
    hReadEntryHeader,

    -- * Index construction
    build,
    -- ** Incremental construction
    -- $incremental-construction
    IndexBuilder,
    empty,
    addNextEntry,
    skipNextEntry,
    finalise,
    unfinalise,

    -- * Serialising indexes
    serialise,
    deserialise,

    -- * Lower level operations with offsets and I\/O on tar files
    hReadEntryHeaderOrEof,
    hSeekEntryOffset,
    hSeekEntryContentOffset,
    hSeekEndEntryOffset,
    nextEntryOffset,
    indexEndEntryOffset,
    indexNextEntryOffset,
  ) where

import Prelude hiding (lookup)
import Codec.Archive.Tar.Index.Internal

-- $incremental-construction
-- If you need more control than 'build' then you can construct the index
-- in an accumulator style using the t'IndexBuilder' and operations.
--
-- Start with 'empty' and use 'addNextEntry' (or 'skipNextEntry') for
-- each 'Codec.Archive.Tar.Entry.Entry' in the tar file in order. Every entry must added or skipped in
-- order, otherwise the resulting t'TarIndex' will report the wrong
-- 'TarEntryOffset's. At the end use 'finalise' to get the t'TarIndex'.
--
-- For example, 'build' is simply:
--
-- > build = go empty
-- >   where
-- >     go !builder (Next e es) = go (addNextEntry e builder) es
-- >     go !builder  Done       = Right $! finalise builder
-- >     go !_       (Fail err)  = Left err
