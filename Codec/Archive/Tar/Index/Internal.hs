{-# LANGUAGE CPP, BangPatterns, PatternGuards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Codec.Archive.Tar.Index.Internal
-- Copyright   :  (c) 2010-2015 Duncan Coutts
-- License     :  BSD3
--
-- Maintainer  :  duncan@community.haskell.org
-- Portability :  portable
--
-----------------------------------------------------------------------------
module Codec.Archive.Tar.Index.Internal (

    -- * Index type
    TarIndex(..),

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

    -- * Deprecated aliases
    emptyIndex,
    finaliseIndex,

    toComponentIds,
    serialiseLBS,
    serialiseSize,
  ) where

import Data.Typeable (Typeable)

import Codec.Archive.Tar.Types as Tar
import Codec.Archive.Tar.Read  as Tar
import qualified Codec.Archive.Tar.Index.StringTable as StringTable
import Codec.Archive.Tar.Index.StringTable (StringTable, StringTableBuilder)
import qualified Codec.Archive.Tar.Index.IntTrie as IntTrie
import Codec.Archive.Tar.Index.IntTrie (IntTrie, IntTrieBuilder)

import qualified System.FilePath.Posix as FilePath
import Data.Monoid (Monoid(..))
import Data.Monoid ((<>))
import Data.Word
import Data.Int
import Data.Bits
import qualified Data.Array.Unboxed as A
import Prelude hiding (lookup)
import System.IO
import Control.Exception (assert, throwIO)
import Control.DeepSeq

import qualified Data.ByteString        as BS
import qualified Data.ByteString.Char8  as BS.Char8
import qualified Data.ByteString.Lazy   as LBS
import qualified Data.ByteString.Unsafe as BS
import Data.ByteString.Builder          as BS
import Data.ByteString.Builder.Extra    as BS (toLazyByteStringWith,
                                               untrimmedStrategy)

-- | An index of the entries in a tar file.
--
-- This index type is designed to be quite compact and suitable to store either
-- on disk or in memory.
--
data TarIndex = TarIndex

  -- As an example of how the mapping works, consider these example files:
  --   "foo/bar.hs" at offset 0
  --   "foo/baz.hs" at offset 1024
  --
  -- We split the paths into components and enumerate them.
  --   { "foo" -> TokenId 0, "bar.hs" -> TokenId 1,  "baz.hs" -> TokenId 2 }
  --
  -- We convert paths into sequences of 'TokenId's, i.e.
  --   "foo/bar.hs" becomes [PathComponentId 0, PathComponentId 1]
  --   "foo/baz.hs" becomes [PathComponentId 0, PathComponentId 2]
  --
  -- We use a trie mapping sequences of 'PathComponentId's to the entry offset:
  --  { [PathComponentId 0, PathComponentId 1] -> offset 0
  --  , [PathComponentId 0, PathComponentId 2] -> offset 1024 }

  -- The mapping of filepath components as strings to ids.
  {-# UNPACK #-} !(StringTable PathComponentId)

  -- Mapping of sequences of filepath component ids to tar entry offsets.
  {-# UNPACK #-} !(IntTrie PathComponentId TarEntryOffset)

  -- The offset immediatly after the last entry, where we would append any
  -- additional entries.
  {-# UNPACK #-} !TarEntryOffset

  deriving (Eq, Show, Typeable)

instance NFData TarIndex where
  rnf (TarIndex _ _ _) = () -- fully strict by construction

-- | The result of 'lookup' in a 'TarIndex'. It can either be a file directly,
-- or a directory entry containing further entries (and all subdirectories
-- recursively). Note that the subtrees are constructed lazily, so it's
-- cheaper if you don't look at them.
--
data TarIndexEntry = TarFileEntry {-# UNPACK #-} !TarEntryOffset
                   | TarDir [(FilePath, TarIndexEntry)]
  deriving (Show, Typeable)


newtype PathComponentId = PathComponentId Int
  deriving (Eq, Ord, Enum, Show, Typeable)

-- | An offset within a tar file. Use 'hReadEntry', 'hReadEntryHeader' or
-- 'hSeekEntryOffset'.
--
-- This is actually a tar \"record\" number, not a byte offset.
--
type TarEntryOffset = Word32


-- | Look up a given filepath in the 'TarIndex'. It may return a 'TarFileEntry'
-- containing the 'TarEntryOffset' of the file within the tar file, or if
-- the filepath identifies a directory then it returns a 'TarDir' containing
-- the list of files within that directory.
--
-- Given the 'TarEntryOffset' you can then use one of the I\/O operations:
--
-- * 'hReadEntry' to read the whole entry;
--
-- * 'hReadEntryHeader' to read just the file metadata (e.g. its length);
--
lookup :: TarIndex -> FilePath -> Maybe TarIndexEntry
lookup (TarIndex pathTable pathTrie _) path = do
    fpath  <- toComponentIds pathTable path
    tentry <- IntTrie.lookup pathTrie fpath
    return (mkIndexEntry tentry)
  where
    mkIndexEntry (IntTrie.Entry offset)        = TarFileEntry offset
    mkIndexEntry (IntTrie.Completions entries) =
      TarDir [ (fromComponentId pathTable key, mkIndexEntry entry)
             | (key, entry) <- entries ]


toComponentIds :: StringTable PathComponentId -> FilePath -> Maybe [PathComponentId]
toComponentIds table =
    lookupComponents []
  . filter (/= BS.Char8.singleton '.')
  . splitDirectories
  . BS.Char8.pack
  where
    lookupComponents cs' []     = Just (reverse cs')
    lookupComponents cs' (c:cs) = case StringTable.lookup table c of
      Nothing  -> Nothing
      Just cid -> lookupComponents (cid:cs') cs

fromComponentId :: StringTable PathComponentId -> PathComponentId -> FilePath
fromComponentId table = BS.Char8.unpack . StringTable.index table

-- | All the files in the index with their corresponding 'TarEntryOffset's.
--
-- Note that the files are in no special order. If you intend to read all or
-- most files then is is recommended to sort by the 'TarEntryOffset'.
--
toList :: TarIndex -> [(FilePath, TarEntryOffset)]
toList (TarIndex pathTable pathTrie _) =
    [ (path, off)
    | (cids, off) <- IntTrie.toList pathTrie
    , let path = FilePath.joinPath (map (fromComponentId pathTable) cids) ]


-- | Build a 'TarIndex' from a sequence of tar 'Entries'. The 'Entries' are
-- assumed to start at offset @0@ within a file.
--
build :: Entries e -> Either e TarIndex
build = go empty
  where
    go !builder (Next e es) = go (addNextEntry e builder) es
    go !builder  Done       = Right $! finalise builder
    go !_       (Fail err)  = Left err


-- $incremental-construction
-- If you need more control than 'build' then you can construct the index
-- in an accumulator style using the 'IndexBuilder' and operations.
--
-- Start with 'empty' and use 'addNextEntry' (or 'skipNextEntry') for
-- each 'Entry' in the tar file in order. Every entry must added or skipped in
-- order, otherwise the resulting 'TarIndex' will report the wrong
-- 'TarEntryOffset's. At the end use 'finalise' to get the 'TarIndex'.
--
-- For example, 'build' is simply:
--
-- > build = go empty
-- >   where
-- >     go !builder (Next e es) = go (addNextEntry e builder) es
-- >     go !builder  Done       = Right $! finalise builder
-- >     go !_       (Fail err)  = Left err


-- | The intermediate type used for incremental construction of a 'TarIndex'.
--
data IndexBuilder
   = IndexBuilder !(StringTableBuilder PathComponentId)
                  !(IntTrieBuilder PathComponentId TarEntryOffset)
   {-# UNPACK #-} !TarEntryOffset
  deriving (Eq, Show)

instance NFData IndexBuilder where
  rnf (IndexBuilder _ _ _) = () -- fully strict by construction

-- | The initial empty 'IndexBuilder'.
--
empty :: IndexBuilder
empty = IndexBuilder StringTable.empty IntTrie.empty 0

emptyIndex :: IndexBuilder
emptyIndex = empty
{-# DEPRECATED emptyIndex "Use TarIndex.empty" #-}

-- | Add the next 'Entry' into the 'IndexBuilder'.
--
addNextEntry :: Entry -> IndexBuilder -> IndexBuilder
addNextEntry entry (IndexBuilder stbl itrie nextOffset) =
    IndexBuilder stbl' itrie'
                 (nextEntryOffset entry nextOffset)
  where
    !entrypath    = splitTarPath (entryTarPath entry)
    (stbl', cids) = StringTable.inserts entrypath stbl
    itrie'        = IntTrie.insert cids nextOffset itrie

-- | Use this function if you want to skip some entries and not add them to the
-- final 'TarIndex'.
--
skipNextEntry :: Entry -> IndexBuilder -> IndexBuilder
skipNextEntry entry (IndexBuilder stbl itrie nextOffset) =
    IndexBuilder stbl itrie (nextEntryOffset entry nextOffset)

-- | Finish accumulating 'Entry' information and build the compact 'TarIndex'
-- lookup structure.
--
finalise :: IndexBuilder -> TarIndex
finalise (IndexBuilder stbl itrie finalOffset) =
    TarIndex pathTable pathTrie finalOffset
  where
    pathTable = StringTable.finalise stbl
    pathTrie  = IntTrie.finalise itrie

finaliseIndex :: IndexBuilder -> TarIndex
finaliseIndex = finalise
{-# DEPRECATED finaliseIndex "Use TarIndex.finalise" #-}

-- | This is the offset immediately following the entry most recently added
-- to the 'IndexBuilder'. You might use this if you need to know the offsets
-- but don't want to use the 'TarIndex' lookup structure.
-- Use with 'hSeekEntryOffset'. See also 'nextEntryOffset'.
--
indexNextEntryOffset :: IndexBuilder -> TarEntryOffset
indexNextEntryOffset (IndexBuilder _ _ off) = off

-- | This is the offset immediately following the last entry in the tar file.
-- This can be useful to append further entries into the tar file.
-- Use with 'hSeekEntryOffset', or just use 'hSeekEndEntryOffset' directly.
--
indexEndEntryOffset :: TarIndex -> TarEntryOffset
indexEndEntryOffset (TarIndex _ _ off) = off

-- | Calculate the 'TarEntryOffset' of the next entry, given the size and
-- offset of the current entry.
--
-- This is much like using 'skipNextEntry' and 'indexNextEntryOffset', but without
-- using an 'IndexBuilder'.
--
nextEntryOffset :: Entry -> TarEntryOffset -> TarEntryOffset
nextEntryOffset entry offset =
    offset
  + 1
  + case entryContent entry of
      NormalFile     _   size -> blocks size
      OtherEntryType _ _ size -> blocks size
      _                       -> 0
  where
    -- NOTE: to avoid underflow, do the (fromIntegral :: Int64 -> Word32) last
    blocks :: Int64 -> TarEntryOffset
    blocks size = fromIntegral (1 + (size - 1) `div` 512)

type FilePathBS = BS.ByteString

splitTarPath :: TarPath -> [FilePathBS]
splitTarPath (TarPath name prefix) =
    splitDirectories prefix ++ splitDirectories name

splitDirectories :: FilePathBS -> [FilePathBS]
splitDirectories bs =
    case BS.Char8.split '/' bs of
      c:cs | BS.null c -> BS.Char8.singleton '/' : filter (not . BS.null) cs
      cs               ->                          filter (not . BS.null) cs


-------------------------
-- Resume building an existing index
--

-- | Resume building an existing index
--
-- A 'TarIndex' is optimized for a highly compact and efficient in-memory
-- representation. This, however, makes it read-only. If you have an existing
-- 'TarIndex' for a large file, and want to add to it, you can translate the
-- 'TarIndex' back to an 'IndexBuilder'. Be aware that this is a relatively
-- costly operation (linear in the size of the 'TarIndex'), though still
-- faster than starting again from scratch.
--
-- This is the left inverse to 'finalise' (modulo ordering).
--
unfinalise :: TarIndex -> IndexBuilder
unfinalise (TarIndex pathTable pathTrie finalOffset) =
    IndexBuilder (StringTable.unfinalise pathTable)
                 (IntTrie.unfinalise pathTrie)
                 finalOffset


-------------------------
-- I/O operations
--

-- | Reads an entire 'Entry' at the given 'TarEntryOffset' in the tar file.
-- The 'Handle' must be open for reading and be seekable.
--
-- This reads the whole entry into memory strictly, not incrementally. For more
-- control, use 'hReadEntryHeader' and then read the entry content manually.
--
hReadEntry :: Handle -> TarEntryOffset -> IO Entry
hReadEntry hnd off = do
    entry <- hReadEntryHeader hnd off
    case entryContent entry of
      NormalFile       _ size -> do body <- LBS.hGet hnd (fromIntegral size)
                                    return entry {
                                      entryContent = NormalFile body size
                                    }
      OtherEntryType c _ size -> do body <- LBS.hGet hnd (fromIntegral size)
                                    return entry {
                                      entryContent = OtherEntryType c body size
                                    }
      _                       -> return entry

-- | Read the header for a 'Entry' at the given 'TarEntryOffset' in the tar
-- file. The 'entryContent' will contain the correct metadata but an empty file
-- content. The 'Handle' must be open for reading and be seekable.
--
-- The 'Handle' position is advanced to the beginning of the entry content (if
-- any). You must check the 'entryContent' to see if the entry is of type
-- 'NormalFile'. If it is, the 'NormalFile' gives the content length and you
-- are free to read this much data from the 'Handle'.
--
-- > entry <- Tar.hReadEntryHeader hnd
-- > case Tar.entryContent entry of
-- >   Tar.NormalFile _ size -> do content <- BS.hGet hnd size
-- >                               ...
--
-- Of course you don't have to read it all in one go (as 'hReadEntry' does),
-- you can use any appropriate method to read it incrementally.
--
-- In addition to I\/O errors, this can throw a 'FormatError' if the offset is
-- wrong, or if the file is not valid tar format.
--
-- There is also the lower level operation 'hSeekEntryOffset'.
--
hReadEntryHeader :: Handle -> TarEntryOffset -> IO Entry
hReadEntryHeader hnd blockOff = do
    hSeekEntryOffset hnd blockOff
    header <- LBS.hGet hnd 512
    case Tar.read header of
      Tar.Next entry _ -> return entry
      Tar.Fail e       -> throwIO e
      Tar.Done         -> fail "hReadEntryHeader: impossible"

-- | Set the 'Handle' position to the position corresponding to the given
-- 'TarEntryOffset'.
--
-- This position is where the entry metadata can be read. If you already know
-- the entry has a body (and perhaps know it's length), you may wish to seek to
-- the body content directly using 'hSeekEntryContentOffset'.
--
hSeekEntryOffset :: Handle -> TarEntryOffset -> IO ()
hSeekEntryOffset hnd blockOff =
    hSeek hnd AbsoluteSeek (fromIntegral blockOff * 512)

-- | Set the 'Handle' position to the entry content position corresponding to
-- the given 'TarEntryOffset'.
--
-- This position is where the entry content can be read using ordinary I\/O
-- operations (though you have to know in advance how big the entry content
-- is). This is /only valid/ if you /already know/ the entry has a body (i.e.
-- is a normal file).
--
hSeekEntryContentOffset :: Handle -> TarEntryOffset -> IO ()
hSeekEntryContentOffset hnd blockOff =
    hSeekEntryOffset hnd (blockOff + 1)

-- | This is a low level variant on 'hReadEntryHeader', that can be used to
-- iterate through a tar file, entry by entry.
--
-- It has a few differences compared to 'hReadEntryHeader':
--
-- * It returns an indication when the end of the tar file is reached.
--
-- * It /does not/ move the 'Handle' position to the beginning of the entry
--   content.
--
-- * It returns the 'TarEntryOffset' of the next entry.
--
-- After this action, the 'Handle' position is not in any useful place. If
-- you want to skip to the next entry, take the 'TarEntryOffset' returned and
-- use 'hReadEntryHeaderOrEof' again. Or if having inspected the 'Entry'
-- header you want to read the entry content (if it has one) then use
-- 'hSeekEntryContentOffset' on the original input 'TarEntryOffset'.
--
hReadEntryHeaderOrEof :: Handle -> TarEntryOffset
                      -> IO (Maybe (Entry, TarEntryOffset))
hReadEntryHeaderOrEof hnd blockOff = do
    hSeekEntryOffset hnd blockOff
    header <- LBS.hGet hnd 1024
    case Tar.read header of
      Tar.Next entry _ -> let !blockOff' = nextEntryOffset entry blockOff
                           in return (Just (entry, blockOff'))
      Tar.Done         -> return Nothing
      Tar.Fail e       -> throwIO e

-- | Seek to the end of a tar file, to the position where new entries can
-- be appended, and return that 'TarEntryOffset'.
--
-- If you have a valid 'TarIndex' for this tar file then you should supply it
-- because it allows seeking directly to the correct location.
--
-- If you do not have an index, then this becomes an expensive linear
-- operation because we have to read each tar entry header from the beginning
-- to find the location immediately after the last entry (this is because tar
-- files have a variable length trailer and we cannot reliably find that by
-- starting at the end). In this mode, it will fail with an exception if the
-- file is not in fact in the tar format.
--
hSeekEndEntryOffset :: Handle -> Maybe TarIndex -> IO TarEntryOffset
hSeekEndEntryOffset hnd (Just index) = do
    let offset = indexEndEntryOffset index
    hSeekEntryOffset hnd offset
    return offset

hSeekEndEntryOffset hnd Nothing = do
    size <- hFileSize hnd
    if size == 0
      then return 0
      else seekToEnd 0
  where
    seekToEnd offset = do
      mbe <- hReadEntryHeaderOrEof hnd offset
      case mbe of
        Nothing -> do hSeekEntryOffset hnd offset
                      return offset
        Just (_, offset') -> seekToEnd offset'

-------------------------
-- (de)serialisation
--

-- | The 'TarIndex' is compact in memory, and it has a similarly compact
-- external representation.
--
serialise :: TarIndex -> BS.ByteString
serialise = toStrict . serialiseLBS

-- we keep this version around just so we can check we got the size right.
serialiseLBS :: TarIndex -> LBS.ByteString
serialiseLBS index =
    BS.toLazyByteStringWith
      (BS.untrimmedStrategy (serialiseSize index) 512) LBS.empty
      (serialiseBuilder index)

serialiseSize :: TarIndex -> Int
serialiseSize (TarIndex stringTable intTrie _) =
    StringTable.serialiseSize stringTable
  + IntTrie.serialiseSize intTrie
  + 8

serialiseBuilder :: TarIndex -> BS.Builder
serialiseBuilder (TarIndex stringTable intTrie finalOffset) =
     BS.word32BE 2 -- format version
  <> BS.word32BE finalOffset
  <> StringTable.serialise stringTable
  <> IntTrie.serialise intTrie

-- | Read the external representation back into a 'TarIndex'.
--
deserialise :: BS.ByteString -> Maybe (TarIndex, BS.ByteString)
deserialise bs
  | BS.length bs < 8
  = Nothing

  | let ver = readWord32BE bs 0
  , ver == 1
  = do let !finalOffset = readWord32BE bs 4
       (stringTable, bs')  <- StringTable.deserialiseV1 (BS.drop 8 bs)
       (intTrie,     bs'') <- IntTrie.deserialise bs'
       return (TarIndex stringTable intTrie finalOffset, bs'')

  | let ver = readWord32BE bs 0
  , ver == 2
  = do let !finalOffset = readWord32BE bs 4
       (stringTable, bs')  <- StringTable.deserialiseV2 (BS.drop 8 bs)
       (intTrie,     bs'') <- IntTrie.deserialise bs'
       return (TarIndex stringTable intTrie finalOffset, bs'')

  | otherwise = Nothing

readWord32BE :: BS.ByteString -> Int -> Word32
readWord32BE bs i =
    assert (i >= 0 && i+3 <= BS.length bs - 1) $
    fromIntegral (BS.unsafeIndex bs (i + 0)) `shiftL` 24
  + fromIntegral (BS.unsafeIndex bs (i + 1)) `shiftL` 16
  + fromIntegral (BS.unsafeIndex bs (i + 2)) `shiftL` 8
  + fromIntegral (BS.unsafeIndex bs (i + 3))

toStrict :: LBS.ByteString -> BS.ByteString
toStrict = LBS.toStrict
