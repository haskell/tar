{-# LANGUAGE CPP, BangPatterns, PatternGuards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable #-}

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
-- > import qualified Codec.Archive.Tar.Index as Tar
--
-----------------------------------------------------------------------------
module Codec.Archive.Tar.Index (

    -- | The @tar@ format does not contain an index of files within the
    -- archive. Normally, @tar@ file have to be processed linearly. It is
    -- sometimes useful however to be able to get random access to files
    -- within the archive.
    --
    -- This module provides an index of a @tar@ file. A linear pass of the
    -- @tar@ file is needed to 'build' the 'TarIndex', but thereafter you can
    -- 'lookup' paths in the @tar@ file, and then use 'hReadEntry' to
    -- seek to the right part of the file and read the entry.
    --
    -- An index cannot be used to lookup 'Directory' entries in a tar file;
    -- instead, you will get 'TarDir' entry listing all the entries in the
    -- directory.

    -- * Index type
    TarIndex,

    -- * Index lookup
    lookup,
    TarIndexEntry(..),

    -- ** I\/O operations
    TarEntryOffset,
    hReadEntry,
    hReadEntryHeader,

    -- * Index construction
    build,
    -- ** Incremental construction
    -- $incremental-construction
    IndexBuilder,
    emptyIndex,
    addNextEntry,
    skipNextEntry,
    finaliseIndex,
    resumeIndexBuilder,

    -- * Serialising indexes
    serialise,
    deserialise,
    toList,

    -- * Lower level operations with offsets and I\/O on tar files
    hReadEntryHeaderOrEof,
    hSeekEntryOffset,
    hSeekEntryContentOffset,
    hSeekEndEntryOffset,
    nextEntryOffset,
    indexEndEntryOffset,
    indexNextEntryOffset,

#ifdef TESTS
    prop_lookup,
    prop_valid,
    prop_index_matches_tar,
    prop_finalise_resume,
#endif
  ) where

import Data.Typeable (Typeable)

import Codec.Archive.Tar.Types as Tar
import Codec.Archive.Tar.Read  as Tar
import qualified Codec.Archive.Tar.Index.StringTable as StringTable
import Codec.Archive.Tar.Index.StringTable (StringTable(..))
import qualified Codec.Archive.Tar.Index.IntTrie as IntTrie
import Codec.Archive.Tar.Index.IntTrie (IntTrie(..))

import qualified System.FilePath.Posix as FilePath
import Data.Monoid (Monoid(..))
#if (MIN_VERSION_base(4,5,0))
import Data.Monoid ((<>))
#endif
import Data.Word
import Data.Int
import Data.Bits
import qualified Data.Array.Unboxed as A
import Prelude hiding (lookup)
import System.IO
import Control.Exception (throwIO)
import Control.Arrow (first)

import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS
#if MIN_VERSION_bytestring(0,10,2)
import Data.ByteString.Builder      as BS
#else
import Data.ByteString.Lazy.Builder as BS
#endif

#ifdef TESTS
import qualified Prelude
import Test.QuickCheck hiding (Result)
import Test.QuickCheck.Property (Result, exception, succeeded)
import Control.Applicative ((<$>), (<*>))
import Control.Monad (unless)
import Data.List (nub, sort, sortBy, stripPrefix, isPrefixOf)
import Data.Maybe
import Data.Function (on)
import System.IO.Unsafe (unsafePerformIO)
import Control.Exception (SomeException, try)
import Codec.Archive.Tar.Write          as Tar
import qualified Data.ByteString.Handle as HBS
#endif


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
  . filter (/= ".")
  . FilePath.splitDirectories
  where
    lookupComponents cs' []     = Just (reverse cs')
    lookupComponents cs' (c:cs) = case StringTable.lookup table c of
      Nothing  -> Nothing
      Just cid -> lookupComponents (cid:cs') cs

fromComponentId :: StringTable PathComponentId -> PathComponentId -> FilePath
fromComponentId table = StringTable.index table


-- | Build a 'TarIndex' from a sequence of tar 'Entries'. The 'Entries' are
-- assumed to start at offset @0@ within a file.
--
build :: Entries e -> Either e TarIndex
build = go emptyIndex
  where
    go !builder (Next e es) = go (addNextEntry e builder) es
    go !builder  Done       = Right $! finaliseIndex builder
    go !_       (Fail err)  = Left err


-- $incremental-construction
-- If you need more control than 'build' then you can construct the index
-- in an acumulator style using the 'IndexBuilder' and operations.
--
-- Start with the 'emptyIndex' and use 'addNextEntry' (or 'skipNextEntry') for
-- each 'Entry' in the tar file in order. Every entry must added or skipped in
-- order, otherwise the resulting 'TarIndex' will report the wrong
-- 'TarEntryOffset's. At the end use 'finaliseIndex' to get the 'TarIndex'.
--
-- For example, 'build' is simply:
--
-- > build = go emptyIndex
-- >   where
-- >     go !builder (Next e es) = go (addNextEntry e builder) es
-- >     go !builder  Done       = Right $! finaliseIndex builder
-- >     go !_       (Fail err)  = Left err


-- | The intermediate type used for incremental construction of a 'TarIndex'.
--
data IndexBuilder = IndexBuilder [(FilePath, TarEntryOffset)]
                                 {-# UNPACK #-} !TarEntryOffset

-- | The initial empty 'IndexBuilder'.
--
emptyIndex :: IndexBuilder
emptyIndex = IndexBuilder [] 0

-- | Add the next 'Entry' into the 'IndexBuilder'.
--
addNextEntry :: Entry -> IndexBuilder -> IndexBuilder
addNextEntry entry (IndexBuilder acc nextOffset) =
    IndexBuilder ((entrypath, nextOffset):acc)
                 (nextEntryOffset entry nextOffset)
  where
    !entrypath  = entryPath entry

-- | Use this function if you want to skip some entries and not add them to the
-- final 'TarIndex'.
--
skipNextEntry :: Entry -> IndexBuilder -> IndexBuilder
skipNextEntry entry (IndexBuilder acc nextOffset) =
    IndexBuilder acc (nextEntryOffset entry nextOffset)

-- | Finish accumulating 'Entry' information and build the compact 'TarIndex'
-- lookup structure.
--
finaliseIndex :: IndexBuilder -> TarIndex
finaliseIndex (IndexBuilder pathsOffsets finalOffset) =
    TarIndex pathTable pathTrie finalOffset
  where
    pathComponents = concatMap (FilePath.splitDirectories . fst) pathsOffsets
    pathTable = StringTable.construct pathComponents
    pathTrie  = IntTrie.construct
                  [ (cids, offset)
                  | (path, offset) <- pathsOffsets
                  , let Just cids = toComponentIds pathTable path ]

-- | This is the offset immediately following the entry most recently added
-- to the 'IndexBuilder'. You might use this if you need to know the offsets
-- but don't want to use the 'TarIndex' lookup structure.
-- Use with 'hSeekEntryOffset'. See also 'nextEntryOffset'.
--
indexNextEntryOffset :: IndexBuilder -> TarEntryOffset
indexNextEntryOffset (IndexBuilder _ off) = off

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
    -- NOTE: The special case for 0 is important to avoid underflow,
    -- because we are computing an unsigned TarEntryOffset (aka Word32) value
    blocks 0    = 0
    blocks size = 1 + ((fromIntegral size - 1) `div` 512)


-------------------------
-- Resume building an existing index
--

-- | Resume building an existing index
--
-- A 'TarIndex' is optimized for a highly compact and efficient in-memory
-- representation. This, however, makes it read-only. If you have an existing
-- 'TarIndex' for a large file, and want to add to it, you can translate the
-- 'TarIndex' back to an 'IndexBuilder', but be aware that this is a relatively
-- costly operation (linear in the size of the 'TarIndex').
--
-- This is the left inverse to 'finaliseIndex' (modulo ordering).
resumeIndexBuilder :: TarIndex -> IndexBuilder
resumeIndexBuilder (TarIndex pathTable pathTrie finalOffset) =
    IndexBuilder (map (first mkPath) (IntTrie.toList pathTrie)) finalOffset
  where
    mkPath :: [PathComponentId] -> FilePath
    mkPath = FilePath.joinPath . map (StringTable.index pathTable)

toList :: TarIndex -> [(FilePath, TarEntryOffset)]
toList idx = let IndexBuilder list _next = resumeIndexBuilder idx in list

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
serialise :: TarIndex -> BS.Builder
serialise (TarIndex stringTable intTrie finalOffset) =
     BS.word32BE 1 -- format version
  <> BS.word32BE finalOffset
  <> serialiseStringTable stringTable
  <> serialiseIntTrie intTrie

-- | Read the external representation back into a 'TarIndex'.
--
deserialise :: BS.ByteString -> Maybe (TarIndex, BS.ByteString)
deserialise bs
  | BS.length bs >= 8
  , let ver = readWord32BE bs 0
  , ver == 1
  = do let !finalOffset = readWord32BE bs 4
       (stringTable, bs')  <- deserialiseStringTable (BS.drop 8 bs)
       (intTrie,     bs'') <- deserialiseIntTrie bs'
       return (TarIndex stringTable intTrie finalOffset, bs'')

  | otherwise = Nothing

serialiseIntTrie :: IntTrie k v -> BS.Builder
serialiseIntTrie (IntTrie arr) =
    let (_, !ixEnd) = A.bounds arr in
    BS.word32BE (ixEnd+1)
 <> foldr (\n r -> BS.word32BE n <> r) mempty (A.elems arr)

deserialiseIntTrie :: BS.ByteString -> Maybe (IntTrie k v, BS.ByteString)
deserialiseIntTrie bs
  | BS.length bs >= 4
  , let lenArr   = readWord32BE bs 0
        lenTotal = 4 + 4 * fromIntegral lenArr
  , BS.length bs >= 4 + 4 * fromIntegral lenArr
  , let !arr = A.array (0, lenArr-1)
                      [ (i, readWord32BE bs off)
                      | (i, off) <- zip [0..lenArr-1] [4,8 .. lenTotal - 4] ]
        !bs' = BS.drop lenTotal bs
  = Just (IntTrie arr, bs')

  | otherwise
  = Nothing

serialiseStringTable :: StringTable id -> BS.Builder
serialiseStringTable (StringTable strs arr) =
      let (_, !ixEnd) = A.bounds arr in

      BS.word32BE (fromIntegral (BS.length strs))
   <> BS.word32BE (fromIntegral ixEnd + 1)
   <> BS.byteString strs
   <> foldr (\n r -> BS.word32BE n <> r) mempty (A.elems arr)

deserialiseStringTable :: BS.ByteString -> Maybe (StringTable id, BS.ByteString)
deserialiseStringTable bs
  | BS.length bs >= 8
  , let lenStrs = fromIntegral (readWord32BE bs 0)
        lenArr  = fromIntegral (readWord32BE bs 4)
        lenTotal= 8 + lenStrs + 4 * lenArr
  , BS.length bs >= lenTotal
  , let strs = BS.take lenStrs (BS.drop 8 bs)
        arr  = A.array (0, lenArr-1)
                       [ (i, readWord32BE bs off)
                       | (i, off) <- zip [0..lenArr-1]
                                         [offArrS,offArrS+4 .. offArrE]
                       ]
        offArrS = 8 + lenStrs
        offArrE = offArrS + 4 * lenArr - 1
        !stringTable = StringTable strs arr
        !bs'         = BS.drop lenTotal bs
  = Just (stringTable, bs')

  | otherwise
  = Nothing

readWord32BE :: BS.ByteString -> Int -> Word32
readWord32BE bs i =
     fromIntegral (BS.index bs (i + 0)) `shiftL` 24
   + fromIntegral (BS.index bs (i + 1)) `shiftL` 16
   + fromIntegral (BS.index bs (i + 2)) `shiftL` 8
   + fromIntegral (BS.index bs (i + 3))


-------------------------
-- Test properties
--

#ifdef TESTS

-- Not quite the properties of a finite mapping because we also have lookups
-- that result in completions.

prop_lookup :: ValidPaths -> NonEmptyFilePath -> Bool
prop_lookup (ValidPaths paths) (NonEmptyFilePath p) =
  case (lookup index p, Prelude.lookup p paths) of
    (Nothing,                    Nothing)      -> True
    (Just (TarFileEntry offset), Just offset') -> offset == offset'
    (Just (TarDir entries),      Nothing)      -> sort (nub (map fst entries))
                                               == sort (nub completions)
    _                                          -> False
  where
    index       = finaliseIndex (IndexBuilder paths 0)
    completions = [ head (FilePath.splitDirectories completion)
                  | (path,_) <- paths
                  , completion <- maybeToList $ stripPrefix (p ++ "/") path ]

prop_valid :: ValidPaths -> Bool
prop_valid (ValidPaths paths)
  | not $ StringTable.prop_valid   pathbits = error "TarIndex: bad string table"
  | not $ IntTrie.prop_lookup      intpaths = error "TarIndex: bad int trie"
  | not $ IntTrie.prop_completions intpaths = error "TarIndex: bad int trie"
  | not $ prop'                             = error "TarIndex: bad prop"
  | otherwise                               = True

  where
    index@(TarIndex pathTable _ _) = finaliseIndex (IndexBuilder paths 0)

    pathbits = concatMap (FilePath.splitDirectories . fst) paths
    intpaths = [ (cids, offset)
               | (path, offset) <- paths
               , let Just cids = toComponentIds pathTable path ]
    prop' = flip all paths $ \(file, offset) ->
      case lookup index file of
        Just (TarFileEntry offset') -> offset' == offset
        _                           -> False

newtype NonEmptyFilePath = NonEmptyFilePath FilePath deriving Show

instance Arbitrary NonEmptyFilePath where
  arbitrary = NonEmptyFilePath . FilePath.joinPath
                <$> listOf1 (elements ["a", "b", "c", "d"])

newtype ValidPaths = ValidPaths [(FilePath, TarEntryOffset)] deriving Show

instance Arbitrary ValidPaths where
  arbitrary =
      ValidPaths . makeNoPrefix <$> listOf ((,) <$> arbitraryPath <*> arbitrary)
    where
      arbitraryPath   = FilePath.joinPath
                         <$> listOf1 (elements ["a", "b", "c", "d"])
      makeNoPrefix [] = []
      makeNoPrefix ((k,v):kvs)
        | all (\(k', _) -> not (isPrefixOfOther k k')) kvs
                     = (k,v) : makeNoPrefix kvs
        | otherwise  =         makeNoPrefix kvs

      isPrefixOfOther a b = a `isPrefixOf` b || b `isPrefixOf` a

example0 :: Entries ()
example0 =
         testEntry "foo-1.0/foo-1.0.cabal" 1500 -- at block 0
  `Next` testEntry "foo-1.0/LICENSE"       2000 -- at block 4
  `Next` testEntry "foo-1.0/Data/Foo.hs"   1000 -- at block 9
  `Next` Done

example1 :: Entries ()
example1 =
  Next (testEntry "./" 1500) Done <> example0

testEntry :: FilePath -> Int64 -> Entry
testEntry name size = simpleEntry path (NormalFile mempty size)
  where
    Right path = toTarPath False name

-- | Simple tar archive containing regular files only
data SimpleTarArchive = SimpleTarArchive {
    simpleTarEntries :: Tar.Entries ()
  , simpleTarRaw     :: [(FilePath, LBS.ByteString)]
  , simpleTarBS      :: LBS.ByteString
  }

instance Show SimpleTarArchive where
  show = show . simpleTarRaw

prop_index_matches_tar :: SimpleTarArchive -> Result
prop_index_matches_tar sta =
    case unsafePerformIO (try go) of
      Left ex  -> exception "" ex
      Right () -> succeeded
  where
    go :: IO ()
    go = do
      h <- HBS.readHandle True (simpleTarBS sta)
      goEntries h 0 (simpleTarEntries sta)

    goEntries :: Handle -> TarEntryOffset -> Tar.Entries () -> IO ()
    goEntries _ _ Tar.Done =
      return ()
    goEntries _ _ (Tar.Fail _) =
      throwIO (userError "Fail entry in SimpleTarArchive")
    goEntries h offset (Tar.Next e es) = do
      goEntry h offset e
      goEntries h (nextEntryOffset e offset) es

    goEntry :: Handle -> TarEntryOffset -> Tar.Entry -> IO ()
    goEntry h offset e = do
      e' <- hReadEntry h offset
      case (Tar.entryContent e, Tar.entryContent e') of
        (Tar.NormalFile bs sz, Tar.NormalFile bs' sz') ->
          unless (sz == sz' && bs == bs') $
            throwIO $ userError "Entry mismatch"
        _otherwise ->
          throwIO $ userError "unexpected entry types"

instance Arbitrary SimpleTarArchive where
  arbitrary = do
      numEntries <- sized $ \n -> choose (0, n)
      rawEntries <- mkRaw numEntries
      let entries = mkList rawEntries
      return SimpleTarArchive {
          simpleTarEntries = mkEntries entries
        , simpleTarRaw     = rawEntries
        , simpleTarBS      = Tar.write entries
        }
    where
      mkRaw :: Int -> Gen [(FilePath, LBS.ByteString)]
      mkRaw 0 = return []
      mkRaw n = do
         -- Pick a size around 0, 1, or 2 block boundaries
         sz <- sized $ \n -> elements (take n fileSizes)
         bs <- LBS.pack `fmap` vectorOf sz arbitrary
         es <- mkRaw (n - 1)
         return $ ("file" ++ show n, bs) : es

      mkList :: [(FilePath, LBS.ByteString)] -> [Tar.Entry]
      mkList []            = []
      mkList ((fp, bs):es) = entry : mkList es
        where
          Right path = toTarPath False fp
          entry   = simpleEntry path content
          content = NormalFile bs (LBS.length bs)

      mkEntries :: [Tar.Entry] -> Tar.Entries ()
      mkEntries []     = Tar.Done
      mkEntries (e:es) = Tar.Next e (mkEntries es)

      -- Sizes around 0, 1, and 2 block boundaries
      fileSizes :: [Int]
      fileSizes = [
                           0 ,    1 ,    2
        ,  510 ,  511 ,  512 ,  513 ,  514
        , 1022 , 1023 , 1024 , 1025 , 1026
        ]

-- | 'IndexBuilder' constructed from a 'SimpleIndex'
newtype SimpleIndexBuilder = SimpleIndexBuilder IndexBuilder

instance Show SimpleIndexBuilder where
  show (SimpleIndexBuilder (IndexBuilder acc nextOffset)) =
    "IndexBuilder " ++ show acc ++ " " ++ show nextOffset

instance Arbitrary SimpleIndexBuilder where
  arbitrary = SimpleIndexBuilder . build' . simpleTarEntries <$> arbitrary
    where
      -- like 'build', but don't finalize
      build' :: Show e => Entries e -> IndexBuilder
      build' = go emptyIndex
        where
          go !builder (Next e es) = go (addNextEntry e builder) es
          go !builder  Done       = builder
          go !_       (Fail err)  = error (show err)

prop_finalise_resume :: SimpleIndexBuilder -> Bool
prop_finalise_resume (SimpleIndexBuilder index) =
    resumeIndexBuilder (finaliseIndex index) `sameIndex` index
  where
    sameIndex :: IndexBuilder -> IndexBuilder -> Bool
    sameIndex (IndexBuilder acc nextOffset) (IndexBuilder acc' nextOffset') =
         sortBy (compare `on` fst) acc == sortBy (compare `on` fst) acc'
      && nextOffset == nextOffset'
#endif

#if !(MIN_VERSION_base(4,5,0))
(<>) :: Monoid m => m -> m -> m
(<>) = mappend
#endif
