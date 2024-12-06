{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_HADDOCK hide #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Codec.Archive.Tar.Types
-- Copyright   :  (c) 2007 Bjorn Bringert,
--                    2008 Andrea Vezzosi,
--                    2008-2009 Duncan Coutts
--                    2011 Max Bolingbroke
-- License     :  BSD3
--
-- Maintainer  :  duncan@community.haskell.org
-- Portability :  portable
--
-- Types to represent the content of @.tar@ archives.
--
-----------------------------------------------------------------------------
module Codec.Archive.Tar.Types (

  GenEntry(..),
  Entry,
  entryPath,
  GenEntryContent(..),
  EntryContent,
  FileSize,
  Permissions,
  Ownership(..),
  EpochTime,
  TypeCode,
  DevMajor,
  DevMinor,
  Format(..),

  simpleEntry,
  longLinkEntry,
  longSymLinkEntry,
  fileEntry,
  symlinkEntry,
  directoryEntry,

  ordinaryFilePermissions,
  symbolicLinkPermission,
  executableFilePermissions,
  directoryPermissions,

  TarPath(..),
  toTarPath,
  toTarPath',
  ToTarPathResult(..),
  fromTarPath,
  fromTarPathToPosixPath,
  fromTarPathToWindowsPath,
  fromFilePathToNative,

  LinkTarget(..),
  toLinkTarget,
  fromLinkTarget,
  fromLinkTargetToPosixPath,
  fromLinkTargetToWindowsPath,
  fromFilePathToWindowsPath,

  GenEntries(..),
  Entries,
  mapEntries,
  mapEntriesNoFail,
  foldEntries,
  foldlEntries,
  unfoldEntries,
  unfoldEntriesM,
  ) where

import Data.Bifunctor (Bifunctor, bimap)
import Data.Int      (Int64)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Monoid   (Monoid(..))
import Data.Semigroup as Sem
import Data.Typeable
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BS.Char8
import qualified Data.ByteString.Lazy  as LBS
import Control.DeepSeq
import Control.Exception (Exception, displayException)

import qualified System.FilePath as FilePath.Native
         ( joinPath, splitDirectories, addTrailingPathSeparator, hasTrailingPathSeparator, pathSeparator, isAbsolute, hasTrailingPathSeparator )
import qualified System.FilePath.Posix as FilePath.Posix
         ( joinPath, splitPath, splitDirectories, hasTrailingPathSeparator
         , addTrailingPathSeparator, pathSeparator )
import qualified System.FilePath.Windows as FilePath.Windows
         ( joinPath, addTrailingPathSeparator, pathSeparator )
import System.Posix.Types
         ( FileMode )
import "os-string" System.OsString.Posix (PosixString, PosixChar)
import qualified "os-string" System.OsString.Posix as PS

import Codec.Archive.Tar.PackAscii

-- | File size in bytes.
type FileSize  = Int64

-- | The number of seconds since the UNIX epoch.
type EpochTime = Int64

-- | Major device number.
type DevMajor  = Int

-- | Minor device number.
type DevMinor  = Int

-- | User-defined tar format expansion.
type TypeCode  = Char

-- | Permissions information for 'GenEntry'.
type Permissions = FileMode

-- | Polymorphic tar archive entry. High-level interfaces
-- commonly work with 'GenEntry' 'FilePath' 'FilePath',
-- while low-level ones use 'GenEntry' t'TarPath' t'LinkTarget'.
--
-- @since 0.6.0.0
data GenEntry tarPath linkTarget = Entry {

    -- | The path of the file or directory within the archive.
    entryTarPath :: !tarPath,

    -- | The real content of the entry. For 'NormalFile' this includes the
    -- file data. An entry usually contains a 'NormalFile' or a 'Directory'.
    entryContent :: !(GenEntryContent linkTarget),

    -- | File permissions (Unix style file mode).
    entryPermissions :: {-# UNPACK #-} !Permissions,

    -- | The user and group to which this file belongs.
    entryOwnership :: {-# UNPACK #-} !Ownership,

    -- | The time the file was last modified.
    entryTime :: {-# UNPACK #-} !EpochTime,

    -- | The tar format the archive is using.
    entryFormat :: !Format
  }
  deriving
  ( Eq
  , Show
  , Functor -- ^ @since 0.6.4.0
  )

-- | @since 0.6.4.0
instance Bifunctor GenEntry where
  bimap f g e = e
    { entryTarPath = f (entryTarPath e)
    , entryContent = fmap g (entryContent e)
    }

-- | Monomorphic tar archive entry, ready for serialization / deserialization.
--
type Entry = GenEntry TarPath LinkTarget

-- | Low-level function to get a native 'FilePath' of the file or directory
-- within the archive, not accounting for long names. It's likely
-- that you want to apply 'Codec.Archive.Tar.decodeLongNames'
-- and use 'Codec.Archive.Tar.Entry.entryTarPath' afterwards instead of 'entryPath'.
--
entryPath :: GenEntry TarPath linkTarget -> FilePath
entryPath = fromTarPath . entryTarPath

-- | Polymorphic content of a tar archive entry. High-level interfaces
-- commonly work with 'GenEntryContent' 'FilePath',
-- while low-level ones use 'GenEntryContent' t'LinkTarget'.
--
-- Portable archives should contain only 'NormalFile' and 'Directory'.
--
-- @since 0.6.0.0
data GenEntryContent linkTarget
  = NormalFile      LBS.ByteString {-# UNPACK #-} !FileSize
  | Directory
  | SymbolicLink    !linkTarget
  | HardLink        !linkTarget
  | CharacterDevice {-# UNPACK #-} !DevMajor
                    {-# UNPACK #-} !DevMinor
  | BlockDevice     {-# UNPACK #-} !DevMajor
                    {-# UNPACK #-} !DevMinor
  | NamedPipe
  | OtherEntryType  {-# UNPACK #-} !TypeCode LBS.ByteString
                    {-# UNPACK #-} !FileSize
  deriving
  ( Eq
  , Ord
  , Show
  , Functor -- ^ @since 0.6.4.0
  )

-- | Monomorphic content of a tar archive entry,
-- ready for serialization / deserialization.
type EntryContent = GenEntryContent LinkTarget

-- | Ownership information for 'GenEntry'.
data Ownership = Ownership {
    -- | The owner user name. Should be set to @\"\"@ if unknown.
    -- Must not contain non-ASCII characters.
    ownerName :: String,

    -- | The owner group name. Should be set to @\"\"@ if unknown.
    -- Must not contain non-ASCII characters.
    groupName :: String,

    -- | Numeric owner user id. Should be set to @0@ if unknown.
    ownerId :: {-# UNPACK #-} !Int,

    -- | Numeric owner group id. Should be set to @0@ if unknown.
    groupId :: {-# UNPACK #-} !Int
  }
    deriving (Eq, Ord, Show)

-- | There have been a number of extensions to the tar file format over the
-- years. They all share the basic entry fields and put more meta-data in
-- different extended headers.
--
data Format =

     -- | This is the classic Unix V7 tar format. It does not support owner and
     -- group names, just numeric Ids. It also does not support device numbers.
     V7Format

     -- | The \"USTAR\" format is an extension of the classic V7 format. It was
     -- later standardised by POSIX. It has some restrictions but is the most
     -- portable format.
   | UstarFormat

     -- | The GNU tar implementation also extends the classic V7 format, though
     -- in a slightly different way from the USTAR format. This is the only format
     -- supporting long file names.
   | GnuFormat
  deriving (Eq, Ord, Show)

instance (NFData tarPath, NFData linkTarget) => NFData (GenEntry tarPath linkTarget) where
  rnf (Entry p c _ _ _ _) = rnf p `seq` rnf c

instance NFData linkTarget => NFData (GenEntryContent linkTarget) where
  rnf x = case x of
      NormalFile       c _  -> rnf c
      SymbolicLink lnk      -> rnf lnk
      HardLink lnk          -> rnf lnk
      OtherEntryType _ c _  -> rnf c
      _                     -> seq x ()

instance NFData Ownership where
  rnf (Ownership o g _ _) = rnf o `seq` rnf g

-- | @rw-r--r--@ for normal files
ordinaryFilePermissions :: Permissions
ordinaryFilePermissions   = 0o0644

-- | @rw-r--r--@ for normal files
--
-- @since 0.6.0.0
symbolicLinkPermission :: Permissions
symbolicLinkPermission   = 0o0777

-- | @rwxr-xr-x@ for executable files
executableFilePermissions :: Permissions
executableFilePermissions = 0o0755

-- | @rwxr-xr-x@ for directories
directoryPermissions :: Permissions
directoryPermissions  = 0o0755

-- | An entry with all default values except for the file name and type. It
-- uses the portable USTAR/POSIX format (see 'UstarFormat').
--
-- You can use this as a basis and override specific fields, eg:
--
-- > (emptyEntry name HardLink) { linkTarget = target }
--
simpleEntry :: tarPath -> GenEntryContent linkTarget -> GenEntry tarPath linkTarget
simpleEntry tarpath content = Entry {
    entryTarPath     = tarpath,
    entryContent     = content,
    entryPermissions = case content of
                         Directory -> directoryPermissions
                         SymbolicLink _ -> symbolicLinkPermission
                         _         -> ordinaryFilePermissions,
    entryOwnership   = Ownership "" "" 0 0,
    entryTime        = 0,
    entryFormat      = UstarFormat
  }

-- | A tar entry for a file.
--
-- Entry  fields such as file permissions and ownership have default values.
--
-- You can use this as a basis and override specific fields. For example if you
-- need an executable file you could use:
--
-- > (fileEntry name content) { fileMode = executableFileMode }
--
fileEntry :: tarPath -> LBS.ByteString -> GenEntry tarPath linkTarget
fileEntry name fileContent =
  simpleEntry name (NormalFile fileContent (LBS.length fileContent))

-- | A tar entry for a symbolic link.
symlinkEntry :: tarPath -> linkTarget -> GenEntry tarPath linkTarget
symlinkEntry name targetLink =
  simpleEntry name (SymbolicLink targetLink)

-- | [GNU extension](https://www.gnu.org/software/tar/manual/html_node/Standard.html)
-- to store a filepath too long to fit into 'Codec.Archive.Tar.Entry.entryTarPath'
-- as 'OtherEntryType' @\'L\'@ with the full filepath as 'entryContent'.
-- The next entry must contain the actual
-- data with truncated 'Codec.Archive.Tar.Entry.entryTarPath'.
--
-- See [What exactly is the GNU tar ././@LongLink "trick"?](https://stackoverflow.com/questions/2078778/what-exactly-is-the-gnu-tar-longlink-trick)
--
-- @since 0.6.0.0
longLinkEntry :: FilePath -> GenEntry TarPath linkTarget
longLinkEntry tarpath = Entry {
    entryTarPath     = TarPath [PS.pstr|././@LongLink|] mempty,
    entryContent     = OtherEntryType 'L' (LBS.fromStrict $ posixToByteString $ toPosixString tarpath) (fromIntegral $ length tarpath),
    entryPermissions = ordinaryFilePermissions,
    entryOwnership   = Ownership "" "" 0 0,
    entryTime        = 0,
    entryFormat      = GnuFormat
  }

-- | [GNU extension](https://www.gnu.org/software/tar/manual/html_node/Standard.html)
-- to store a link target too long to fit into 'Codec.Archive.Tar.Entry.entryTarPath'
-- as 'OtherEntryType' @\'K\'@ with the full filepath as 'entryContent'.
-- The next entry must contain the actual
-- data with truncated 'Codec.Archive.Tar.Entry.entryTarPath'.
--
-- @since 0.6.0.0
longSymLinkEntry :: FilePath -> GenEntry TarPath linkTarget
longSymLinkEntry linkTarget = Entry {
    entryTarPath     = TarPath [PS.pstr|././@LongLink|] mempty,
    entryContent     = OtherEntryType 'K' (LBS.fromStrict $ posixToByteString $ toPosixString $ linkTarget) (fromIntegral $ length linkTarget),
    entryPermissions = ordinaryFilePermissions,
    entryOwnership   = Ownership "" "" 0 0,
    entryTime        = 0,
    entryFormat      = GnuFormat
  }

-- | A tar entry for a directory.
--
-- Entry fields such as file permissions and ownership have default values.
--
directoryEntry :: tarPath -> GenEntry tarPath linkTarget
directoryEntry name = simpleEntry name Directory

--
-- * Tar paths
--

-- | The classic tar format allowed just 100 characters for the file name. The
-- USTAR format extended this with an extra 155 characters, however it uses a
-- complex method of splitting the name between the two sections.
--
-- Instead of just putting any overflow into the extended area, it uses the
-- extended area as a prefix. The aggravating insane bit however is that the
-- prefix (if any) must only contain a directory prefix. That is the split
-- between the two areas must be on a directory separator boundary. So there is
-- no simple calculation to work out if a file name is too long. Instead we
-- have to try to find a valid split that makes the name fit in the two areas.
--
-- The rationale presumably was to make it a bit more compatible with old tar
-- programs that only understand the classic format. A classic tar would be
-- able to extract the file name and possibly some dir prefix, but not the
-- full dir prefix. So the files would end up in the wrong place, but that's
-- probably better than ending up with the wrong names too.
--
-- So it's understandable but rather annoying.
--
-- * Tar paths use Posix format (ie @\'/\'@ directory separators), irrespective
--   of the local path conventions.
--
-- * The directory separator between the prefix and name is /not/ stored.
--
data TarPath = TarPath
  {-# UNPACK #-} !PosixString
  -- ^ path name, 100 characters max.
  {-# UNPACK #-} !PosixString
  -- ^ path prefix, 155 characters max.
  deriving (Eq, Ord)

instance NFData TarPath where
  rnf (TarPath _ _) = () -- fully strict by construction

instance Show TarPath where
  show = show . fromTarPath

-- | Convert a t'TarPath' to a native 'FilePath'.
--
-- The native 'FilePath' will use the native directory separator but it is not
-- otherwise checked for validity or sanity. In particular:
--
-- * The tar path may be invalid as a native path, eg the file name @\"nul\"@
--   is not valid on Windows.
--
-- * The tar path may be an absolute path or may contain @\"..\"@ components.
--   For security reasons this should not usually be allowed, but it is your
--   responsibility to check for these conditions
--   (e.g., using 'Codec.Archive.Tar.Check.checkEntrySecurity').
--
fromTarPath :: TarPath -> FilePath
fromTarPath = fromPosixString . fromTarPathInternal (PS.unsafeFromChar FilePath.Native.pathSeparator)

-- | Convert a t'TarPath' to a Unix\/Posix 'FilePath'.
--
-- The difference compared to 'fromTarPath' is that it always returns a Unix
-- style path irrespective of the current operating system.
--
-- This is useful to check how a t'TarPath' would be interpreted on a specific
-- operating system, eg to perform portability checks.
--
fromTarPathToPosixPath :: TarPath -> FilePath
fromTarPathToPosixPath = fromPosixString . fromTarPathInternal (PS.unsafeFromChar FilePath.Posix.pathSeparator)

-- | Convert a t'TarPath' to a Windows 'FilePath'.
--
-- The only difference compared to 'fromTarPath' is that it always returns a
-- Windows style path irrespective of the current operating system.
--
-- This is useful to check how a t'TarPath' would be interpreted on a specific
-- operating system, eg to perform portability checks.
--
fromTarPathToWindowsPath :: TarPath -> FilePath
fromTarPathToWindowsPath = fromPosixString . fromTarPathInternal (PS.unsafeFromChar FilePath.Windows.pathSeparator)

fromTarPathInternal :: PosixChar -> TarPath -> PosixString
fromTarPathInternal sep = go
  where
    posixSep = PS.unsafeFromChar FilePath.Posix.pathSeparator
    adjustSeps = if sep == posixSep then id else
      PS.map $ \c -> if c == posixSep then sep else c
    go (TarPath name prefix)
     | PS.null prefix = adjustSeps name
     | PS.null name = adjustSeps prefix
     | otherwise = adjustSeps prefix <> PS.cons sep (adjustSeps name)
{-# INLINE fromTarPathInternal #-}

-- | Convert a native 'FilePath' to a t'TarPath'.
--
-- The conversion may fail if the 'FilePath' is empty or too long.
toTarPath :: Bool -- ^ Is the path for a directory? This is needed because for
                  -- directories a t'TarPath' must always use a trailing @\/@.
          -> FilePath
          -> Either String TarPath
toTarPath isDir path = case toTarPath' path' of
  FileNameEmpty      -> Left "File name empty"
  FileNameOK tarPath -> Right tarPath
  FileNameTooLong{}  -> Left $ "File name too long: " ++ path'
  where
    path' = if isDir && not (FilePath.Native.hasTrailingPathSeparator path)
            then path <> [FilePath.Native.pathSeparator]
            else path

-- | Convert a native 'FilePath' to a t'TarPath'.
-- Directory paths must always have a trailing @\/@, this is not checked.
--
-- @since 0.6.0.0
toTarPath'
  :: FilePath
  -> ToTarPathResult
toTarPath'
  = splitLongPath
  . (if nativeSep == posixSep then id else adjustSeps)
  where
    nativeSep = FilePath.Native.pathSeparator
    posixSep = FilePath.Posix.pathSeparator
    adjustSeps = map $ \c -> if c == nativeSep then posixSep else c

-- | Return type of 'toTarPath''.
--
-- @since 0.6.0.0
data ToTarPathResult
  = FileNameEmpty
  -- ^ 'FilePath' was empty, but t'TarPath' must be non-empty.
  | FileNameOK TarPath
  -- ^ All good, this is just a normal t'TarPath'.
  | FileNameTooLong TarPath
  -- ^ 'FilePath' was longer than 255 characters, t'TarPath' contains
  -- a truncated part only. An actual entry must be preceded by
  -- 'longLinkEntry'.

-- | Take a sanitised path, split on directory separators and try to pack it
-- into the 155 + 100 tar file name format.
--
-- The strategy is this: take the name-directory components in reverse order
-- and try to fit as many components into the 100 long name area as possible.
-- If all the remaining components fit in the 155 name area then we win.
splitLongPath :: FilePath -> ToTarPathResult
splitLongPath path = case reverse (FilePath.Posix.splitPath path) of
  [] -> FileNameEmpty
  c : cs -> case packName nameMax (c :| cs) of
    Nothing                 -> FileNameTooLong $ TarPath (toPosixString $ take 100 path) mempty
    Just (name, [])         -> FileNameOK $! TarPath (toPosixString name) mempty
    Just (name, first:rest) -> case packName prefixMax remainder of
      Nothing               -> FileNameTooLong $ TarPath (toPosixString $ take 100 path) mempty
      Just (_     , _:_)    -> FileNameTooLong $ TarPath (toPosixString $ take 100 path) mempty
      Just (prefix, [])     -> FileNameOK $! TarPath (toPosixString name) (toPosixString prefix)
      where
        -- drop the '/' between the name and prefix:
        remainder = init first :| rest

  where
    nameMax, prefixMax :: Int
    nameMax   = 100
    prefixMax = 155

    packName :: Int -> NonEmpty FilePath -> Maybe (FilePath, [FilePath])
    packName maxLen (c :| cs)
      | n > maxLen         = Nothing
      | otherwise          = Just (packName' maxLen n [c] cs)
      where n = length c

    packName' :: Int -> Int -> [FilePath] -> [FilePath] -> (FilePath, [FilePath])
    packName' maxLen n ok (c:cs)
      | n' <= maxLen             = packName' maxLen n' (c:ok) cs
                                     where n' = n + length c
    packName' _      _ ok    cs  = (FilePath.Posix.joinPath ok, cs)

-- | The tar format allows just 100 ASCII characters for the 'SymbolicLink' and
-- 'HardLink' entry types.
--
newtype LinkTarget = LinkTarget PosixString
  deriving (Eq, Ord, Show)

instance NFData LinkTarget where
    rnf (LinkTarget bs) = rnf bs

-- | Convert a native 'FilePath' to a tar t'LinkTarget'.
-- string is longer than 100 characters or if it contains non-portable
-- characters.
toLinkTarget :: FilePath -> Maybe LinkTarget
toLinkTarget path
  | length path <= 100 = do
    target <- toLinkTarget' path
    Just $! LinkTarget (toPosixString target)
  | otherwise = Nothing

data LinkTargetException = IsAbsolute FilePath
                         | TooLong FilePath
  deriving (Show,Typeable)

instance Exception LinkTargetException where
  displayException (IsAbsolute fp) = "Link target \"" <> fp <> "\" is unexpectedly absolute"
  displayException (TooLong _) = "The link target is too long"

-- | Convert a native 'FilePath' to a unix filepath suitable for
-- using as t'LinkTarget'. Does not error if longer than 100 characters.
toLinkTarget' :: FilePath -> Maybe FilePath
toLinkTarget' path
  | FilePath.Native.isAbsolute path = Nothing
  | otherwise = Just $ adjustDirectory $ FilePath.Posix.joinPath $ FilePath.Native.splitDirectories path
  where
    adjustDirectory | FilePath.Native.hasTrailingPathSeparator path
                    = FilePath.Posix.addTrailingPathSeparator
                    | otherwise = id

-- | Convert a tar t'LinkTarget' to a native 'FilePath'.
fromLinkTarget :: LinkTarget -> FilePath
fromLinkTarget (LinkTarget pathbs) = fromFilePathToNative $ fromPosixString pathbs

-- | Convert a tar t'LinkTarget' to a Unix\/POSIX 'FilePath' (@\'/\'@ path separators).
fromLinkTargetToPosixPath :: LinkTarget -> FilePath
fromLinkTargetToPosixPath (LinkTarget pathbs) = fromPosixString pathbs

-- | Convert a tar t'LinkTarget' to a Windows 'FilePath' (@\'\\\\\'@ path separators).
fromLinkTargetToWindowsPath :: LinkTarget -> FilePath
fromLinkTargetToWindowsPath (LinkTarget pathbs) =
  fromFilePathToWindowsPath $ fromPosixString pathbs

-- | Convert a unix FilePath to a native 'FilePath'.
fromFilePathToNative :: FilePath -> FilePath
fromFilePathToNative =
  fromFilePathInternal FilePath.Posix.pathSeparator FilePath.Native.pathSeparator

-- | Convert a unix FilePath to a Windows 'FilePath'.
fromFilePathToWindowsPath :: FilePath -> FilePath
fromFilePathToWindowsPath =
  fromFilePathInternal FilePath.Posix.pathSeparator FilePath.Windows.pathSeparator

fromFilePathInternal :: Char -> Char -> FilePath -> FilePath
fromFilePathInternal fromSep toSep = adjustSeps
  where
    adjustSeps = if fromSep == toSep then id else
      map $ \c -> if c == fromSep then toSep else c
{-# INLINE fromFilePathInternal #-}

--
-- * Entries type
--

-- | Polymorphic sequence of archive entries.
-- High-level interfaces
-- commonly work with 'GenEntries' 'FilePath' 'FilePath',
-- while low-level ones use 'GenEntries' t'TarPath' t'LinkTarget'.
--
-- The point of this type as opposed to just using a list is that it makes the
-- failure case explicit. We need this because the sequence of entries we get
-- from reading a tarball can include errors.
--
-- Converting from a list can be done with just @foldr Next Done@. Converting
-- back into a list can be done with 'foldEntries' however in that case you
-- must be prepared to handle the 'Fail' case inherent in the 'Entries' type.
--
-- The 'Monoid' instance lets you concatenate archives or append entries to an
-- archive.
--
-- @since 0.6.0.0
data GenEntries tarPath linkTarget e
  = Next (GenEntry tarPath linkTarget) (GenEntries tarPath linkTarget e)
  | Done
  | Fail e
  deriving
    ( Eq
    , Show
    , Functor
    , Foldable    -- ^ @since 0.6.0.0
    , Traversable -- ^ @since 0.6.0.0
    )

infixr 5 `Next`

-- | Monomorphic sequence of archive entries,
-- ready for serialization / deserialization.
type Entries e = GenEntries TarPath LinkTarget e

-- | This is like the standard 'Data.List.unfoldr' function on lists, but for 'Entries'.
-- It includes failure as an extra possibility that the stepper function may
-- return.
--
-- It can be used to generate 'Entries' from some other type. For example it is
-- used internally to lazily unfold entries from a 'LBS.ByteString'.
--
unfoldEntries
  :: (a -> Either e (Maybe (GenEntry tarPath linkTarget, a)))
  -> a
  -> GenEntries tarPath linkTarget e
unfoldEntries f = unfold
  where
    unfold x = case f x of
      Left err             -> Fail err
      Right Nothing        -> Done
      Right (Just (e, x')) -> Next e (unfold x')

unfoldEntriesM
  :: Monad m
  => (forall a. m a -> m a)
  -- ^ id or unsafeInterleaveIO
  -> m (Either e (Maybe (GenEntry tarPath linkTarget)))
  -> m (GenEntries tarPath linkTarget e)
unfoldEntriesM interleave f = unfold
  where
    unfold = do
      f' <- f
      case f' of
        Left err       -> pure $ Fail err
        Right Nothing  -> pure Done
        Right (Just e) -> Next e <$> interleave unfold

-- | This is like the standard 'Data.List.foldr' function on lists, but for 'Entries'.
-- Compared to 'Data.List.foldr' it takes an extra function to account for the
-- possibility of failure.
--
-- This is used to consume a sequence of entries. For example it could be used
-- to scan a tarball for problems or to collect an index of the contents.
--
foldEntries
  :: (GenEntry tarPath linkTarget -> a -> a)
  -> a
  -> (e -> a)
  -> GenEntries tarPath linkTarget e -> a
foldEntries next done fail' = fold
  where
    fold (Next e es) = next e (fold es)
    fold Done        = done
    fold (Fail err)  = fail' err

-- | A 'Data.List.foldl'-like function on Entries. It either returns the final
-- accumulator result, or the failure along with the intermediate accumulator
-- value.
--
foldlEntries
  :: (a -> GenEntry tarPath linkTarget -> a)
  -> a
  -> GenEntries tarPath linkTarget e
  -> Either (e, a) a
foldlEntries f = go
  where
    go !acc (Next e es) = go (f acc e) es
    go !acc  Done       = Right acc
    go !acc (Fail err)  = Left (err, acc)

-- | This is like the standard 'Data.List.map' function on lists, but for 'Entries'. It
-- includes failure as a extra possible outcome of the mapping function.
--
-- If your mapping function cannot fail it may be more convenient to use
-- 'mapEntriesNoFail'
mapEntries
  :: (GenEntry tarPath linkTarget -> Either e' (GenEntry tarPath linkTarget))
  -- ^ Function to apply to each entry
  -> GenEntries tarPath linkTarget e
  -- ^ Input sequence
  -> GenEntries tarPath linkTarget (Either e e')
mapEntries f =
  foldEntries (\entry rest -> either (Fail . Right) (`Next` rest) (f entry)) Done (Fail . Left)

-- | Like 'mapEntries' but the mapping function itself cannot fail.
--
mapEntriesNoFail
  :: (GenEntry tarPath linkTarget -> GenEntry tarPath linkTarget)
  -> GenEntries tarPath linkTarget e
  -> GenEntries tarPath linkTarget e
mapEntriesNoFail f =
  foldEntries (Next . f) Done Fail

-- | @since 0.5.1.0
instance Sem.Semigroup (GenEntries tarPath linkTarget e) where
  a <> b = foldEntries Next b Fail a

instance Monoid (GenEntries tarPath linkTarget e) where
  mempty  = Done
  mappend = (Sem.<>)

instance (NFData tarPath, NFData linkTarget, NFData e) => NFData (GenEntries tarPath linkTarget e) where
  rnf (Next e es) = rnf e `seq` rnf es
  rnf  Done       = ()
  rnf (Fail e)    = rnf e
