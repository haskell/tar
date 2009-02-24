-----------------------------------------------------------------------------
-- |
-- Module      :  Codec.Archive.Tar.Types
-- Copyright   :  (c) 2007 Bjorn Bringert,
--                    2008 Andrea Vezzosi,
--                    2008-2009 Duncan Coutts
-- License     :  BSD3
--
-- Maintainer  :  duncan@haskell.org
-- Portability :  portable
--
-- Types to represent the content of @.tar@ archives.
--
-----------------------------------------------------------------------------
module Codec.Archive.Tar.Types (

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
  toFileTypeCode,
  fromFileTypeCode,

  emptyEntry,
  fileEntry,
  directoryEntry,

  ordinaryFileMode,
  executableFileMode,
  directoryFileMode,

  TarPath(..),
  toTarPath,
  fromTarPath,
  fromTarPathToPosixPath,
  fromTarPathToWindowsPath,

  Entries(..),
  mapEntries,
  foldEntries,
  unfoldEntries,

  ) where

import Data.Int      (Int64)

import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BS.Char8
import Data.ByteString.Lazy (ByteString)

import qualified System.FilePath as FilePath.Native
         ( joinPath, splitDirectories )
import qualified System.FilePath.Posix as FilePath.Posix
         ( joinPath, splitPath, splitDirectories, addTrailingPathSeparator )
import qualified System.FilePath.Windows as FilePath.Windows
         ( joinPath )
import System.Posix.Types
         ( FileMode )

type FileSize  = Int64
type UserId    = Int
type GroupId   = Int
-- | The number of seconds since the UNIX epoch
type EpochTime = Int
type DevMajor  = Int
type DevMinor  = Int

-- | Tar archive entry
data Entry = Entry {

    -- | Path of the file or directory.
    filePath :: !TarPath,

    -- | UNIX file mode.
    fileMode :: !FileMode,

    -- | Numeric owner user id. Should be set to @0@ if unknown.
    ownerId :: !UserId,

    -- | Numeric owner group id. Should be set to @0@ if unknown.
    groupId :: !GroupId,

    -- | File size in bytes. Should be 0 for entries other than normal files.
    fileSize :: !FileSize,

    -- | Last modification time.
    modTime :: !EpochTime,

    -- | Type of this entry.
    fileType :: FileType,

    -- | If the entry is a hard link or a symbolic link, this is the path of
    -- the link target. For all other entry types this should be @\"\"@.
    -- The maximum length of this is 100 ASCII characters.
    linkTarget :: FilePath,

    -- | The remaining meta-data is in the V7, USTAR/POSIX or GNU formats.
    -- For V7 there is no extended info at all. The information for USTAR/POSIX
    -- and the GNU format is the same though the kind affects the way the
    -- information is encoded.
    headerExt :: ExtendedHeader,

    -- | Entry contents. For entries other than normal files, this should be an
    -- empty string.
    fileContent :: ByteString
  }

-- | 'FilePath' of the file or directory within the archive.
--
-- The difference between this function and 'filePath' is that it gives us back
-- a native 'FilePath' rather than a archive 'TarPath'.
--
fileName :: Entry -> FilePath
fileName = fromTarPath . filePath

-- | There have been a number of extensions to the tar file format over the
-- years. They all share the basic 'Entry' fields and put more meta-data in
-- different extended headers.
--
data ExtendedHeader =

     -- | This is the classic Unix V7 tar header format. This format contains
     -- just the basic 'Entry' fields.
     V7Header

     -- | The \"USTAR\" format is an extension of the classic V7 format. It was
     -- later standardised by POSIX. It has some restructions but is the most
     -- portable format.
     --
   | UstarHeader {
    -- | The owner user name. Should be set to @\"\"@ if unknown.
    ownerName :: String,

    -- | The owner group name. Should be set to @\"\"@ if unknown.
    groupName :: String,

    -- | For character and block device entries, this is the
    -- major number of the device. For all other entry types, it
    -- should be set to @0@.
    deviceMajor :: !DevMajor,

    -- | For character and block device entries, this is the
    -- minor number of the device. For all other entry types, it
    -- should be set to @0@.
    deviceMinor :: !DevMinor
   }

     -- | The GNU tar implementation also extends the classic V7 format, though
     -- in a slightly different way from the USTAR format. In general for new
     -- archives the standard USTAR/POSIX should be used.
     --
   | GnuHeader {
    -- | The owner user name. Should be set to @\"\"@ if unknown.
    ownerName :: String,

    -- | The owner group name. Should be set to @\"\"@ if unknown.
    groupName :: String,

    -- | For unix character and block device entries, this is the major number
    -- of the device. For all other entry types, it should be set to @0@.
    deviceMajor :: !DevMajor,

    -- | For unix character and block device entries, this is the minor number
    -- of the device. For all other entry types, it should be set to @0@.
    deviceMinor :: !DevMinor
   }

-- | Tar archive entry types.
--
-- In portable archives you should only use 'NormalFile' and 'Directory'.
--
data FileType = NormalFile
              | HardLink
              | SymbolicLink
              | CharacterDevice
              | BlockDevice
              | Directory
              | FIFO
              | ExtendedHeader
              | GlobalHeader
              | Custom Char   -- ^ 'A' .. 'Z'
              | Reserved Char -- ^ other \/ reserved \/ unknown
  deriving Eq

toFileTypeCode :: FileType -> Char
toFileTypeCode NormalFile      = '0'
toFileTypeCode HardLink        = '1'
toFileTypeCode SymbolicLink    = '2'
toFileTypeCode CharacterDevice = '3'
toFileTypeCode BlockDevice     = '4'
toFileTypeCode Directory       = '5'
toFileTypeCode FIFO            = '6'
toFileTypeCode ExtendedHeader  = 'x'
toFileTypeCode GlobalHeader    = 'g'
toFileTypeCode (Custom   c)    = c
toFileTypeCode (Reserved c)    = c

fromFileTypeCode :: Char -> FileType
fromFileTypeCode '0'  = NormalFile
fromFileTypeCode '\0' = NormalFile
fromFileTypeCode '1'  = HardLink
fromFileTypeCode '2'  = SymbolicLink
fromFileTypeCode '3'  = CharacterDevice
fromFileTypeCode '4'  = BlockDevice
fromFileTypeCode '5'  = Directory
fromFileTypeCode '6'  = FIFO
fromFileTypeCode '7'  = NormalFile
fromFileTypeCode 'x'  = ExtendedHeader
fromFileTypeCode 'g'  = GlobalHeader
fromFileTypeCode  c   | c >= 'A' && c <= 'Z'
                      = Custom c
fromFileTypeCode  c   = Reserved c

-- | @rw-r--r--@ for normal files
ordinaryFileMode :: FileMode
ordinaryFileMode   = 0o0644

-- | @rwxr-xr-x@ for executable files
executableFileMode :: FileMode
executableFileMode = 0o0755

-- | @rwxr-xr-x@ for directories
directoryFileMode :: FileMode
directoryFileMode  = 0o0755

-- | An 'Entry' with all default values except for the file name and type. It
-- uses the portable USTAR/POSIX format (see 'UstarHeader').
--
-- You can use this as a basis and override specific fields, eg:
--
-- > (emptyEntry HardLink name) { linkTarget = target }
--
emptyEntry :: FileType -> TarPath -> Entry
emptyEntry ftype tarpath = Entry {
    filePath = tarpath,
    fileMode = case ftype of
                 Directory -> directoryFileMode
                 _         -> ordinaryFileMode,
    ownerId  = 0,
    groupId  = 0,
    fileSize = 0,
    modTime  = 0,
    fileType = ftype,
    linkTarget = "",
    headerExt  = UstarHeader {
      ownerName = "",
      groupName = "",
      deviceMajor = 0,
      deviceMinor = 0
    },
    fileContent = BS.empty
  }

-- | A tar 'Entry' for a file.
--
-- Entry  fields such as file permissions and ownership have default values.
--
-- You can use this as a basis and override specific fields. For example if you
-- need an executable file you could use:
--
-- > (fileEntry name content) { fileMode = executableFileMode }
--
fileEntry :: TarPath -> ByteString -> Entry
fileEntry name content = (emptyEntry NormalFile name) {
    fileSize = BS.length content,
    fileContent = content
  }

-- | A tar 'Entry' for a directory.
--
-- Entry fields such as file permissions and ownership have default values.
--
directoryEntry :: TarPath -> Entry
directoryEntry name = emptyEntry Directory name

--
-- * Tar paths
--

-- | The classic tar format allowed just 100 charcters for the file name. The
-- USTAR format extended this with an extra 155 characters, however it uses a
-- complex method of splitting the name between the two sections.
--
-- Instead of just putting any overflow into the extended area, it uses the
-- extended area as a prefix. The agrevating insane bit however is that the
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
-- * Tar paths use posix format (ie @\'/\'@ directory separators), irrespective
--   of the local path conventions.
--
-- * The directory separator between the prefix and name is /not/ stored.
--
data TarPath = TarPath FilePath -- path name, 100 characters max.
                       FilePath -- path prefix, 155 characters max.

-- | Convert a 'TarPath' to a native 'FilePath'.
--
-- The native 'FilePath' will use the native directory separator but it is not
-- otherwise checked for validity or sanity. In particular:
--
-- * The tar path may be invalid as a native path, eg the filename @\"nul\"@ is
--   not valid on Windows.
--
-- * The tar path may be an absolute path or may contain @\"..\"@ components.
--   For security reasons this should not usually be allowed, but it is your
--   responsibility to check for these conditions (eg using 'checkSecurity').
--
fromTarPath :: TarPath -> FilePath
fromTarPath (TarPath name prefix) =
  FilePath.Native.joinPath $ FilePath.Posix.splitDirectories prefix
                          ++ FilePath.Posix.splitDirectories name

-- | Convert a 'TarPath' to a Unix/Posix 'FilePath'.
--
-- The difference compared to 'fromTarPath' is that it always returns a Unix
-- style path irrespective of the current operating system.
--
-- This is useful to check how a 'TarPath' would be interpreted on a specific
-- operating system, eg to perform portability checks.
--
fromTarPathToPosixPath :: TarPath -> FilePath
fromTarPathToPosixPath (TarPath name prefix) =
  FilePath.Posix.joinPath $ FilePath.Posix.splitDirectories prefix
                         ++ FilePath.Posix.splitDirectories name

-- | Convert a 'TarPath' to a Windows 'FilePath'.
--
-- The only difference compared to 'fromTarPath' is that it always returns a
-- Windows style path irrespective of the current operating system.
--
-- This is useful to check how a 'TarPath' would be interpreted on a specific
-- operating system, eg to perform portability checks.
--
fromTarPathToWindowsPath :: TarPath -> FilePath
fromTarPathToWindowsPath (TarPath name prefix) =
  FilePath.Windows.joinPath $ FilePath.Posix.splitDirectories prefix
                           ++ FilePath.Posix.splitDirectories name

-- | Convert a native 'FilePath' to a 'TarPath'. The 'FileType' parameter is
-- needed because for directories a 'TarPath' always uses a trailing @\/@.
--
-- The conversion may fail if the 'FilePath' is too long. See 'TarPath' for a
-- description of the problem with splitting long 'FilePath's.
--
toTarPath :: FileType -> FilePath -> Either String TarPath
toTarPath ftype = splitLongPath
                . addTrailingSep ftype
                . FilePath.Posix.joinPath
                . FilePath.Native.splitDirectories
  where
    addTrailingSep Directory = FilePath.Posix.addTrailingPathSeparator
    addTrailingSep _         = id

-- | Take a sanitized path, split on directory separators and try to pack it
-- into the 155 + 100 tar file name format.
--
-- The stragey is this: take the name-directory components in reverse order
-- and try to fit as many components into the 100 long name area as possible.
-- If all the remaining components fit in the 155 name area then we win.
--
splitLongPath :: FilePath -> Either String TarPath
splitLongPath path =
  case packName nameMax (reverse (FilePath.Posix.splitPath path)) of
    Left err                 -> Left err
    Right (name, [])         -> Right (TarPath name "")
    Right (name, first:rest) -> case packName prefixMax remainder of
      Left err               -> Left err
      Right (_     , (_:_))  -> Left "File name too long (cannot split)"
      Right (prefix, [])     -> Right (TarPath name prefix)
      where
        -- drop the '/' between the name and prefix:
        remainder = init first : rest

  where
    nameMax, prefixMax :: Int
    nameMax   = 100
    prefixMax = 155

    packName _      []     = Left "File name empty"
    packName maxLen (c:cs)
      | n > maxLen         = Left "File name too long"
      | otherwise          = Right (packName' maxLen n [c] cs)
      where n = length c

    packName' maxLen n ok (c:cs)
      | n' <= maxLen             = packName' maxLen n' (c:ok) cs
                                     where n' = n + length c
    packName' _      _ ok    cs  = (FilePath.Posix.joinPath ok, cs)

--
-- * Entries type
--

-- | A tar archive is a sequence of entries.
--
-- The point of this type as opposed to just using a list is that it makes the
-- failure case explicit. We need this because the sequence of entries we get
-- from reading a tarball can include errors.
--
-- It is a concrete data type so you can manipulate it directly but it is often
-- clearer to use the provided functions for mapping, folding and unfolding.
--
-- Converting from a list can be done with just @foldr Next Done@. Converting
-- back into a list can be done with 'foldEntries' however in that case you
-- must be prepared to handle the 'Fail' case inherent in the 'Entries' type.
--
data Entries = Next Entry Entries
             | Done
             | Fail String

-- | This is like the standard 'unfoldr' function on lists, but for 'Entries'.
-- It includes failure as an extra posibility that the stepper function may
-- return.
--
-- It can be used to generate 'Entries' from some other type. For example it is
-- used internally to lazily unfold entries from a 'ByteString'.
--
unfoldEntries :: (a -> Either String (Maybe (Entry, a))) -> a -> Entries
unfoldEntries f = unfold
  where
    unfold x = case f x of
      Left err             -> Fail err
      Right Nothing        -> Done
      Right (Just (e, x')) -> Next e (unfold x')

-- | This is like the standard 'foldr' function on lists, but for 'Entries'.
-- Compared to 'foldr' it takes an extra function to account for the posibility
-- of failure.
--
-- This is used to consume a sequence of entries. For example it could be used
-- to scan a tarball for problems or to collect an index of the contents.
--
foldEntries :: (Entry -> a -> a) -> a -> (String -> a) -> Entries -> a
foldEntries next done fail' = fold
  where
    fold (Next e es) = next e (fold es)
    fold Done        = done
    fold (Fail err)  = fail' err

-- | This is like the standard 'map' function on lists, but for 'Entries'. It
-- includes failure as a extra possible outcome of the mapping function.
--
mapEntries :: (Entry -> Either String Entry) -> Entries -> Entries
mapEntries f =
  foldEntries (\entry rest -> either Fail (flip Next rest) (f entry)) Done Fail
