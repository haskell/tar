module Codec.Archive.Tar.Types where

import Data.ByteString.Lazy (ByteString)
import Data.Int (Int64)
import System.Posix.Types (FileMode, UserID, GroupID, EpochTime)
import System.PosixCompat.Extensions (CMajor, CMinor)

-- | A TAR archive.
newtype TarArchive = TarArchive { archiveEntries :: [TarEntry] }
  deriving Show

-- | A TAR archive entry for a file or directory.
data TarEntry = TarEntry { 
                          -- | Entry meta-data.
                          entryHeader :: TarHeader,
                          -- | Entry contents. For entries other than normal 
                          -- files, this should be an empty string.
                          entryData :: ByteString
                         }
  deriving Show

-- | TAR archive entry meta-data.
data TarHeader = TarHeader 
    {
     -- | Path of the file or directory. The path separator should be @/@ 
     -- for portable TAR archives.
     tarFileName :: FilePath,
     -- | UNIX file mode.
     tarFileMode :: FileMode,
     -- | Numeric owner user id. Should be set to @0@ if unknown.
     tarOwnerID :: UserID,
     -- | Numeric owner group id. Should be set to @0@ if unknown.
     tarGroupID :: GroupID,
     -- | File size in bytes. Should be 0 for entries other than normal files.
     tarFileSize :: Int64,
     -- | Last modification time, expressed as the number of seconds
     -- since the UNIX epoch.
     tarModTime :: EpochTime,
     -- | Type of this entry.
     tarFileType :: TarFileType,
     -- | If the entry is a hard link or a symbolic link, this is the path of
     -- the link target. For all other entry types this should be @\"\"@.
     tarLinkTarget :: FilePath,
     -- | The owner user name. Should be set to @\"\"@ if unknown.
     tarOwnerName :: String,
     -- | The owner group name. Should be set to @\"\"@ if unknown.
     tarGroupName :: String,
     -- | For character and block device entries, this is the 
     -- major number of the device. For all other entry types, it
     -- should be set to @0@.
     tarDeviceMajor :: CMajor,
     -- | For character and block device entries, this is the 
     -- minor number of the device. For all other entry types, it
     -- should be set to @0@.
     tarDeviceMinor :: CMinor
    } 
  deriving Show

-- | TAR archive entry types.
data TarFileType = 
   TarNormalFile
 | TarHardLink
 | TarSymbolicLink
 | TarCharacterDevice
 | TarBlockDevice
 | TarDirectory
 | TarFIFO
 | TarOther Char
  deriving (Eq,Show)