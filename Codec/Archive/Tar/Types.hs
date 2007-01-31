module Codec.Archive.Tar.Types where

import Data.ByteString.Lazy (ByteString)
import Data.Int (Int64)
import System.Posix.Types (CMode, EpochTime)

-- | A TAR archive.
newtype TarArchive = TarArchive { archiveEntries :: [TarEntry] }
  deriving Show

-- | A TAR archive entry for a file or directory.
data TarEntry = TarEntry { entryHeader :: TarHeader,
                           entryData :: ByteString }
  deriving Show

-- | TAR archive entry meta-data.
data TarHeader = TarHeader 
    {
     tarFileName :: FilePath,
     tarFileMode :: CMode,
     tarOwnerID :: Int,
     tarGroupID :: Int,
     tarFileSize :: Int64,
     tarModTime :: EpochTime,
     tarFileType :: TarFileType,
     tarLinkTarget :: FilePath,
     tarOwnerName :: String,
     tarGroupName :: String,
     tarDeviceMajor :: Int,
     tarDeviceMinor :: Int
    } 
  deriving Show

data TarFileType = 
   TarNormalFile
 | TarHardLink
 | TarSymbolicLink
 | TarCharacterDevice
 | TarBlockDevice
 | TarDirectory
 | TarFIFO
 | TarContiguous
 | TarCustom Char
  deriving (Eq,Show)