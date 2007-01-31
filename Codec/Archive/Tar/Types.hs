module Codec.Archive.Tar.Types where

import Data.ByteString.Lazy (ByteString)
import Data.Int (Int64)
import System.Posix.Types (CMode, EpochTime)

newtype TarArchive = TarArchive { archiveEntries :: [TarEntry] }
  deriving Show

data TarEntry = TarEntry { entryHeader :: TarHeader,
                           entryData :: ByteString }
  deriving Show

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
 | TarSymLink
 | TarCharDev
 | TarBlockDev
 | TarDir
 | TarFIFO
 | TarContiguous
 | TarCustom Char
  deriving (Eq,Show)