module Codec.Archive.Tar.Extract where

import Codec.Archive.Tar.Types
import Codec.Archive.Tar.Util

import System.PosixCompat.Extensions
import System.PosixCompat.Files

import Control.Monad
import qualified Data.ByteString.Lazy as BS
import System.Directory
import System.Posix.Types

-- | Extracts the contents of a TAR archive into the current directory.
--
-- If problems are encountered, warnings are printed to 
-- 'stderr', and the extraction continues.
extractTarArchive :: TarArchive -> IO ()
extractTarArchive = mapM_ extractTarEntry . archiveEntries

-- | Extracts a TAR entry into the current directory.
--
-- This function throws an exception if any problems are encountered.
extractTarEntry :: TarEntry -> IO ()
extractTarEntry (TarEntry hdr cnt) = 
    do -- FIXME: more path checks?
       path <- forceRelativePath $ tarFileName hdr
       let dir = dirName path
           mode = tarFileMode hdr
       when (not (null dir)) $ createDirectoryIfMissing True dir
       case tarFileType hdr of
               TarHardLink        -> 
                   -- FIXME: sanitize link target
                   createLink (tarLinkTarget hdr) path
               TarSymbolicLink    -> 
                   -- FIXME: sanitize link target
                   createSymbolicLink (tarLinkTarget hdr) path
               TarCharacterDevice -> 
                   createCharacterDevice path mode (tarDeviceID hdr)
               TarBlockDevice     -> 
                   createBlockDevice path mode (tarDeviceID hdr)
               TarDirectory       -> createDirectoryIfMissing False path
               TarFIFO            -> createNamedPipe path mode
               _                  -> BS.writeFile path cnt
       warnIOError $ setFileMode path mode
       -- FIXME: use tarOwnerName / tarGroupName if available
       -- FIXME: gives lots of warnings if run by non-root
       --warnIOError $ setOwnerAndGroup path (tarOwnerID hdr) (tarGroupID hdr)
       setFileTimes path (tarModTime hdr) (tarModTime hdr)

createCharacterDevice :: FilePath -> FileMode -> DeviceID -> IO ()
createCharacterDevice path mode dev = createDevice path m dev
    where m = mode `unionFileModes` characterSpecialMode

createBlockDevice :: FilePath -> FileMode -> DeviceID -> IO ()
createBlockDevice path mode dev = createDevice path m dev
    where m = mode `unionFileModes` blockSpecialMode

characterSpecialMode :: FileMode
characterSpecialMode = 0o0020000

blockSpecialMode     :: FileMode
blockSpecialMode     = 0o0060000

tarDeviceID :: TarHeader -> DeviceID
tarDeviceID hdr = makeDeviceID (tarDeviceMajor hdr) (tarDeviceMajor hdr)
