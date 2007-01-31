module Codec.Archive.Tar.Extract where

import Codec.Archive.Tar.Types
import Codec.Archive.Tar.Util

import Control.Monad
import Data.Bits
import qualified Data.ByteString.Lazy as BS
import System.Directory
import System.IO.Error
import System.Posix.Types


extractTarArchive :: TarArchive -> IO ()
extractTarArchive = mapM_ extractTarEntry' . archiveEntries

-- | Prints a warning on error, doesn't fail.
extractTarEntry' :: TarEntry -> IO ()
extractTarEntry' e = catchJustIOError isIllegalOperationErrorType
                       (extractTarEntry e)
                       (\err -> warn $ show err)

-- | Fails if any problems are encountered.
extractTarEntry :: TarEntry -> IO ()
extractTarEntry (TarEntry hdr cnt) = 
    do -- FIXME: more path checks?
       path <- forceRelativePath $ tarFileName hdr
       let dir = dirName path
       when (not (null dir)) $ createDirectoryIfMissing True dir
       let unsupported t = illegalOperation (t ++ " not supported") (Just path)
       case tarFileType hdr of
               TarHardLink   -> unsupported "hardlink" 
               TarSymLink    -> unsupported "symlink" 
               TarCharDev    -> unsupported "char dev"
               TarBlockDev   -> unsupported "block dev"
               TarDir        -> createDirectoryIfMissing True path >> return True
               TarFIFO       -> unsupported "FIFO"
               _             -> BS.writeFile path cnt >> return True
       -- FIXME: set owner
       -- FIXME: set group
       -- FIXME: set modification time
       setPermissions path (modeToPerms (tarFileType hdr == TarDir) (tarFileMode hdr))

modeToPerms :: Bool -> CMode -> Permissions
modeToPerms is_dir mode = 
    Permissions {
                 readable   = r,
                 writable   = w,
                 executable = not is_dir && x,
                 searchable = is_dir && x
                }
  where r = mode `testBit` 8
        w = mode `testBit` 7
        x = mode `testBit` 6
