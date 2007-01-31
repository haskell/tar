module Codec.Archive.Tar.Extract where

import Codec.Archive.Tar.Types
import Codec.Archive.Tar.Util

import Data.Bits
import qualified Data.ByteString.Lazy as BS
import System.Directory
import System.Posix.Types


extractTarArchive :: TarArchive -> IO ()
extractTarArchive = mapM_ extractTarEntry . archiveEntries

extractTarEntry :: TarEntry -> IO ()
extractTarEntry (TarEntry hdr cnt) = 
    do -- FIXME: make sure path is sane
       let path = tarFileName hdr
           typ = tarFileType hdr
           -- FIXME: set owner
           -- FIXME: set group
           -- FIXME: set modification time
           setMeta = setPermissions path (modeToPerms (typ == TarDir) (tarFileMode hdr))
       case typ of
         TarHardLink   -> warn $ "Can't create hardlink yet, skipping " ++ path 
         TarSymLink    -> warn $ "Can't create symlink yet, skipping " ++ path 
         TarCharDev    -> warn $ "Can't create char dev yet, skipping " ++ path
         TarBlockDev   -> warn $ "Can't create block dev yet, skipping " ++ path 
         TarDir        -> do createDirectoryIfMissing True path
                             setMeta
         TarFIFO       -> warn $ "Can't create FIFO yet, skipping " ++ path 
         _             -> do createDirectoryIfMissing True $ dirName path
                             BS.writeFile path cnt
                             setMeta
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
