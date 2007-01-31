module Codec.Archive.Tar.Create where

import Codec.Archive.Tar.Types
import Codec.Archive.Tar.Util

import Control.Monad
import qualified Data.ByteString.Lazy as BS
import Data.List
import System.Directory
import System.IO
import System.IO.Unsafe (unsafeInterleaveIO)
import System.Posix.Types


-- | Create a TAR archive containing a number of files
-- and directories. In the list of paths, any directory 
-- should come before any files in that directory.
-- Only includes files and directories mentioned in the list,
-- does not recurse through directories.
createTarArchive :: [FilePath] -> IO TarArchive
createTarArchive = liftM TarArchive . mapM createTarEntry

createTarEntry :: FilePath -> IO TarEntry
createTarEntry path = 
    do t <- getFileType path
       path' <- sanitizePath t path
       perms <- getPermissions path
       time <- getModificationTime path
       let hdr = TarHeader {
                            tarFileName = path',
                            tarFileMode = permsToMode perms,
                            tarOwnerID = 0,
                            tarGroupID = 0,
                            tarFileSize = 0, -- set below
                            tarModTime = clockTimeToEpochTime time,
                            tarFileType = t,
                            tarLinkTarget = "",
                            tarOwnerName = "",
                            tarGroupName = "",
                            tarDeviceMajor = 0,
                            tarDeviceMinor = 0
                           }
       case t of
         TarNormalFile -> do h <- openBinaryFile path ReadMode
                             size <- liftM fromIntegral $ hFileSize h
                             cnt <- BS.hGetContents h -- FIXME: warn if size has changed?
                             return $ TarEntry (hdr { tarFileSize = size }) cnt
         _             -> return $ TarEntry hdr BS.empty

-- * File permissions

-- | This is a bit brain-dead, since 'Permissions' doesn't
-- deal with user, group, others permissions.
permsToMode :: Permissions -> CMode
permsToMode perms = boolsToBits [r,w,x,r,False,x,r,False,x]
  where r = readable perms
        w = writable perms
        x = executable perms || searchable perms

-- * Path and file stuff

-- FIXME: normalize paths?
sanitizePath :: TarFileType -> FilePath -> IO FilePath
sanitizePath t path = 
    do path' <- liftM (removeDuplSep . addTrailingSep) $ forceRelativePath path       
       when (null path' || length path' > 255) $
            fail $ "Path too long: " ++ show path' -- FIXME: warn instead?
       return path'
  where 
    addTrailingSep = if t == TarDir then (++[pathSep]) else id
    removeDuplSep = 
        concat . map (\g -> if all (==pathSep) g then [pathSep] else g) . group

getFileType :: FilePath -> IO TarFileType
getFileType path = 
    do f <- doesFileExist path
       if f then return TarNormalFile
            else do d <- doesDirectoryExist path
                    if d then return TarDir
                         else doesNotExist "tar" path

-- | Recurse through a list of files and directories
-- in depth-first order.
-- Each of the given paths are returned, and each path which 
-- refers to a directory is followed by its descendants.
-- The output is suitable for feeding to the
-- TAR archive creation functions.
recurseDirectories :: [FilePath] -> IO [FilePath]
recurseDirectories = 
    liftM concat . mapM (\p -> liftM (p:) $ unsafeInterleaveIO $ descendants p)
  where 
    descendants path =
        do d <- doesDirectoryExist path
           if d then do cs <- getDirectoryContents path
                        let cs' = [path++[pathSep]++c | c <- cs, includeDir c]
                        ds <- recurseDirectories cs'
                        return ds
                else return []
     where includeDir "." = False
           includeDir ".." = False
           includeDir _ = True
