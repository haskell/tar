module Codec.Archive.Tar.Create (
                                 -- * Creating TAR archives from files
                                 createTarArchive, createTarEntry,
                                 recurseDirectories,
                                 -- * Creating TAR archives from scratch
                                 mkTarHeader) where

import Codec.Archive.Tar.Types
import Codec.Archive.Tar.Util

import System.PosixCompat.Extensions
import System.PosixCompat.Files

import Control.Monad
import qualified Data.ByteString.Lazy as L
import Data.List
import System.Directory
import System.IO
import System.IO.Unsafe (unsafeInterleaveIO)


-- | Creates a TAR archive containing a number of files
-- and directories taken from the file system. In the list 
-- of paths, any directory 
-- should come before any files in that directory.
-- Only files and directories mentioned in the list are included,
-- this function does not recurse into the directories.
createTarArchive :: [FilePath] -- ^ Files and directories to include in the archive.
                    -> IO TarArchive
createTarArchive = liftM TarArchive . mapM createTarEntry

-- | Creates a TAR archive entry for a file or directory.
-- The meta-data and file contents are taken from the given file.
createTarEntry :: FilePath -> IO TarEntry
createTarEntry path = 
    do stat <- getSymbolicLinkStatus path
       let t = fileType stat
       path' <- sanitizePath t path
       target <- case t of
                   TarSymbolicLink -> readSymbolicLink path
                   _               -> return ""
       let (major,minor) = if t == TarCharacterDevice || t == TarBlockDevice
                             then let dev = deviceID stat
                                   in (deviceMajor dev, deviceMinor dev)
                             else (0,0)
       -- FIXME: don't work on OS X
       -- FIXME: if it fails, return nil
       owner <- return "" --liftM userName  $ getUserEntryForID  $ fileOwner stat
       grp   <- return "" --liftM groupName $ getGroupEntryForID $ fileGroup stat
       let hdr = TarHeader {
                            tarFileName    = path',
                            tarFileMode    = fileMode stat,
                            tarOwnerID     = fileOwner stat,
                            tarGroupID     = fileGroup stat,
                            tarFileSize    = fromIntegral $ fileSize stat,
                            tarModTime     = modificationTime stat,
                            tarFileType    = t,
                            tarLinkTarget  = target,
                            tarOwnerName   = owner,
                            tarGroupName   = grp,
                            tarDeviceMajor = major,
                            tarDeviceMinor = minor
                           }
       cnt <- case t of
                TarNormalFile -> L.readFile path -- FIXME: warn if size has changed?
                _             -> return L.empty
       return $ TarEntry hdr cnt

fileType :: FileStatus -> TarFileType
fileType stat | isRegularFile     stat = TarNormalFile
              | isSymbolicLink    stat = TarSymbolicLink
              | isCharacterDevice stat = TarCharacterDevice
              | isBlockDevice     stat = TarBlockDevice
              | isDirectory       stat = TarDirectory
              | isNamedPipe       stat = TarFIFO
              | otherwise              = error "Unknown file type."

-- | Creates a TAR header for a normal file with the given path.
-- Does not consult the file system.
-- All meta-data is set to default values.
mkTarHeader :: FilePath -> TarHeader
mkTarHeader path = TarHeader {
                              tarFileName    = path,
                              tarFileMode    = stdFileMode,
                              tarOwnerID     = 0,
                              tarGroupID     = 0,
                              tarFileSize    = 0,
                              tarModTime     = 0,
                              tarFileType    = TarNormalFile,
                              tarLinkTarget  = "",
                              tarOwnerName   = "",
                              tarGroupName   = "",
                              tarDeviceMajor = 0,
                              tarDeviceMinor = 0
                             }

-- * Path and file stuff

-- FIXME: normalize paths?
sanitizePath :: TarFileType -> FilePath -> IO FilePath
sanitizePath t path = 
    do path' <- liftM (removeDuplSep . addTrailingSep) $ forceRelativePath path       
       when (null path' || length path' > 255) $
            fail $ "Path too long: " ++ show path' -- FIXME: warn instead?
       return path'
  where 
    addTrailingSep = if t == TarDirectory then (++[pathSep]) else id
    removeDuplSep = 
        concat . map (\g -> if all (==pathSep) g then [pathSep] else g) . group

-- | Recurses through a list of files and directories
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
