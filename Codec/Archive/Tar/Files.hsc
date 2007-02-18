-- | This module makes the operations exported by "System.Posix.Files" 
-- available on all platforms. On POSIX systems it re-exports operations 
-- from "System.Posix.Files". On other platforms it emulates this
-- operations as far as possible.
--
-- NOTE: the portable implementations are not well tested.
module Codec.Archive.Tar.Files (
    -- * File modes
    -- FileMode exported by System.Posix.Types
    unionFileModes, intersectFileModes,
    nullFileMode,
    ownerReadMode, ownerWriteMode, ownerExecuteMode, ownerModes,
    groupReadMode, groupWriteMode, groupExecuteMode, groupModes,
    otherReadMode, otherWriteMode, otherExecuteMode, otherModes,
    setUserIDMode, setGroupIDMode,
    stdFileMode,   accessModes,

    -- ** Setting file modes
    setFileMode, setFdMode, setFileCreationMask,

    -- ** Checking file existence and permissions
    fileAccess, fileExist,

    -- * File status
    FileStatus,
    -- ** Obtaining file status
    getFileStatus, getFdStatus, getSymbolicLinkStatus,
    -- ** Querying file status
    deviceID, fileID, fileMode, linkCount, fileOwner, fileGroup,
    specialDeviceID, fileSize, accessTime, modificationTime,
    statusChangeTime,
    isBlockDevice, isCharacterDevice, isNamedPipe, isRegularFile,
    isDirectory, isSymbolicLink, isSocket,

    -- * Creation
    createNamedPipe, 
    createDevice,

    -- * Hard links
    createLink, removeLink,

    -- * Symbolic links
    createSymbolicLink, readSymbolicLink,

    -- * Renaming files
    rename,

    -- * Changing file ownership
    setOwnerAndGroup,  setFdOwnerAndGroup,
    setSymbolicLinkOwnerAndGroup,

    -- * Changing file timestamps
    setFileTimes, touchFile,

    -- * Setting file sizes
    setFileSize, setFdSize,

    -- * Find system-specific limits for a file
    PathVar(..), getPathVar, getFdPathVar,
 ) where

#define UNIX !defined(mingw32_HOST_OS)

#define HAVE_FD_TO_HANDLE __GLASGOW_HASKELL__

#if UNIX

import System.Posix.Files

#if !HAVE_LCHOWN
import System.Posix.Types
#endif

#include "HsUnix.h"

#else /* Portable implementation */

import Control.Exception (bracket)
import Control.Monad (liftM, liftM2)
import Data.Bits ((.|.), (.&.))
import System.Directory (renameFile, doesFileExist, doesDirectoryExist, 
                         Permissions(..), getPermissions, setPermissions,
                         getModificationTime)
import System.IO (IOMode(..), openFile, hFileSize, hSetFileSize, hClose)
import System.IO.Error
import System.Posix.Types
import System.Time (ClockTime(..), getClockTime)

#if __GLASGOW_HASKELL__
import GHC.Handle (fdToHandle)
#endif


#endif



#if UNIX

#if !HAVE_LCHOWN
setSymbolicLinkOwnerAndGroup :: FilePath -> UserID -> GroupID -> IO ()
setSymbolicLinkOwnerAndGroup name uid gid = return ()
#endif

#else /* Portable implementations */

unsupported :: String -> IO a
unsupported loc = ioError $ mkIOError illegalOperationErrorType loc Nothing Nothing


-- -----------------------------------------------------------------------------
-- POSIX file modes

nullFileMode     :: FileMode
nullFileMode     = 0o000000

ownerReadMode    :: FileMode
ownerWriteMode   :: FileMode
ownerExecuteMode :: FileMode
groupReadMode    :: FileMode
groupWriteMode   :: FileMode
groupExecuteMode :: FileMode
otherReadMode    :: FileMode
otherWriteMode   :: FileMode
otherExecuteMode :: FileMode
setUserIDMode    :: FileMode
setGroupIDMode   :: FileMode

ownerReadMode    = 0o000400
ownerWriteMode   = 0o000200
ownerExecuteMode = 0o000100
groupReadMode    = 0o000040
groupWriteMode   = 0o000020
groupExecuteMode = 0o000010
otherReadMode    = 0o000004
otherWriteMode   = 0o000002
otherExecuteMode = 0o000001
setUserIDMode    = 0o004000
setGroupIDMode   = 0o002000

stdFileMode      :: FileMode
ownerModes       :: FileMode
groupModes       :: FileMode
otherModes       :: FileMode
accessModes      :: FileMode

stdFileMode = ownerReadMode  .|. ownerWriteMode .|. 
	      groupReadMode  .|. groupWriteMode .|. 
	      otherReadMode  .|. otherWriteMode
ownerModes  = ownerReadMode  .|. ownerWriteMode .|. ownerExecuteMode
groupModes  = groupReadMode  .|. groupWriteMode .|. groupExecuteMode
otherModes  = otherReadMode  .|. otherWriteMode .|. otherExecuteMode
accessModes = ownerModes .|. groupModes .|. otherModes

unionFileModes :: FileMode -> FileMode -> FileMode
unionFileModes m1 m2 = m1 .|. m2

intersectFileModes :: FileMode -> FileMode -> FileMode
intersectFileModes m1 m2 = m1 .&. m2

fileTypeModes :: FileMode
fileTypeModes = 0o0170000

blockSpecialMode     :: FileMode
characterSpecialMode :: FileMode
namedPipeMode        :: FileMode
regularFileMode      :: FileMode
directoryMode        :: FileMode
symbolicLinkMode     :: FileMode
socketMode           :: FileMode

blockSpecialMode     = 0o0060000
characterSpecialMode = 0o0020000
namedPipeMode        = 0o0010000
regularFileMode      = 0o0100000
directoryMode        = 0o0040000
symbolicLinkMode     = 0o0120000
socketMode           = 0o0140000


setFileMode :: FilePath -> FileMode -> IO ()
setFileMode name m = setPermissions name $ modeToPerms m


setFdMode :: Fd -> FileMode -> IO ()
setFdMode fd m = unsupported "setFdMode"

-- | The portable implementation does nothing and returns 'nullFileMode'.
setFileCreationMask :: FileMode -> IO FileMode
setFileCreationMask mask = return nullFileMode

modeToPerms :: FileMode -> Permissions
modeToPerms m = Permissions {
                             readable   = m .&. ownerReadMode    /= 0,
                             writable   = m .&. ownerWriteMode   /= 0,
                             executable = m .&. ownerExecuteMode /= 0,
                             searchable = m .&. ownerExecuteMode /= 0
                            }

-- -----------------------------------------------------------------------------
-- access()

fileAccess :: FilePath -> Bool -> Bool -> Bool -> IO Bool
fileAccess name read write exec = 
    do perm <- getPermissions name
       return $ (not read  || readable perm) 
             && (not write || writable perm) 
             && (not exec  || executable perm || searchable perm)

fileExist :: FilePath -> IO Bool
fileExist name = liftM2 (||) (doesFileExist name) (doesDirectoryExist name)

-- -----------------------------------------------------------------------------
-- stat() support

data FileStatus = FileStatus {
                              deviceID         :: DeviceID,
                              fileID           :: FileID,
                              fileMode         :: FileMode,
                              linkCount        :: LinkCount,
                              fileOwner        :: UserID,
                              fileGroup        :: GroupID,
                              specialDeviceID  :: DeviceID,
                              fileSize         :: FileOffset,
                              accessTime       :: EpochTime,
                              modificationTime :: EpochTime,
                              statusChangeTime :: EpochTime
                             }

isBlockDevice :: FileStatus -> Bool
isBlockDevice stat = 
  (fileMode stat `intersectFileModes` fileTypeModes) == blockSpecialMode

isCharacterDevice :: FileStatus -> Bool
isCharacterDevice stat = 
  (fileMode stat `intersectFileModes` fileTypeModes) == characterSpecialMode

isNamedPipe :: FileStatus -> Bool
isNamedPipe stat = 
  (fileMode stat `intersectFileModes` fileTypeModes) == namedPipeMode

isRegularFile :: FileStatus -> Bool
isRegularFile stat = 
  (fileMode stat `intersectFileModes` fileTypeModes) == regularFileMode

isDirectory :: FileStatus -> Bool
isDirectory stat = 
  (fileMode stat `intersectFileModes` fileTypeModes) == directoryMode

isSymbolicLink :: FileStatus -> Bool
isSymbolicLink stat = 
  (fileMode stat `intersectFileModes` fileTypeModes) == symbolicLinkMode

isSocket :: FileStatus -> Bool
isSocket stat = 
  (fileMode stat `intersectFileModes` fileTypeModes) == socketMode

getFileStatus :: FilePath -> IO FileStatus
getFileStatus path = 
    do perm  <- liftM permsToMode $ getPermissions path
       typ   <- getFileType path
       size  <- if typ == regularFileMode then getFileSize path else return 0
       mtime <- liftM clockTimeToEpochTime $ getModificationTime path
       return $ FileStatus {
                            deviceID           = -1,
                            fileID             = -1,
                            fileMode           = typ .|. perm,
                            linkCount          = 1,
                            fileOwner          = 0,
                            fileGroup          = 0,
                            specialDeviceID    = 0,
                            fileSize           = size,
                            accessTime         = mtime,
                            modificationTime   = mtime,
                            statusChangeTime   = mtime
                           }

permsToMode :: Permissions -> FileMode
permsToMode perms = r .|. w .|. x
  where r = f (readable perms) (ownerReadMode .|. groupReadMode .|. otherReadMode)
        w = f (writable perms) (ownerWriteMode .|. groupWriteMode .|. otherWriteMode)
        x = f (executable perms || searchable perms) 
                     (ownerExecuteMode .|. groupExecuteMode .|. otherExecuteMode)
        f True m  = m
        f False _ = nullFileMode

getFileType :: FilePath -> IO FileMode
getFileType path = 
    do f <- doesFileExist path
       if f then return regularFileMode
            else do d <- doesDirectoryExist path
                    if d then return directoryMode
                         else unsupported "Unknown file type."

getFileSize :: FilePath -> IO FileOffset
getFileSize path = 
    bracket (openFile path ReadMode) hClose (liftM fromIntegral . hFileSize)

clockTimeToEpochTime :: ClockTime -> EpochTime
clockTimeToEpochTime (TOD s _) = fromInteger s


getFdStatus :: Fd -> IO FileStatus
getFdStatus fd = unsupported "getFdStatus"

getSymbolicLinkStatus :: FilePath -> IO FileStatus
getSymbolicLinkStatus path = unsupported "getSymbolicLinkStatus"

createNamedPipe :: FilePath -> FileMode -> IO ()
createNamedPipe name mode = unsupported "createNamedPipe"

createDevice :: FilePath -> FileMode -> DeviceID -> IO ()
createDevice path mode dev = unsupported "createDevice"

-- -----------------------------------------------------------------------------
-- Hard links

createLink :: FilePath -> FilePath -> IO ()
createLink name1 name2 = unsupported "createLink"

removeLink :: FilePath -> IO ()
removeLink name = unsupported "removeLink"

-- -----------------------------------------------------------------------------
-- Symbolic Links

createSymbolicLink :: FilePath -> FilePath -> IO ()
createSymbolicLink file1 file2 = unsupported "createSymbolicLink"

readSymbolicLink :: FilePath -> IO FilePath
readSymbolicLink file = unsupported "readSymbolicLink"

-- -----------------------------------------------------------------------------
-- Renaming files

rename :: FilePath -> FilePath -> IO ()
rename name1 name2 = renameFile name1 name2

-- -----------------------------------------------------------------------------
-- chown()

setOwnerAndGroup :: FilePath -> UserID -> GroupID -> IO ()
setOwnerAndGroup name uid gid = unsupported "setOwnerAndGroup"

setFdOwnerAndGroup :: Fd -> UserID -> GroupID -> IO ()
setFdOwnerAndGroup fd uid gid = unsupported "setFdOwnerAndGroup"

setSymbolicLinkOwnerAndGroup :: FilePath -> UserID -> GroupID -> IO ()
setSymbolicLinkOwnerAndGroup name uid gid = 
    unsupported "setSymbolicLinkOwnerAndGroup"

-- -----------------------------------------------------------------------------
-- utime()

setFileTimes :: FilePath -> EpochTime -> EpochTime -> IO ()
setFileTimes name atime mtime = unsupported "setFileTimes"

touchFile :: FilePath -> IO ()
touchFile name = 
    do t <- liftM clockTimeToEpochTime getClockTime 
       setFileTimes name t t

-- -----------------------------------------------------------------------------
-- Setting file sizes

setFileSize :: FilePath -> FileOffset -> IO ()
setFileSize file off = 
    bracket (openFile file WriteMode) (hClose)
            (\h -> hSetFileSize h (fromIntegral off))

setFdSize :: Fd -> FileOffset -> IO ()
#if HAVE_FD_TO_HANDLE
setFdSize (Fd fd) off = 
    do h <- fdToHandle (fromIntegral fd)
       hSetFileSize h (fromIntegral off)
#else
setFdSize fd off = unsupported "setFdSize"
#endif

-- -----------------------------------------------------------------------------
-- pathconf()/fpathconf() support

data PathVar
  = FileSizeBits		  {- _PC_FILESIZEBITS     -}
  | LinkLimit                     {- _PC_LINK_MAX         -}
  | InputLineLimit                {- _PC_MAX_CANON        -}
  | InputQueueLimit               {- _PC_MAX_INPUT        -}
  | FileNameLimit                 {- _PC_NAME_MAX         -}
  | PathNameLimit                 {- _PC_PATH_MAX         -}
  | PipeBufferLimit               {- _PC_PIPE_BUF         -}
				  -- These are described as optional in POSIX:
  				  {- _PC_ALLOC_SIZE_MIN     -}
  				  {- _PC_REC_INCR_XFER_SIZE -}
  				  {- _PC_REC_MAX_XFER_SIZE  -}
  				  {- _PC_REC_MIN_XFER_SIZE  -}
 				  {- _PC_REC_XFER_ALIGN     -}
  | SymbolicLinkLimit		  {- _PC_SYMLINK_MAX      -}
  | SetOwnerAndGroupIsRestricted  {- _PC_CHOWN_RESTRICTED -}
  | FileNamesAreNotTruncated      {- _PC_NO_TRUNC         -}
  | VDisableChar		  {- _PC_VDISABLE         -}
  | AsyncIOAvailable		  {- _PC_ASYNC_IO         -}
  | PrioIOAvailable		  {- _PC_PRIO_IO          -}
  | SyncIOAvailable		  {- _PC_SYNC_IO          -}

getPathVar :: FilePath -> PathVar -> IO Limit
getPathVar name v = unsupported "getPathVar"

getFdPathVar :: Fd -> PathVar -> IO Limit
getFdPathVar fd v = unsupported "getFdPathVar"

#endif

