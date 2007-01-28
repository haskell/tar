-- | Implements the USTAR (POSIX.1-1988) format (tar with extended header information).
module Codec.Archive.Tar (
                          -- * TAR archive types
                          TarArchive(..),
                          TarEntry(..),
                          TarHeader(..),
                          TarFileType(..),
                          -- * Creating and writing TAR archives
                          createTarFile,
                          createTarData,
                          createTarArchive,
                          writeTarArchive,
                          writeTarFile,
                          createTarEntry,
                          -- * Reading and extracting TAR archives
                          extractTarFile,
                          extractTarData,
                          extractTarArchive,
                          readTarArchive,
                          readTarFile,
                          extractTarEntry,
                          -- * Modifying TarArchives
                          filterTarArchive,
                          keepFiles,
                          -- * File utilities
                          recurseDirectories
                         ) where

import Data.Binary.Get
import Data.Binary.Put

import Control.Monad.Error
import Data.Bits
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.ByteString.Lazy (ByteString)
import Data.Char
import Data.Int
import Data.List
import qualified Data.Set as Set
import Numeric
import System.Directory
import System.IO
import System.IO.Error
import System.IO.Unsafe (unsafeInterleaveIO)
import System.Posix.Types
import System.Time

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
     tarModTime :: ClockTime,
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

-- * Building tar archives

-- | Convenience function:
-- @createTarFile f fs = createTarData fs >>= Data.ByteString.Lazy.writeFile f@
createTarFile :: FilePath -> [FilePath] -> IO ()
createTarFile f fs = createTarData fs >>= BS.writeFile f

-- | Convenience function:
-- @createTarData = liftM writeTarArchive . createTarArchive@
createTarData :: [FilePath] -> IO ByteString
createTarData = liftM writeTarArchive . createTarArchive 

-- | Create a TAR archive containing a number of files
-- and directories. In the list of paths, any directory 
-- should come before any files in that directory.
-- Only includes files and directories mentioned in the list,
-- does not recurse through directories.
createTarArchive :: [FilePath] -> IO TarArchive
createTarArchive = liftM TarArchive . mapM createTarEntry

-- FIXME: Warning if filepath is longer than 255 chars?
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
                            tarModTime = time,
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

-- * Path and file stuff

-- FIXME: normalize paths?
-- FIXME: fail if path is empty
sanitizePath :: TarFileType -> FilePath -> IO FilePath
sanitizePath t path = liftM (removeDuplSep . addTrailingSep) $ removeInit path
  where 
    removeInit p | null d = return p
                 | otherwise = 
                     do warn $ "removing initial " ++ d ++" from path " ++ p
                        return p'
        where p' = fixEq (removeDotDot . removeSep) p
              d = take (length p - length p') p
    removeDotDot ('.':'.':p) = p
    removeDotDot p = p
    removeSep (c:p) | c == pathSep = p
    removeSep p = p
    addTrailingSep = if t == TarDir then (++[pathSep]) else id
    removeDuplSep = 
        concat . map (\g -> if all (==pathSep) g then [pathSep] else g) . group

fixEq :: Eq a => (a -> a) -> a -> a
fixEq f x = let x' = f x in if x' == x then x else fixEq f x'

pathSep :: Char
pathSep = '/' -- FIXME: backslash on Windows

-- FIXME: not good enough. Use System.FilePath?
dirName :: FilePath -> FilePath
dirName p = if null d then "." else d
  where d = reverse $ dropWhile (/=pathSep) $ reverse p

getFileType :: FilePath -> IO TarFileType
getFileType path = 
    do f <- doesFileExist path
       if f then return TarNormalFile
            else do d <- doesDirectoryExist path
                    if d then return TarDir
                         else ioError $ doesNotExistError "htar" path

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


-- * Extracting tar archives

extractTarFile :: FilePath -> IO ()
extractTarFile f = BS.readFile f >>= extractTarData

extractTarData :: ByteString -> IO ()
extractTarData = extractTarArchive . readTarArchive

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

-- * Modifying TarArchives

filterTarArchive :: (TarHeader -> Bool) -> TarArchive -> TarArchive
filterTarArchive p = TarArchive . filter (p . entryHeader) . archiveEntries

-- FIXME: allow files names to differ in trailing slashes
keepFiles :: [FilePath] -> TarArchive -> TarArchive
keepFiles files = filterTarArchive ((`Set.member` Set.fromList files) . tarFileName)

-- * File permissions

-- | This is a bit brain-dead, since 'Permissions' doesn't
-- deal with user, group, others permissions.
permsToMode :: Permissions -> CMode
permsToMode perms = boolsToBits [r,w,x,r,False,x,r,False,x]
  where r = readable perms
        w = writable perms
        x = executable perms || searchable perms

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

-- * Reading and writing tar archives

writeTarArchive :: TarArchive -> ByteString
writeTarArchive = runPut . putTarArchive

writeTarFile :: FilePath -> TarArchive -> IO ()
writeTarFile f = BS.writeFile f . writeTarArchive

readTarArchive :: ByteString -> TarArchive
readTarArchive = runGet getTarArchive

readTarFile :: FilePath -> IO TarArchive
readTarFile = liftM readTarArchive . BS.readFile

putTarArchive :: TarArchive -> Put
putTarArchive (TarArchive es) = 
    do mapM_ putTarEntry es
       fill 512 '\0'
       fill 512 '\0'

getTarArchive :: Get TarArchive
getTarArchive = liftM TarArchive $ unfoldM getTarEntry

unfoldM :: Monad m => m (Maybe a) -> m [a]
unfoldM f = f >>= maybe (return []) (\x -> liftM (x:) (unfoldM f))

putTarEntry :: TarEntry -> Put
putTarEntry (TarEntry hdr cnt) = 
    do putTarHeader hdr
       putLazyByteString (rpadMod 512 '\0' cnt)
       flush

-- | Returns 'Nothing' if the entry is an end block.
getTarEntry :: Get (Maybe TarEntry)
getTarEntry =
    do mhdr <- getTarHeader
       case mhdr of
         Nothing -> return Nothing
         Just hdr -> liftM Just $ getBody hdr
  where getBody hdr = 
            do 
            let size = tarFileSize hdr
                padding = (512 - size) `mod` 512
            cnt <- liftM (BS.take size) $ getBytes $ size + padding
            return $ TarEntry hdr cnt

putTarHeader :: TarHeader -> Put
putTarHeader hdr = 
    do let x = runPut (putHeaderNoChkSum hdr)
           chkSum = sumBS x
       putLazyByteString $ setPart 148 (fmtOct 8 chkSum) x

putHeaderNoChkSum :: TarHeader -> Put
putHeaderNoChkSum hdr =
    do let (filePrefix, fileSuffix) = splitLongPath 100 (tarFileName hdr)
       putString  100 $ fileSuffix
       putOct       8 $ tarFileMode hdr
       putOct       8 $ tarOwnerID hdr
       putOct       8 $ tarGroupID hdr
       putOct      12 $ tarFileSize hdr
       putOct      12 $ let TOD s _ = tarModTime hdr in s
       fill         8 $ ' ' -- dummy checksum
       putTarFileType $ tarFileType hdr
       putString  100 $ tarLinkTarget hdr -- FIXME: take suffix split at / if too long
       putString    6 $ "ustar "
       putString    2 $ " " -- strange ustar version
       putString   32 $ tarOwnerName hdr
       putString   32 $ tarGroupName hdr
       putOct       8 $ tarDeviceMajor hdr
       putOct       8 $ tarDeviceMinor hdr
       putString  155 $ filePrefix
       fill        12 $ '\NUL'

getTarHeader :: Get (Maybe TarHeader)
getTarHeader =
    do block <- liftM BS.copy $ getLazyByteString 512
       return $ 
        if BS.head block == '\NUL'
          then Nothing
          else let chkSum' = sumBS $ setPart 148 (BS.replicate 8 ' ') block
                   (hdr,chkSum) = runGet getHeaderAndChkSum block -- FIXME: this gets the byte number wrong in error messages
                in if chkSum == chkSum'
                     then Just hdr
                     else error $ "TAR header checksum failure: " 
                                   ++ show chkSum ++ " /= " ++ show chkSum'

getHeaderAndChkSum :: Get (TarHeader, Int)
getHeaderAndChkSum =
    do fileSuffix <- getString  100
       mode       <- getOct       8
       uid        <- getOct       8
       gid        <- getOct       8
       size       <- getOct      12
       time       <- getOct      12
       chkSum     <- getOct       8
       typ        <- getTarFileType
       target     <- getString  100
       _ustar     <- skip         6
       _version   <- skip         2
       uname      <- getString   32
       gname      <- getString   32
       major      <- getOct       8
       minor      <- getOct       8
       filePrefix <- getString  155
       _          <- skip        12      
       let hdr = TarHeader {
                            tarFileName    = filePrefix ++ fileSuffix,
                            tarFileMode    = mode,
                            tarOwnerID     = uid,
                            tarGroupID     = gid,
                            tarFileSize    = size,
                            tarModTime     = TOD time 0,
                            tarFileType    = typ,
                            tarLinkTarget  = target,
                            tarOwnerName   = uname,
                            tarGroupName   = gname,
                            tarDeviceMajor = major,
                            tarDeviceMinor = minor
                           }
       return (hdr,chkSum)

putTarFileType :: TarFileType -> Put
putTarFileType t = 
    putChar8 $ case t of
                 TarNormalFile -> '0'
                 TarHardLink   -> '1'
                 TarSymLink    -> '2'
                 TarCharDev    -> '3'
                 TarBlockDev   -> '4'
                 TarDir        -> '5'
                 TarFIFO       -> '6'
                 TarContiguous -> '7'
                 TarCustom c   -> c

getTarFileType :: Get TarFileType
getTarFileType = 
    do c <- getChar8
       return $ case c of
                  '\0'-> TarNormalFile
                  '0' -> TarNormalFile
                  '1' -> TarHardLink
                  '2' -> TarSymLink
                  '3' -> TarCharDev
                  '4' -> TarBlockDev
                  '5' -> TarDir
                  '6' -> TarFIFO
                  '7' -> TarContiguous
                  _   -> TarCustom c

splitLongPath :: Int -> FilePath -> (String,String)
splitLongPath l path | l < 1 || null path = error $ unwords ["splitFileName", show l, show path]
splitLongPath l path | n > l = error $ "File path too long: " ++ show path -- FIXME: implement real splitting
                     | otherwise = ("",path)
  where n = length path

sumBS :: ByteString -> Int
sumBS = BS.foldl' (\x y -> x + ord y) 0

-- * TAR format primitive output

putString :: Int64 -> String -> Put
putString n = putLazyByteString . rpad n '\0' . ltrunc n . BS.pack

putOct :: Integral a => Int64 -> a -> Put
putOct n = putLazyByteString . fmtOct n

fmtOct :: Integral a => Int64 -> a -> ByteString
fmtOct n x = (lpad l '0' $ ltrunc l $ BS.pack $ showOct x "") 
             `BS.append` BS.singleton '\NUL'
    where l = n-1

putChar8 :: Char -> Put
putChar8 = putWord8 . fromIntegral . ord

-- * TAR format primitive input

getOct :: Integral a => Int -> Get a
getOct n = getLazyByteString n >>= parseOct . takeWhile (/='\0') . BS.unpack
  where parseOct "" = return 0
        parseOct s = case readOct s of
                       [(x,_)] -> return x
                       _       -> fail $ "Number format error: " ++ show s

getString :: Int -> Get String
getString = liftM (takeWhile (/='\NUL') . BS.unpack) . getLazyByteString

getChar8 :: Get Char
getChar8 = fmap (chr . fromIntegral) getWord8

-- * Utilities

warn :: String -> IO ()
warn = hPutStrLn stderr . ("htar: "++)

doesNotExistError :: String -> FilePath -> IOError
doesNotExistError loc = mkIOError doesNotExistErrorType loc Nothing . Just

boolsToBits :: Bits a => [Bool] -> a
boolsToBits = f 0
  where f x [] = x
        f x (b:bs) = f (x `shiftL` 1 .|. if b then 1 else 0) bs


lpad :: Int64 -> Char -> ByteString -> ByteString
lpad n b xs = BS.replicate (n - BS.length xs) b `BS.append` xs

rpad :: Int64 -> Char -> ByteString -> ByteString
rpad n b xs = xs `BS.append` BS.replicate (n - BS.length xs) b

-- | Right-pad up to the nearest multiple of the given length.
rpadMod :: Int64 -> Char -> ByteString -> ByteString
rpadMod n b xs = xs `BS.append` BS.replicate ((n - BS.length xs) `mod` n) b

ltrunc :: Int64 -> ByteString -> ByteString
ltrunc n xs = BS.drop (BS.length xs - n) xs

setPart :: Int64 -> ByteString -> ByteString -> ByteString
setPart off new old = 
    let (before,rest) = BS.splitAt off old
        after = BS.drop (BS.length new) rest
     in before `BS.append` (BS.take (BS.length old - off) new) `BS.append` after

fill :: Int -> Char -> Put
fill n = putLazyByteString . BS.replicate (fromIntegral n)
