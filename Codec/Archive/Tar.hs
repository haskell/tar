-- | Implements the USTAR (POSIX.1-1988) format (tar with extended header information).
module Codec.Archive.Tar where

import Data.Binary
import Data.Binary.Get (runGet, getLazyByteString, skip, lookAhead)
import Data.Binary.Put (runPut)

import Control.Monad
import Control.Monad.Error
import Data.Bits
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.ByteString.Lazy (ByteString)
import Data.Char
import Data.Int
import Data.List
import Data.Word
import Numeric
import System.Directory
import System.IO
import System.IO.Error
import System.Time

newtype TarArchive = TarArchive { archiveEntries :: [TarEntry] }
  deriving Show

data TarEntry = TarEntry { entryHeader :: TarHeader,
                           entryData :: ByteString }
  deriving Show

data TarHeader = TarHeader 
    {
     tarFileName :: FilePath,
     tarFileMode :: Int,
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

-- FIXME: Warning if filepath is longer than 255 chars?
fileToTarEntry :: FilePath -> IO TarEntry
fileToTarEntry path = 
    do t <- getFileType path
       let path' = path ++ if t == TarDir && not ([pathSep] `isSuffixOf` path) 
                              then [pathSep] else ""
       perms <- getPermissions path
       time <- getModificationTime path
       let hdr = TarHeader {
                            tarFileName = path',
                            tarFileMode = permsToMode (t == TarDir) perms,
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

pathSep :: Char
pathSep = '/' -- FIXME: backslash on Windows

getFileType :: FilePath -> IO TarFileType
getFileType path = 
    do f <- doesFileExist path
       if f then return TarNormalFile
            else do d <- doesDirectoryExist path
                    if d then return TarDir
                         else ioError $ doesNotExistError "htar" path


-- | This is a bit brain-dead, since 'Permissions' doesn't
-- deal with user, group, others permissions.
permsToMode :: Bool -> Permissions -> Int
permsToMode isDir perms = boolsToBits [r,w,x,r,w,x,r,w,x]
  where r = readable perms
        w = writable perms
        x = executable perms || searchable perms

-- * Formatted information about archives

archiveHeaders :: TarArchive -> [TarHeader]
archiveHeaders = map entryHeader . archiveEntries

archiveFileNames :: TarArchive -> String
archiveFileNames = unlines . map tarFileName . archiveHeaders

archiveFileInfo :: TarArchive -> String
archiveFileInfo = unlines . map fileInfo . archiveHeaders
  where fileInfo hdr = unwords [typ ++ mode, owner, group, size, time, name] -- FIXME: nice padding
          where typ = case tarFileType hdr of
                        TarSymLink  -> "l"
                        TarCharDev  -> "c"
                        TarBlockDev -> "b"
                        TarDir      -> "d"
                        TarFIFO     -> "p"
                        _           -> "_"
                mode = concat [u,g,o] -- FIXME: handle setuid etc.
                  where m = tarFileMode hdr 
                        f x = [t 2 'r', t 1 'w', t 0 'x']
                          where t n c = if testBit x n then c else '-'
                        u = f (m `shiftR` 6)
                        g = f (m `shiftR` 3)
                        o = f m
                owner = let name = tarOwnerName hdr in if null name then show (tarOwnerID hdr) else name
                group = let name = tarGroupName hdr in if null name then show (tarGroupID hdr) else name
                size = show (tarFileSize hdr)
                time = show (tarModTime hdr)
                name = tarFileName hdr

-- * Serializing and deserializing tar archives

instance Binary TarArchive where

    put (TarArchive es) = do mapM_ put es
                             put nulBlock
                             put nulBlock
        where nulBlock = BS.replicate 512 '\0'

    get = do block <- lookAhead 512
             if BS.head block == '\NUL'
                then return $ TarArchive [] -- FIXME: should we check the next block too?
                else do me <- tryError get
--                        fail $ "read first entry"
                        TarArchive es <- get
                        -- FIXME: output error message if the entry failed
                        return $ TarArchive $ either (\_ -> es) (:es) me

instance Binary TarEntry where

    put (TarEntry hdr cnt) = do put hdr
                                put (rpadMod 512 '\0' cnt)
    get = do hdr <- get
             cnt <- getLazyByteString (fromIntegral $ tarFileSize hdr) -- FIXME: this only allows files < 2GB. getLazyByteString should be changed.
             skip $ fromIntegral ((512 - tarFileSize hdr) `mod` 512)
             return $ TarEntry hdr cnt

instance Binary TarHeader where

    put hdr = do let x = runPut putHeader
                     chkSum = sumBS x
                 put $ setPart 148 (fmtOct 8 chkSum) x
      where putHeader = 
              do let (filePrefix, fileSuffix) = splitFileName 100 (tarFileName hdr)
                 putString 100 $ fileSuffix
                 putOct      8 $ tarFileMode hdr
                 putOct      8 $ tarOwnerID hdr
                 putOct      8 $ tarGroupID hdr
                 putOct     12 $ tarFileSize hdr
                 putOct     12 $ let TOD s _ = tarModTime hdr in s
                 put           $ dummyChkSum
                 put           $ tarFileType hdr
                 putString 100 $ tarLinkTarget hdr -- FIXME: take suffix split at / if too long
                 putString   6 $ "ustar "
                 putString   2 $ " " -- strange ustar version
                 putString  32 $ tarOwnerName hdr
                 putString  32 $ tarGroupName hdr
                 putOct      8 $ tarDeviceMajor hdr
                 putOct      8 $ tarDeviceMinor hdr
                 putString 155 $ filePrefix
                 put           $ BS.replicate 12 '\NUL'

    get = do block <- lookAhead 512
             let chkSum' = sumBS $ setPart 148 dummyChkSum block
             (hdr,chkSum) <- getHeader
             if chkSum == chkSum'
                then return hdr
                else fail $ "TAR header checksum failure: " 
                              ++ show chkSum ++ " /= " ++ show chkSum' 
     where getHeader =            
             do fileSuffix <- getString  100
                mode       <- getOct       8
                uid        <- getOct       8
                gid        <- getOct       8
                size       <- getOct      12
                time       <- getOct      12
                chkSum     <- getOct       8
                typ        <- get
                target     <- getString  100
                ustar      <- skip         6
                version    <- skip         2
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

splitFileName :: Int -> FilePath -> (String,String)
splitFileName l path | l < 1 || null path = error $ unwords ["splitFileName", show l, show path]
splitFileName l path | n > l = error $ "File path too long: " ++ show path -- FIXME: implement real splitting
                     | otherwise = ("",path)
  where n = length path

sumBS :: ByteString -> Int
sumBS = BS.foldl' (\x y -> x + ord y) 0

dummyChkSum :: ByteString
dummyChkSum = BS.replicate 8 ' '

instance Binary TarFileType where
    put t = putChar8 $ case t of
                         TarNormalFile -> '0'
                         TarHardLink   -> '1'
                         TarSymLink    -> '2'
                         TarCharDev    -> '3'
                         TarBlockDev   -> '4'
                         TarDir        -> '5'
                         TarFIFO       -> '6'
                         TarContiguous -> '7'
                         TarCustom c   -> c
    get = do c <- getChar8
             return $ case c of
                        '0' -> TarNormalFile
                        '1' -> TarHardLink
                        '2' -> TarSymLink
                        '3' -> TarCharDev
                        '4' -> TarBlockDev
                        '5' -> TarDir
                        '6' -> TarFIFO
                        '7' -> TarContiguous
                        _   -> TarCustom c

-- * TAR format primitive output

putString :: Int64 -> String -> Put ()
putString n = put . rpad n '\0' . ltrunc n . BS.pack

putOct :: Integral a => Int64 -> a -> Put ()
putOct n = put . fmtOct n

fmtOct :: Integral a => Int64 -> a -> ByteString
fmtOct n x = (lpad l '0' $ ltrunc l $ BS.pack $ showOct x "") 
             `BS.append` BS.singleton '\NUL'
    where l = n-1

putChar8 :: Char -> Put ()
putChar8 c = put (fromIntegral (ord c) :: Word8)

-- * TAR format primitive input

getOct :: Integral a => Int -> Get a
getOct n = getLazyByteString n >>= parseOct . BS.unpack
  where parseOct s = case readOct s of
                       [(x,_)] -> return x
                       _       -> fail $ "Number format error: " ++ show s

getString :: Int -> Get String
getString = liftM (takeWhile (/='\NUL') . BS.unpack) . getLazyByteString

getChar8 :: Get Char
getChar8 = fmap (chr . fromIntegral) (get :: Get Word8)

-- * Utilities

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
rpadMod n b xs = xs `BS.append` BS.replicate (n - BS.length xs `mod` n) b

ltrunc :: Int64 -> ByteString -> ByteString
ltrunc n xs = BS.drop (BS.length xs - n) xs

rtrunc :: Int64 -> ByteString -> ByteString
rtrunc = BS.take

setPart :: Int64 -> ByteString -> ByteString -> ByteString
setPart off new old = 
    let (before,rest) = BS.splitAt off old
        after = BS.drop (BS.length new) rest
     in before `BS.append` (BS.take (BS.length old - off) new) `BS.append` after

runGetM :: Monad m => Get a -> ByteString -> m a
runGetM g = either (fail . show) return . runGet g

tryError :: MonadError e m => m a -> m (Either e a)
tryError m = liftM Right m `catchError` (return . Left)