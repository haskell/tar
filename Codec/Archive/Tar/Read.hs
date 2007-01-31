module Codec.Archive.Tar.Read where

import Codec.Archive.Tar.Types
import Codec.Archive.Tar.Util

import Data.Binary.Get

import Data.Char (chr,ord)
import Control.Monad (liftM)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.ByteString.Lazy (ByteString)
import Data.Int (Int8)
import Numeric (readOct)


readTarArchive :: ByteString -> TarArchive
readTarArchive = runGet getTarArchive

readTarFile :: FilePath -> IO TarArchive
readTarFile = liftM readTarArchive . BS.readFile

getTarArchive :: Get TarArchive
getTarArchive = liftM TarArchive $ unfoldM getTarEntry

unfoldM :: Monad m => m (Maybe a) -> m [a]
unfoldM f = f >>= maybe (return []) (\x -> liftM (x:) (unfoldM f))

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

getTarHeader :: Get (Maybe TarHeader)
getTarHeader =
    do block <- liftM BS.copy $ getLazyByteString 512
       return $ 
        if BS.head block == '\NUL'
          then Nothing
          else let (hdr,chkSum) = runGet getHeaderAndChkSum block
                in if checkChkSum block chkSum
                     then Just hdr
                     else error $ "TAR header checksum failure." 

checkChkSum :: ByteString -> Int -> Bool
checkChkSum block s = s == sumBS block' || s == signedChkSum block'
  where 
    block' = setPart 148 (BS.replicate 8 ' ') block
    -- tar.info says that Sun tar is buggy and 
    -- calculates the checksum using signed chars
    signedChkSum = BS.foldl' (\x y -> x + (signedOrd y)) 0

signedOrd :: Char -> Int
signedOrd c = fromIntegral (fromIntegral (ord c) :: Int8)

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
                            tarModTime     = fromInteger time,
                            tarFileType    = typ,
                            tarLinkTarget  = target,
                            tarOwnerName   = uname,
                            tarGroupName   = gname,
                            tarDeviceMajor = major,
                            tarDeviceMinor = minor
                           }
       return (hdr,chkSum)

getTarFileType :: Get TarFileType
getTarFileType = 
    do c <- getChar8
       return $ case c of
                  '\0'-> TarNormalFile
                  '0' -> TarNormalFile
                  '1' -> TarHardLink
                  '2' -> TarSymbolicLink
                  '3' -> TarCharacterDevice
                  '4' -> TarBlockDevice
                  '5' -> TarDirectory
                  '6' -> TarFIFO
                  '7' -> TarContiguous
                  _   -> TarCustom c
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
