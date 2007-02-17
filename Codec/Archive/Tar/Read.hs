module Codec.Archive.Tar.Read where

import Codec.Archive.Tar.Types
import Codec.Archive.Tar.Util

import Data.Binary.Get

import Data.Char (chr,ord)
import Control.Monad (liftM)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Int (Int8)
import Numeric (readOct)


readTarArchive :: L.ByteString -> TarArchive
readTarArchive = runGet getTarArchive

readTarFile :: FilePath -> IO TarArchive
readTarFile = liftM readTarArchive . L.readFile

getTarArchive :: Get TarArchive
getTarArchive = liftM TarArchive $ unfoldM getTarEntry

-- | Returns 'Nothing' if the entry is an end block.
getTarEntry :: Get (Maybe TarEntry)
getTarEntry =
    do mhdr <- getTarHeader
       case mhdr of
         Nothing -> return Nothing
         Just hdr -> liftM Just $ getBody hdr
  where getBody hdr = 
            do 
            let size = tarFileSize hdr -- FIXME: treat size as 0 for non-files, it seems
                                       -- like diretories can sometimes have non-zero size
                                       -- in the header.
                padding = (512 - size) `mod` 512
            cnt <- liftM (L.take size) $ getLazyByteString $ size + padding
            return $ TarEntry hdr cnt

getTarHeader :: Get (Maybe TarHeader)
getTarHeader =
    do -- FIXME: warn and return nothing on EOF
       block <- liftM B.copy $ getBytes 512
       return $ 
        if B.head block == '\NUL'
          then Nothing
          else let (hdr,chkSum) = 
                       runGet getHeaderAndChkSum $ L.fromChunks [block]
                in if checkChkSum block chkSum
                     then Just hdr
                     else error $ "TAR header checksum failure." 

checkChkSum :: B.ByteString -> Int -> Bool
checkChkSum block s = s == chkSum block' || s == signedChkSum block'
  where 
    block' = B.concat [B.take 148 block, B.replicate 8 ' ', B.drop 156 block]
    -- tar.info says that Sun tar is buggy and 
    -- calculates the checksum using signed chars
    chkSum = B.foldl' (\x y -> x + ord y) 0
    signedChkSum = B.foldl' (\x y -> x + (ordSigned y)) 0

ordSigned :: Char -> Int
ordSigned c = fromIntegral (fromIntegral (ord c) :: Int8)

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
getOct n = getBytes n >>= parseOct . takeWhile (/='\0') . B.unpack
  where parseOct "" = return 0
        parseOct s = case readOct s of
                       [(x,_)] -> return x
                       _       -> fail $ "Number format error: " ++ show s

getString :: Int -> Get String
getString n = liftM (takeWhile (/='\NUL') . B.unpack) $ getBytes n

getChar8 :: Get Char
getChar8 = fmap (chr . fromIntegral) getWord8
