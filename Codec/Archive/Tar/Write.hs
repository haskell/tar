module Codec.Archive.Tar.Write where

import Codec.Archive.Tar.Types
import Codec.Archive.Tar.Util

import Data.Binary.Put

import qualified Data.ByteString.Lazy.Char8 as BS
import Data.ByteString.Lazy (ByteString)
import Data.Char (ord)
import Data.Int (Int64)
import Numeric (showOct)


writeTarArchive :: TarArchive -> ByteString
writeTarArchive = runPut . putTarArchive

writeTarFile :: FilePath -> TarArchive -> IO ()
writeTarFile f = BS.writeFile f . writeTarArchive

putTarArchive :: TarArchive -> Put
putTarArchive (TarArchive es) = 
    do mapM_ putTarEntry es
       fill 512 '\0'
       fill 512 '\0'

putTarEntry :: TarEntry -> Put
putTarEntry (TarEntry hdr cnt) = 
    do putTarHeader hdr
       putLazyByteString (rpadMod 512 '\0' cnt)
       flush

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
       putOct      12 $ epochTimeToSecs $ tarModTime hdr
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

splitLongPath :: Int -> FilePath -> (String,String)
splitLongPath l path | l < 1 || null path = error $ unwords ["splitFileName", show l, show path]
splitLongPath l path | n > l = error $ "File path too long: " ++ show path -- FIXME: implement real splitting
                     | otherwise = ("",path)
  where n = length path

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

fill :: Int -> Char -> Put
fill n = putLazyByteString . BS.replicate (fromIntegral n)
