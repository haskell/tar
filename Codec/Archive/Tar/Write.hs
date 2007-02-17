module Codec.Archive.Tar.Write (writeTarArchive) where

import Codec.Archive.Tar.Types
import Codec.Archive.Tar.Util

import Data.Binary.Put

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Char (ord)
import Numeric (showOct)

-- | Writes a TAR archive to a lazy ByteString.
--
-- The archive is written in USTAR (POSIX.1-1988) format 
-- (tar with extended header information).
writeTarArchive :: TarArchive -> L.ByteString
writeTarArchive = runPut . putTarArchive

putTarArchive :: TarArchive -> Put
putTarArchive (TarArchive es) = 
    do mapM_ putTarEntry es
       fill 512 '\0'
       fill 512 '\0'
       flush

putTarEntry :: TarEntry -> Put
putTarEntry (TarEntry hdr cnt) = 
    do putTarHeader hdr
       putContent cnt
       flush

-- | Puts a lazy ByteString and nul-pads to a multiple of 512 bytes.
putContent :: L.ByteString -> Put
putContent = f 0 . L.toChunks
  where f 0 []     = return ()
        f n []     = fill (512 - n) '\NUL'
        f n (x:xs) = putByteString x >> f ((n+B.length x) `mod` 512) xs

putTarHeader :: TarHeader -> Put
putTarHeader hdr = 
    do let block = B.concat $ L.toChunks $ runPut (putHeaderNoChkSum hdr)
           chkSum = B.foldl' (\x y -> x + ord y) 0 block
       putByteString $ B.take 148 block
       putOct 8 chkSum
       putByteString $ B.drop 156 block

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
       putString    6 $ "ustar"
       putString    2 $ "00" -- no nul byte
       putString   32 $ tarOwnerName hdr
       putString   32 $ tarGroupName hdr
       putOct       8 $ tarDeviceMajor hdr
       putOct       8 $ tarDeviceMinor hdr
       putString  155 $ filePrefix
       fill        12 $ '\NUL'

putTarFileType :: TarFileType -> Put
putTarFileType t = 
    putChar8 $ case t of
                 TarNormalFile      -> '0'
                 TarHardLink        -> '1'
                 TarSymbolicLink    -> '2'
                 TarCharacterDevice -> '3'
                 TarBlockDevice     -> '4'
                 TarDirectory       -> '5'
                 TarFIFO            -> '6'
                 TarOther c         -> c

splitLongPath :: Int -> FilePath -> (String,String)
splitLongPath l path | l < 1 || null path = error $ unwords ["splitFileName", show l, show path]
splitLongPath l path | n > l = error $ "File path too long: " ++ show path -- FIXME: implement real splitting
                     | otherwise = ("",path)
  where n = length path

-- * TAR format primitive output

putString :: Int -> String -> Put
putString n s = do mapM_ putChar8 $ take n s
                   fill (n - length s) '\NUL'

putOct :: Integral a => Int -> a -> Put
putOct n x = do let o = take n $ showOct x ""
                fill (n - length o - 1) '0'
                mapM_ putChar8 o
                putChar8 '\NUL'

putChar8 :: Char -> Put
putChar8 c = putWord8 $ fromIntegral $ ord c

fill :: Int -> Char -> Put
fill n c = putByteString $ B.replicate n c
