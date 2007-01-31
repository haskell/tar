module Codec.Archive.Tar.Util where

import Data.Bits
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.ByteString.Lazy (ByteString)
import Data.Char (ord)
import Data.Int (Int64)
import System.IO
import System.IO.Error

-- * IO

warn :: String -> IO ()
warn = hPutStrLn stderr . ("htar: "++)

doesNotExistError :: String -> FilePath -> IOError
doesNotExistError loc = mkIOError doesNotExistErrorType loc Nothing . Just

-- * Bits

boolsToBits :: Bits a => [Bool] -> a
boolsToBits = f 0
  where f x [] = x
        f x (b:bs) = f (x `shiftL` 1 .|. if b then 1 else 0) bs

-- * ByteString

sumBS :: ByteString -> Int
sumBS = BS.foldl' (\x y -> x + ord y) 0

setPart :: Int64 -> ByteString -> ByteString -> ByteString
setPart off new old = 
    let (before,rest) = BS.splitAt off old
        after = BS.drop (BS.length new) rest
     in before `BS.append` (BS.take (BS.length old - off) new) `BS.append` after

lpad :: Int64 -> Char -> ByteString -> ByteString
lpad n b xs = BS.replicate (n - BS.length xs) b `BS.append` xs

rpad :: Int64 -> Char -> ByteString -> ByteString
rpad n b xs = xs `BS.append` BS.replicate (n - BS.length xs) b

-- | Right-pad up to the nearest multiple of the given length.
rpadMod :: Int64 -> Char -> ByteString -> ByteString
rpadMod n b xs = xs `BS.append` BS.replicate ((n - BS.length xs) `mod` n) b

ltrunc :: Int64 -> ByteString -> ByteString
ltrunc n xs = BS.drop (BS.length xs - n) xs

-- * File paths

pathSep :: Char
pathSep = '/' -- FIXME: backslash on Windows

-- FIXME: not good enough. Use System.FilePath?
dirName :: FilePath -> FilePath
dirName p = if null d then "." else d
  where d = reverse $ dropWhile (/=pathSep) $ reverse p