module Codec.Archive.Tar.Util where

import Control.Exception (Exception(..), catchJust)
import Data.Bits
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.ByteString.Lazy (ByteString)
import Data.Char (ord)
import Data.Int (Int64)
import System.IO
import System.IO.Error

-- * Functions

fixEq :: Eq a => (a -> a) -> a -> a
fixEq f x = let x' = f x in if x' == x then x else fixEq f x'

-- * IO

warn :: String -> IO ()
warn = hPutStrLn stderr . ("tar: "++)

doesNotExist :: String -> FilePath -> IO a
doesNotExist loc = ioError . mkIOError doesNotExistErrorType loc Nothing . Just

illegalOperation :: String -> Maybe FilePath -> IO a
illegalOperation loc = ioError . mkIOError illegalOperationErrorType loc Nothing

catchJustIOError :: (IOErrorType -> Bool) -> IO a -> (IOError -> IO a) -> IO a
catchJustIOError p = catchJust q
  where q (IOException ioe) | p (ioeGetErrorType ioe) = Just ioe
        q _                                           = Nothing

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

-- FIXME: make nicer, no IO
forceRelativePath :: FilePath -> IO FilePath
forceRelativePath p
    | null d = return p
    | otherwise = do warn $ "removing initial " ++ d ++" from path " ++ p
                     return p'
      where p' = fixEq (removeDotDot . removeSep) p
            d = take (length p - length p') p
            removeDotDot ('.':'.':x) = x
            removeDotDot x = x
            removeSep (c:x) | c == pathSep = x
            removeSep x = x
