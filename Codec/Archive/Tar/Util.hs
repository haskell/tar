module Codec.Archive.Tar.Util where

import Control.Exception (Exception(..), catchJust)
import Control.Monad (liftM)
import Data.Bits (Bits, shiftL, (.|.))
import System.IO (hPutStrLn, stderr)
import System.IO.Error (IOErrorType, ioeGetErrorType, mkIOError, 
                        doesNotExistErrorType, illegalOperationErrorType)
import System.Posix.Types (EpochTime)
import System.Time (ClockTime(..))

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

-- * Monads

unfoldM :: Monad m => m (Maybe a) -> m [a]
unfoldM f = f >>= maybe (return []) (\x -> liftM (x:) (unfoldM f))

-- * Bits

boolsToBits :: Bits a => [Bool] -> a
boolsToBits = f 0
  where f x [] = x
        f x (b:bs) = f (x `shiftL` 1 .|. if b then 1 else 0) bs

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

-- * Date and time

epochTimeToSecs :: EpochTime -> Integer
epochTimeToSecs = round . toRational

clockTimeToEpochTime :: ClockTime -> EpochTime
clockTimeToEpochTime (TOD s _) = fromInteger s