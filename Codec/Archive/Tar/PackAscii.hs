{-# LANGUAGE PackageImports #-}

{-# OPTIONS_HADDOCK hide #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid restricted function" #-}

module Codec.Archive.Tar.PackAscii
  ( toPosixString
  , fromPosixString
  , posixToByteString
  , byteToPosixString
  , packLatin1
  , filePathToOsPath
  , osPathToFilePath
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS.Char8
import qualified Data.ByteString.Short as Sh
import Data.Char
import GHC.Stack
import System.IO.Unsafe (unsafePerformIO)
import "os-string" System.OsString.Posix (PosixString)
import qualified "filepath" System.OsPath as OS
import qualified "os-string" System.OsString.Posix as PS
import qualified "os-string" System.OsString.Internal.Types as PS

toPosixString :: FilePath -> PosixString
toPosixString = unsafePerformIO . PS.encodeFS

fromPosixString :: PosixString -> FilePath
fromPosixString = unsafePerformIO . PS.decodeFS

posixToByteString :: PosixString -> ByteString
posixToByteString = Sh.fromShort . PS.getPosixString

byteToPosixString :: ByteString -> PosixString
byteToPosixString = PS.PosixString . Sh.toShort

packLatin1 :: HasCallStack => FilePath -> BS.Char8.ByteString
packLatin1 xs
  | all isLatin1 xs = BS.Char8.pack xs
  | otherwise = error $ "packLatin1: only Latin-1 inputs are supported, but got " ++ xs

filePathToOsPath :: FilePath -> OS.OsPath
filePathToOsPath = unsafePerformIO . OS.encodeFS

osPathToFilePath :: OS.OsPath -> FilePath
osPathToFilePath = unsafePerformIO . OS.decodeFS
