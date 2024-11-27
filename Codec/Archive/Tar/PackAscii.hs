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
  , packAscii
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

packAscii :: HasCallStack => FilePath -> BS.Char8.ByteString
packAscii xs
  | all isAscii xs = BS.Char8.pack xs
  | otherwise = error $ "packAscii: only ASCII inputs are supported, but got " ++ xs

filePathToOsPath :: FilePath -> OS.OsPath
filePathToOsPath = unsafePerformIO . OS.encodeFS

osPathToFilePath :: OS.OsPath -> FilePath
osPathToFilePath = unsafePerformIO . OS.decodeFS
