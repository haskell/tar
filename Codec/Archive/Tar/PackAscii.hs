{-# LANGUAGE PackageImports #-}
{-# OPTIONS_HADDOCK hide #-}

module Codec.Archive.Tar.PackAscii
  ( toPosixString
  , fromPosixString
  , posixToByteString
  , byteToPosixString
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Short as Sh
import System.IO.Unsafe (unsafePerformIO)
import "os-string" System.OsString.Posix (PosixString)
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
