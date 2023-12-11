{-# LANGUAGE PackageImports #-}
{-# OPTIONS_HADDOCK hide #-}

module Codec.Archive.Tar.PackAscii
  ( posixToByteString
  , byteToPosixString
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Short as Sh
import "os-string" System.OsString.Posix (PosixString)
import qualified "os-string" System.OsString.Internal.Types as PS

posixToByteString :: PosixString -> ByteString
posixToByteString = Sh.fromShort . PS.getPosixString

byteToPosixString :: ByteString -> PosixString
byteToPosixString = PS.PosixString . Sh.toShort
