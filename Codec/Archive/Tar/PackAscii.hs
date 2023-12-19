{-# LANGUAGE PackageImports #-}

module Codec.Archive.Tar.PackAscii
  ( packAscii
  , toPosixString
  , fromPosixString
  , posixToByteString
  , byteToPosixString
  ) where

import qualified Data.ByteString.Char8 as BS.Char8
import qualified Data.ByteString.Short as Sh
import Data.Char
import GHC.Stack
import System.IO.Unsafe (unsafePerformIO)
import "os-string" System.OsString.Posix (PosixString)
import qualified "os-string" System.OsString.Posix as PS
import qualified "os-string" System.OsString.Internal.Types as PS

-- | We should really migrate to @OsPath@ from @filepath@ package,
-- but for now let's not corrupt data silently.
packAscii :: HasCallStack => FilePath -> BS.Char8.ByteString
packAscii xs
  | all isAscii xs = BS.Char8.pack xs
  | otherwise = error $ "packAscii: only ASCII filenames are supported, but got " ++ xs

toPosixString :: FilePath -> PosixString
toPosixString = unsafePerformIO . PS.encodeFS

fromPosixString :: PosixString -> FilePath
fromPosixString = unsafePerformIO . PS.decodeFS

posixToByteString :: PosixString -> BS.Char8.ByteString
posixToByteString = Sh.fromShort . PS.getPosixString

byteToPosixString :: BS.Char8.ByteString -> PosixString
byteToPosixString = PS.PosixString . Sh.toShort
