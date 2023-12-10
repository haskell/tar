module Codec.Archive.Tar.PackAscii
  ( packAscii
  ) where

import qualified Data.ByteString.Char8 as BS.Char8
import Data.Char
import GHC.Stack

-- | We should really migrate to @OsPath@ from @filepath@ package,
-- but for now let's not corrupt data silently.
packAscii :: HasCallStack => FilePath -> BS.Char8.ByteString
packAscii xs
  | all isAscii xs = BS.Char8.pack xs
  | otherwise = error $ "packAscii: only ASCII filenames are supported, but got " ++ xs
