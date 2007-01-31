-- | Implements the USTAR (POSIX.1-1988) format (tar with extended header information).
module Codec.Archive.Tar (
                          -- * TAR archive types
                          TarArchive(..),
                          TarEntry(..),
                          TarHeader(..),
                          TarFileType(..),
                          -- * Creating TAR archives
                          createTarFile,
                          createTarData,
                          createTarArchive,
                          createTarEntry,
                          -- ** Utilities
                          recurseDirectories,
                          -- * Writing TAR archives
                          writeTarArchive,
                          writeTarFile,
                          -- * extracting TAR archives
                          extractTarFile,
                          extractTarData,
                          extractTarArchive,
                          extractTarEntry,
                          -- * Reading TAR archives
                          readTarArchive,
                          readTarFile
                         ) where

import Codec.Archive.Tar.Create
import Codec.Archive.Tar.Extract
import Codec.Archive.Tar.Read
import Codec.Archive.Tar.Types
import Codec.Archive.Tar.Write

import Control.Monad (liftM)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.ByteString.Lazy (ByteString)
import System.IO


-- * Creating tar archives

-- | Create a TAR archive containing a number of files
-- and directories, and write the archive to a file. 
createTarFile :: FilePath -- ^ File to write the archive to.
              -> [FilePath] 
              -- ^ Files and directories to include in the archive.
              -- Any directory should come before any files in that directory.
              -- Only files and directories mentioned in the list are included,
              -- this function does not recurse into the directories.
              -> IO ()
createTarFile f fs = createTarData fs >>= BS.writeFile f

-- | Convenience function:
-- @createTarData = liftM writeTarArchive . createTarArchive@
createTarData :: [FilePath] -> IO ByteString
createTarData = liftM writeTarArchive . createTarArchive 


-- * Extracting tar archives

extractTarFile :: FilePath -> IO ()
extractTarFile f = BS.readFile f >>= extractTarData

extractTarData :: ByteString -> IO ()
extractTarData = extractTarArchive . readTarArchive

