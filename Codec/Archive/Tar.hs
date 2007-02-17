-- | This is a library for reading and writing TAR archives.
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
                          recurseDirectories,
                          -- * Writing TAR archives
                          writeTarArchive,
                          writeTarFile,
                          -- * Extracting TAR archives
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
import qualified Data.ByteString.Lazy.Char8 as L
import Data.ByteString.Lazy (ByteString)
import System.IO


-- | Creates a TAR archive containing a number of files
-- and directories, and write the archive to a file. 
--
-- See 'createTarArchive' and 'writeTarArchive' for more information.
createTarFile :: FilePath   -- ^ File to write the archive to.
              -> [FilePath] -- ^ Files and directories to include in the archive.
              -> IO ()
createTarFile f fs = createTarData fs >>= L.writeFile f

-- | Creates a TAR archive containing a number of files
-- and directories, and return the archive as a lazy ByteString.
--
-- See 'createTarArchive' and 'writeTarArchive' for more information.
createTarData :: [FilePath]    -- ^ Files and directories to include in the archive.
              -> IO ByteString
createTarData = liftM writeTarArchive . createTarArchive 

-- | Writes a TAR archive to a file.
--
-- See 'writeTarArchive' for more information.
writeTarFile :: FilePath   -- ^ The file to write the archive to.
             -> TarArchive -- ^ The archive to write out.
             -> IO ()
writeTarFile f = L.writeFile f . writeTarArchive


-- | Reads a TAR archive from a file.
--
-- See 'readTarArchive' for more information.
readTarFile :: FilePath -- ^ File to read the archive from.
            -> IO TarArchive
readTarFile = liftM readTarArchive . L.readFile


-- | Reads a TAR archive from a file and extracts its contents into
-- the current directory.
--
-- See 'readTarArchive' and 'extractTarArchive' for more information.
extractTarFile :: FilePath -- ^ File from which the archive is read.
               -> IO ()
extractTarFile f = L.readFile f >>= extractTarData

-- | Reads a TAR archive from a lazy ByteString and extracts its contents into
-- the current directory.
--
-- See 'readTarArchive' and 'extractTarArchive' for more information.
extractTarData :: ByteString -- ^ Data from which the archive is read.
               -> IO ()
extractTarData = extractTarArchive . readTarArchive


