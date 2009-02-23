-----------------------------------------------------------------------------
-- |
-- Module      :  Codec.Archive.Tar
-- Copyright   :  (c) 2007 Bjorn Bringert,
--                    2008 Andrea Vezzosi,
--                    2008-2009 Duncan Coutts
-- License     :  BSD3
--
-- Maintainer  :  duncan@haskell.org
-- Portability :  portable
--
-----------------------------------------------------------------------------
module Codec.Archive.Tar.Unpack (
  unpack,
  ) where

import Codec.Archive.Tar.Types
import Codec.Archive.Tar.Check

import qualified Data.ByteString.Lazy as BS
import System.FilePath
         ( (</>) )
import qualified System.FilePath as FilePath.Native
         ( (</>), takeDirectory )
import System.Directory
         ( createDirectoryIfMissing, copyFile )

-- | Create local files and directories based on the entries of a tar archive.
--
-- This is a portable implementation of unpacking suitable for portable
-- archives. It handles 'NormalFile' and 'Directory' entries and has simulated
-- support for 'SymbolicLink' and 'HardLink' entries. Links are implemented by
-- copying the target file. This therefore works on Windows as well as Unix.
-- All other entry types are ignored, that is they are not unpacked and no
-- exception is raised.
--
-- If the 'Entries' ends in an error then it is raised an an IO error. Any
-- files or directories that have been upacked before the error was encountered
-- will not be deleted. For this reason you may want to unpack into an empty
-- directory so that you can easily clean up if unpacking fails part-way.
--
-- On its own, this function only checks for security (using 'checkSecurity').
-- You can do other checks by applying checking functions to the 'Entries' that
-- you pass to this function. For example:
--
-- > unpack dir (checkTarbomb expectedDir entries)
--
-- If you care about the priority of the reported errors then you may want to
-- use 'checkSecurity' before 'checkTarbomb' or other checks.
--
unpack :: FilePath -> Entries -> IO ()
unpack baseDir entries = extractFiles [] (checkSecurity entries)
                     >>= extractLinks
  where
    extractFiles _     (Fail err)            = fail err
    extractFiles links Done                  = return links
    extractFiles links (Next entry entries') = case fileType entry of
      NormalFile   -> extractFile entry >> extractFiles links entries'
      HardLink     -> extractFiles (saveLink entry links) entries'
      SymbolicLink -> extractFiles (saveLink entry links) entries'
      Directory    -> extractDir entry >> extractFiles links entries'
      _            -> extractFiles links entries' --ignore other file types

    extractFile entry = do
      createDirectoryIfMissing False fileDir
      BS.writeFile fullPath (fileContent entry)
      where
        fileDir  = baseDir </> FilePath.Native.takeDirectory (fileName entry)
        fullPath = baseDir </> fileName entry

    extractDir entry =
      createDirectoryIfMissing False (baseDir </> fileName entry)

    saveLink entry links = seq (length name)
                         $ seq (length name)
                         $ link:links
      where
        name    = fileName entry
        target  = linkTarget entry
        link    = (name, target)

    extractLinks = mapM_ $ \(name, target) ->
      let path      = baseDir </> name
       in copyFile (FilePath.Native.takeDirectory path </> target) path
