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
-- files or directories that have been unpacked before the error was
-- encountered will not be deleted. For this reason you may want to unpack
-- into an empty directory so that you can easily clean up if unpacking fails
-- part-way.
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
unpack baseDir entries = unpackEntries [] (checkSecurity entries)
                     >>= emulateLinks

  where
    unpackEntries _     (Fail err)      = fail err
    unpackEntries links Done            = return links
    unpackEntries links (Next entry es) = case entryContent entry of
      NormalFile file _ -> extractFile path file
                        >> unpackEntries links es
      Directory         -> extractDir path
                        >> unpackEntries links es
      HardLink     link -> (unpackEntries $! saveLink path link links) es
      SymbolicLink link -> (unpackEntries $! saveLink path link links) es
      _                 -> unpackEntries links es --ignore other file types
      where
        path = entryPath entry

    extractFile path content = do
      createDirectoryIfMissing False absDir
      BS.writeFile absPath content
      where
        absDir  = baseDir </> FilePath.Native.takeDirectory path
        absPath = baseDir </> path

    extractDir path = createDirectoryIfMissing False (baseDir </> path)

    saveLink path link links = seq (length path)
                             $ seq (length link')
                             $ (path, link'):links
      where link' = fromLinkTarget link

    emulateLinks = mapM_ $ \(relPath, relLinkTarget) ->
      let absPath   = baseDir </> relPath
          absTarget = FilePath.Native.takeDirectory absPath </> relLinkTarget
       in copyFile absTarget absPath
