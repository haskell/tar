{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_HADDOCK hide #-}
{-# HLINT ignore "Use for_" #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Codec.Archive.Tar
-- Copyright   :  (c) 2007 Bjorn Bringert,
--                    2008 Andrea Vezzosi,
--                    2008-2009, 2012, 2016 Duncan Coutts
-- License     :  BSD3
--
-- Maintainer  :  duncan@community.haskell.org
-- Portability :  portable
--
-----------------------------------------------------------------------------
module Codec.Archive.Tar.Unpack (
  unpack,
  unpackAndCheck,
  ) where

import Codec.Archive.Tar.Types
import Codec.Archive.Tar.Check
import Codec.Archive.Tar.LongNames

import Data.Bits
         ( testBit )
import Data.List (partition, nub)
import Data.Maybe ( fromMaybe )
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy as BS
import System.FilePath
         ( (</>) )
import qualified System.FilePath as FilePath.Native
         ( takeDirectory )
import System.Directory
    ( createDirectoryIfMissing,
      copyFile,
      setPermissions,
      listDirectory,
      doesDirectoryExist,
      createDirectoryLink,
      createFileLink,
      setModificationTime,
      emptyPermissions,
      setOwnerReadable,
      setOwnerWritable,
      setOwnerExecutable,
      setOwnerSearchable )
import Control.Exception
         ( Exception, throwIO, handle )
import System.IO ( stderr, hPutStr )
import System.IO.Error ( ioeGetErrorType, isPermissionError )
import GHC.IO (unsafeInterleaveIO)
import Data.Foldable (traverse_)
import GHC.IO.Exception (IOErrorType(InappropriateType, IllegalOperation, PermissionDenied, InvalidArgument))
import Data.Time.Clock.POSIX
         ( posixSecondsToUTCTime )
import Control.Exception as Exception
         ( catch, SomeException(..) )

-- | Create local files and directories based on the entries of a tar archive.
--
-- This is a portable implementation of unpacking suitable for portable
-- archives. It handles 'NormalFile' and 'Directory' entries and has simulated
-- support for 'SymbolicLink' and 'HardLink' entries. Links are implemented by
-- copying the target file. This therefore works on Windows as well as Unix.
-- All other entry types are ignored, that is they are not unpacked and no
-- exception is raised.
--
-- If the 'Entries' ends in an error then it is raised an an exception. Any
-- files or directories that have been unpacked before the error was
-- encountered will not be deleted. For this reason you may want to unpack
-- into an empty directory so that you can easily clean up if unpacking fails
-- part-way.
--
-- On its own, this function only checks for security (using 'checkEntrySecurity').
-- Use 'unpackAndCheck' if you need more checks.
--
unpack
  :: Exception e
  => FilePath
  -- ^ Base directory
  -> Entries e
  -- ^ Entries to upack
  -> IO ()
unpack = unpackAndCheck (fmap SomeException . checkEntrySecurity)

-- | Like 'Codec.Archive.Tar.unpack', but run custom sanity/security checks instead of 'checkEntrySecurity'.
-- For example,
--
-- > import Control.Exception (SomeException(..))
-- > import Control.Applicative ((<|>))
-- >
-- > unpackAndCheck (\x -> SomeException <$> checkEntryPortability x
-- >                   <|> SomeException <$> checkEntrySecurity x) dir entries
--
-- @since 0.6.0.0
unpackAndCheck
  :: Exception e
  => (GenEntry FilePath FilePath -> Maybe SomeException)
  -- ^ Checks to run on each entry before unpacking
  -> FilePath
  -- ^ Base directory
  -> Entries e
  -- ^ Entries to upack
  -> IO ()
unpackAndCheck secCB baseDir entries = do
  let resolvedEntries = decodeLongNames entries
  uEntries <- unpackEntries [] resolvedEntries
  let (hardlinks, symlinks) = partition (\(_, _, x) -> x) uEntries
  -- handle hardlinks first, in case a symlink points to it
  handleHardLinks hardlinks
  handleSymlinks symlinks

  where
    -- We're relying here on 'secCB' to make sure we're not scribbling
    -- files all over the place.

    unpackEntries :: Exception e
                  => [(FilePath, FilePath, Bool)]
                  -- ^ links (path, link, isHardLink)
                  -> GenEntries FilePath FilePath (Either e DecodeLongNamesError)
                  -- ^ entries
                  -> IO [(FilePath, FilePath, Bool)]
    unpackEntries _     (Fail err)      = either throwIO throwIO err
    unpackEntries links Done            = return links
    unpackEntries links (Next entry es) = do
      case secCB entry of
        Nothing -> pure ()
        Just e -> throwIO e

      case entryContent entry of
        NormalFile file _ -> do
          extractFile (entryPermissions entry) (entryTarPath entry) file (entryTime entry)
          unpackEntries links es
        Directory -> do
          extractDir (entryTarPath entry) (entryTime entry)
          unpackEntries links es
        HardLink link -> do
          (unpackEntries $! saveLink True (entryTarPath entry) link links) es
        SymbolicLink link -> do
          (unpackEntries $! saveLink False (entryTarPath entry) link links) es
        OtherEntryType{} ->
          -- the spec demands that we attempt to extract as normal file on unknown typecode,
          -- but we just skip it
          unpackEntries links es
        CharacterDevice{} -> unpackEntries links es
        BlockDevice{} -> unpackEntries links es
        NamedPipe -> unpackEntries links es

    extractFile permissions (fromFilePathToNative -> path) content mtime = do
      -- Note that tar archives do not make sure each directory is created
      -- before files they contain, indeed we may have to create several
      -- levels of directory.
      createDirectoryIfMissing True absDir
      BS.writeFile absPath content
      setOwnerPermissions absPath permissions
      setModTime absPath mtime
      where
        absDir  = baseDir </> FilePath.Native.takeDirectory path
        absPath = baseDir </> path

    extractDir (fromFilePathToNative -> path) mtime = do
      createDirectoryIfMissing True absPath
      setModTime absPath mtime
      where
        absPath = baseDir </> path

    saveLink isHardLink (fromFilePathToNative -> path) (fromFilePathToNative -> link) links
      = seq (length path)
          $ seq (length link)
          $ (path, link, isHardLink):links


    -- for hardlinks, we just copy
    handleHardLinks = mapM_ $ \(relPath, relLinkTarget, _) ->
      let absPath   = baseDir </> relPath
          -- hard links link targets are always "absolute" paths in
          -- the context of the tar root
          absTarget = baseDir </> relLinkTarget
      -- we don't expect races here, since we should be the
      -- only process unpacking the tar archive and writing to
      -- the destination
      in doesDirectoryExist absTarget >>= \case
          True -> copyDirectoryRecursive absTarget absPath
          False -> copyFile absTarget absPath

    -- For symlinks, we first try to recreate them and if that fails
    -- with 'IllegalOperation', 'PermissionDenied' or 'InvalidArgument',
    -- we fall back to copying.
    -- This error handling isn't too fine grained and maybe should be
    -- platform specific, but this way it might catch erros on unix even on
    -- FAT32 fuse mounted volumes.
    handleSymlinks = mapM_ $ \(relPath, relLinkTarget, _) ->
      let absPath   = baseDir </> relPath
          -- hard links link targets are always "absolute" paths in
          -- the context of the tar root
          absTarget = FilePath.Native.takeDirectory absPath </> relLinkTarget
      -- we don't expect races here, since we should be the
      -- only process unpacking the tar archive and writing to
      -- the destination
      in doesDirectoryExist absTarget >>= \case
          True -> handleSymlinkError (copyDirectoryRecursive absTarget absPath)
            $ createDirectoryLink relLinkTarget absPath
          False -> handleSymlinkError (copyFile absTarget absPath)
            $ createFileLink relLinkTarget absPath

      where
        handleSymlinkError action =
          handle (\e -> if ioeGetErrorType e `elem` [IllegalOperation
                                                    ,PermissionDenied
                                                    ,InvalidArgument]
                      then action
                      else throwIO e
                 )

-- | Recursively copy the contents of one directory to another path.
--
-- This is a rip-off of Cabal library.
copyDirectoryRecursive :: FilePath -> FilePath -> IO ()
copyDirectoryRecursive srcDir destDir = do
  srcFiles <- getDirectoryContentsRecursive srcDir
  copyFilesWith copyFile destDir [ (srcDir, f)
                                   | f <- srcFiles ]
  where
    -- | Common implementation of 'copyFiles', 'installOrdinaryFiles',
    -- 'installExecutableFiles' and 'installMaybeExecutableFiles'.
    copyFilesWith :: (FilePath -> FilePath -> IO ())
                  -> FilePath -> [(FilePath, FilePath)] -> IO ()
    copyFilesWith doCopy targetDir srcFiles = do

      -- Create parent directories for everything
      let dirs = map (targetDir </>) . nub . map (FilePath.Native.takeDirectory . snd) $ srcFiles
      traverse_ (createDirectoryIfMissing True) dirs

      -- Copy all the files
      sequence_ [ let src  = srcBase   </> srcFile
                      dest = targetDir </> srcFile
                   in doCopy src dest
                | (srcBase, srcFile) <- srcFiles ]

    -- | List all the files in a directory and all subdirectories.
    --
    -- The order places files in sub-directories after all the files in their
    -- parent directories. The list is generated lazily so is not well defined if
    -- the source directory structure changes before the list is used.
    --
    getDirectoryContentsRecursive :: FilePath -> IO [FilePath]
    getDirectoryContentsRecursive topdir = recurseDirectories [""]
      where
        recurseDirectories :: [FilePath] -> IO [FilePath]
        recurseDirectories []         = return []
        recurseDirectories (dir:dirs) = unsafeInterleaveIO $ do
          (files, dirs') <- collect [] [] =<< listDirectory (topdir </> dir)
          files' <- recurseDirectories (dirs' ++ dirs)
          return (files ++ files')

          where
            collect files dirs' []              = return (reverse files
                                                         ,reverse dirs')
            collect files dirs' (entry:entries) = do
              let dirEntry = dir </> entry
              isDirectory <- doesDirectoryExist (topdir </> dirEntry)
              if isDirectory
                then collect files (dirEntry:dirs') entries
                else collect (dirEntry:files) dirs' entries

setModTime :: FilePath -> EpochTime -> IO ()
setModTime path t =
    setModificationTime path (posixSecondsToUTCTime (fromIntegral t))
      `Exception.catch` \e -> case ioeGetErrorType e of
        PermissionDenied -> return ()
        -- On FAT32 file system setting time prior to DOS Epoch (1980-01-01)
        -- throws InvalidArgument, https://github.com/haskell/tar/issues/37
        InvalidArgument -> return ()
        _ -> throwIO e

setOwnerPermissions :: FilePath -> Permissions -> IO ()
setOwnerPermissions path permissions =
  setPermissions path ownerPermissions
  where
    -- | Info on Permission bits can be found here:
    -- https://www.gnu.org/software/libc/manual/html_node/Permission-Bits.html
    ownerPermissions =
      setOwnerReadable   (testBit permissions 8) $
      setOwnerWritable   (testBit permissions 7) $
      setOwnerExecutable (testBit permissions 6) $
      setOwnerSearchable (testBit permissions 6) $
      emptyPermissions
