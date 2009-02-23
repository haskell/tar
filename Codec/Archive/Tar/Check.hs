-----------------------------------------------------------------------------
-- |
-- Module      :  Codec.Archive.Tar
-- Copyright   :  (c) 2008-2009 Duncan Coutts
-- License     :  BSD3
--
-- Maintainer  :  duncan@haskell.org
-- Portability :  portable
--
-----------------------------------------------------------------------------
module Codec.Archive.Tar.Check (
  checkSecurity, checkEntrySecurity,
  checkTarbomb,
  ) where

import Codec.Archive.Tar.Types

import Control.Monad (MonadPlus(mplus))
import qualified System.FilePath as FilePath.Native
         ( splitDirectories, isAbsolute, isValid )

checkSecurity :: Entries -> Entries
checkSecurity =
  mapEntries (\entry -> maybe (Right entry) Left (checkEntrySecurity entry))

checkTarbomb :: FilePath -> Entries -> Entries
checkTarbomb _ = id

--checkPortability --TODO: check for only ordinary files


checkEntrySecurity :: Entry -> Maybe String
checkEntrySecurity entry = case fileType entry of
    HardLink     -> check (fileName entry) `mplus` check (linkTarget entry)
    SymbolicLink -> check (fileName entry) `mplus` check (linkTarget entry)
    _            -> check (fileName entry)

  where
    check name
      | FilePath.Native.isAbsolute name
      = Just $ "Absolute file name in tar archive: " ++ show name
      | not (FilePath.Native.isValid name)
      = Just $ "Invalid file name in tar archive: " ++ show name
      | any (=="..") (FilePath.Native.splitDirectories name)
      = Just $ "Invalid file name in tar archive: " ++ show name
      | otherwise = Nothing
