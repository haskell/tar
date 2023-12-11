{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Codec.Archive.Tar.Unpack.Tests
  ( case_modtime_1970
  ) where

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Tar.Types as Tar
import Codec.Archive.Tar.Types (GenEntries(..), Entries, GenEntry(..))
import Control.Exception
import Data.Time.Clock
import Data.Time.Clock.System
import System.Directory.OsPath
import System.IO.Temp
import Test.Tasty.QuickCheck
import qualified System.OsPath as OSP

case_modtime_1970 :: Property
case_modtime_1970 = ioProperty $ withSystemTempDirectory "tar-test" $ \baseDir' -> do
  baseDir <- OSP.encodeFS baseDir'
  let filename = [OSP.osp|foo|]
  Right tarPath <- pure $ Tar.toTarPath False filename
  let entry = (Tar.fileEntry tarPath "bar") { entryTime = 0 }
      entries = Next entry Done :: Entries IOException
  Tar.unpack baseDir entries
  modTime <- getModificationTime (baseDir OSP.</> filename)
  pure $ modTime === UTCTime systemEpochDay 0
