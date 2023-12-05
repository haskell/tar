{-# LANGUAGE OverloadedStrings #-}

module Codec.Archive.Tar.Unpack.Tests
  ( case_modtime_1970
  ) where

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Tar.Types as Tar
import Codec.Archive.Tar.Types (GenEntries(..), Entries, GenEntry(..))
import qualified Codec.Archive.Tar.Unpack as Unpack
import Control.Exception
import Data.Time.Clock
import Data.Time.Clock.System
import System.Directory
import System.FilePath
import System.IO.Temp
import Test.Tasty.QuickCheck

case_modtime_1970 :: Property
case_modtime_1970 = ioProperty $ withSystemTempDirectory "tar-test" $ \baseDir -> do
  let filename = "foo"
  Right tarPath <- pure $ Tar.toTarPath False filename
  let entry = (Tar.fileEntry tarPath "bar") { entryTime = 0 }
      entries = Next entry Done :: Entries IOException
  Tar.unpack baseDir entries
  modTime <- getModificationTime (baseDir </> filename)
  pure $ modTime === UTCTime systemEpochDay 0
