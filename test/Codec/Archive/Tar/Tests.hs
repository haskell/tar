{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Codec.Archive.Tar.Tests
-- Copyright   :  (c) 2007 Bjorn Bringert,
--                    2008 Andrea Vezzosi,
--                    2008-2012 Duncan Coutts
-- License     :  BSD3
--
-- Maintainer  :  duncan@community.haskell.org
-- Portability :  portable
--
-----------------------------------------------------------------------------
module Codec.Archive.Tar.Tests (
  prop_write_read_ustar,
  prop_write_read_gnu,
  prop_write_read_v7,
  prop_large_filesize,
  ) where

import Codec.Archive.Tar
import Codec.Archive.Tar.Types
import Codec.Archive.Tar.Types.Tests
import qualified Data.ByteString.Lazy as BL
import Prelude hiding (read)
import Test.Tasty.QuickCheck

prop_write_read_ustar :: [Entry] -> Property
prop_write_read_ustar entries =
    foldr Next Done entries' === read (write entries')
  where
    entries' = filter ((== UstarFormat) . entryFormat) entries

prop_write_read_gnu :: [Entry] -> Property
prop_write_read_gnu entries =
    foldr Next Done entries' === read (write entries')
  where
    entries' = filter ((== GnuFormat) . entryFormat) entries

prop_write_read_v7 :: [Entry] -> Property
prop_write_read_v7 entries =
    foldr Next Done entries' === read (write entries')
  where
    entries' = map limitToV7FormatCompat $ filter ((== V7Format) . entryFormat) entries

prop_large_filesize :: Word -> Property
prop_large_filesize n = sz === sz'
  where
    sz = fromIntegral $ n * 1024 * 1024 * 128
    Right fn = toTarPath False "Large.file"
    entry = simpleEntry fn (NormalFile (BL.replicate sz 42) sz)
    -- Trim the tail so it does not blow up RAM
    tar = BL.take 2048 $ write [entry]
    Next entry' _ = read tar
    NormalFile _ sz' = entryContent entry'
