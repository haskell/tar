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
  ) where

import Codec.Archive.Tar
import Codec.Archive.Tar.Types
import Codec.Archive.Tar.Types.Tests
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
