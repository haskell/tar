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

prop_write_read_ustar :: [Entry] -> Bool
prop_write_read_ustar entries =
    foldr Next Done entries' == read (write entries')
  where
    entries' = [ e { entryFormat = UstarFormat } | e <- entries ]

prop_write_read_gnu :: [Entry] -> Bool
prop_write_read_gnu entries =
    foldr Next Done entries' == read (write entries')
  where
    entries' = [ e { entryFormat = GnuFormat } | e <- entries ]

prop_write_read_v7 :: [Entry] -> Bool
prop_write_read_v7 entries =
    foldr Next Done entries' == read (write entries')
  where
    entries' = [ limitToV7FormatCompat e { entryFormat = V7Format }
               | e <- entries ]
