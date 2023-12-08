-----------------------------------------------------------------------------
-- |
-- Module      :  Codec.Archive.Tar.Check
-- Copyright   :  (c) 2008-2012 Duncan Coutts
--                    2011 Max Bolingbroke
-- License     :  BSD3
--
-- Maintainer  :  duncan@community.haskell.org
-- Portability :  portable
--
-- Perform various checks on tar file entries.
--
-----------------------------------------------------------------------------
module Codec.Archive.Tar.Check (

  -- * Security
  checkSecurity,
  checkEntrySecurity,
  FileNameError(..),

  -- * Tarbombs
  checkTarbomb,
  checkEntryTarbomb,
  TarBombError(..),

  -- * Portability
  checkPortability,
  checkEntryPortability,
  PortabilityError(..),
  PortabilityPlatform,
  ) where

import Codec.Archive.Tar.Check.Internal
