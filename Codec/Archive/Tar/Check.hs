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
  FileNameError(..),

  -- * Tarbombs
  checkTarbomb,
  TarBombError(..),

  -- * Portability
  checkPortability,
  PortabilityError(..),
  PortabilityPlatform,
  ) where

import Codec.Archive.Tar.Check.Internal
