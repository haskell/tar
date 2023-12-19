-----------------------------------------------------------------------------
-- |
-- Module      :  Codec.Archive.Tar.Types.Tests
-- Copyright   :  (c) 2007 Bjorn Bringert,
--                    2008 Andrea Vezzosi,
--                    2008-2009 Duncan Coutts
--                    2011 Max Bolingbroke
-- License     :  BSD3
--
-----------------------------------------------------------------------------
{-# LANGUAGE RecordWildCards #-}

module Codec.Archive.Tar.Types.Tests
  ( limitToV7FormatCompat
  , prop_fromTarPath
  , prop_fromTarPathToPosixPath
  , prop_fromTarPathToWindowsPath
  ) where

import Codec.Archive.Tar.PackAscii
import Codec.Archive.Tar.Types

import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BS.Char8
import qualified Data.ByteString.Lazy  as LBS

import qualified System.FilePath as FilePath.Native
         ( joinPath, splitDirectories, addTrailingPathSeparator )
import qualified System.FilePath.Posix as FilePath.Posix
         ( joinPath, splitPath, splitDirectories, hasTrailingPathSeparator
         , addTrailingPathSeparator )
import qualified System.FilePath.Windows as FilePath.Windows
         ( joinPath, splitDirectories, addTrailingPathSeparator )

import Test.QuickCheck
import Control.Applicative ((<$>), (<*>), pure)
import Data.Word (Word16)

prop_fromTarPath :: TarPath -> Property
prop_fromTarPath tp = fromTarPath tp === fromTarPathRef tp

prop_fromTarPathToPosixPath :: TarPath -> Property
prop_fromTarPathToPosixPath tp = fromTarPathToPosixPath tp === fromTarPathToPosixPathRef tp

prop_fromTarPathToWindowsPath :: TarPath -> Property
prop_fromTarPathToWindowsPath tp = fromTarPathToWindowsPath tp === fromTarPathToWindowsPathRef tp

fromTarPathRef :: TarPath -> FilePath
fromTarPathRef (TarPath namebs prefixbs) = adjustDirectory $
  FilePath.Native.joinPath $ FilePath.Posix.splitDirectories prefix
                          ++ FilePath.Posix.splitDirectories name
  where
    name   = BS.Char8.unpack $ posixToByteString namebs
    prefix = BS.Char8.unpack $ posixToByteString prefixbs
    adjustDirectory | FilePath.Posix.hasTrailingPathSeparator name
                    = FilePath.Native.addTrailingPathSeparator
                    | otherwise = id

fromTarPathToPosixPathRef :: TarPath -> FilePath
fromTarPathToPosixPathRef (TarPath namebs prefixbs) = adjustDirectory $
  FilePath.Posix.joinPath $ FilePath.Posix.splitDirectories prefix
                         ++ FilePath.Posix.splitDirectories name
  where
    name   = BS.Char8.unpack $ posixToByteString namebs
    prefix = BS.Char8.unpack $ posixToByteString prefixbs
    adjustDirectory | FilePath.Posix.hasTrailingPathSeparator name
                    = FilePath.Posix.addTrailingPathSeparator
                    | otherwise = id

fromTarPathToWindowsPathRef :: TarPath -> FilePath
fromTarPathToWindowsPathRef (TarPath namebs prefixbs) = adjustDirectory $
  FilePath.Windows.joinPath $ FilePath.Posix.splitDirectories prefix
                           ++ FilePath.Posix.splitDirectories name
  where
    name   = BS.Char8.unpack $ posixToByteString namebs
    prefix = BS.Char8.unpack $ posixToByteString prefixbs
    adjustDirectory | FilePath.Posix.hasTrailingPathSeparator name
                    = FilePath.Windows.addTrailingPathSeparator
                    | otherwise = id

instance (Arbitrary tarPath, Arbitrary linkTarget) => Arbitrary (GenEntry tarPath linkTarget) where
  arbitrary = do
    entryTarPath <- arbitrary
    entryContent <- arbitrary
    entryPermissions <- fromIntegral <$> (arbitrary :: Gen Word16)
    entryOwnership <- arbitrary
    entryTime <- arbitraryOctal 11
    entryFormat <- case entryContent of
      OtherEntryType 'K' _ _ -> pure GnuFormat
      OtherEntryType 'L' _ _ -> pure GnuFormat
      _ -> arbitrary
    pure Entry{..}

  shrink (Entry path content perms author time format) =
      [ Entry path' content' perms author' time' format
      | (path', content', author', time') <-
         shrink (path, content, author, time) ]
   ++ [ Entry path content perms' author time format
      | perms' <- shrinkIntegral perms ]

instance Arbitrary TarPath where
  arbitrary = either error id
            . toTarPath False
            . FilePath.Posix.joinPath
          <$> listOf1ToN (255 `div` 5)
                         (elements (map (replicate 4) "abcd"))

  shrink = map (either error id . toTarPath False)
         . map FilePath.Posix.joinPath
         . filter (not . null)
         . shrinkList shrinkNothing
         . FilePath.Posix.splitPath
         . fromTarPathToPosixPath

instance Arbitrary LinkTarget where
  arbitrary = maybe (error "link target too large") id
            . toLinkTarget
            . FilePath.Native.joinPath
          <$> listOf1ToN (100 `div` 5)
                         (elements (map (replicate 4) "abcd"))

  shrink = map (maybe (error "link target too large") id . toLinkTarget)
         . map FilePath.Posix.joinPath
         . filter (not . null)
         . shrinkList shrinkNothing
         . FilePath.Posix.splitPath
         . fromLinkTargetToPosixPath


listOf1ToN :: Int -> Gen a -> Gen [a]
listOf1ToN n g = sized $ \sz -> do
    n <- choose (1, min n (max 1 sz))
    vectorOf n g

listOf0ToN :: Int -> Gen a -> Gen [a]
listOf0ToN n g = sized $ \sz -> do
    n <- choose (0, min n sz)
    vectorOf n g

instance Arbitrary linkTarget => Arbitrary (GenEntryContent linkTarget) where
  arbitrary =
    frequency
      [ (16, do bs <- arbitrary;
                return (NormalFile bs (LBS.length bs)))
      , (2, pure Directory)
      , (1, SymbolicLink    <$> arbitrary)
      , (1, HardLink        <$> arbitrary)
      , (1, CharacterDevice <$> arbitraryOctal 7 <*> arbitraryOctal 7)
      , (1, BlockDevice     <$> arbitraryOctal 7 <*> arbitraryOctal 7)
      , (1, pure NamedPipe)
      , (1, do c  <- elements (['A'..'Z']++['a'..'z'])
               bs <- arbitrary;
               return (OtherEntryType c bs (LBS.length bs)))
      ]

  shrink (NormalFile bs _)   = [ NormalFile bs' (LBS.length bs')
                               | bs' <- shrink bs ]
  shrink  Directory          = []
  shrink (SymbolicLink link) = [ SymbolicLink link' | link' <- shrink link ]
  shrink (HardLink     link) = [ HardLink     link' | link' <- shrink link ]
  shrink (CharacterDevice ma mi) = [ CharacterDevice ma' mi'
                                   | (ma', mi') <- shrink (ma, mi) ]
  shrink (BlockDevice     ma mi) = [ BlockDevice ma' mi'
                                   | (ma', mi') <- shrink (ma, mi) ]
  shrink  NamedPipe              = []
  shrink (OtherEntryType c bs _) = [ OtherEntryType c bs' (LBS.length bs')
                                   | bs' <- shrink bs ]

instance Arbitrary LBS.ByteString where
  arbitrary = fmap LBS.pack arbitrary
  shrink    = map LBS.pack . shrink . LBS.unpack

instance Arbitrary BS.ByteString where
  arbitrary = fmap BS.pack arbitrary
  shrink    = map BS.pack . shrink . BS.unpack

instance Arbitrary Ownership where
  arbitrary = Ownership <$> name <*> name
                        <*> idno <*> idno
    where
      -- restrict user/group to posix ^[a-z][-a-z0-9]{0,30}$
      name = do
        first <- choose ('a', 'z')
        rest <- listOf0ToN 30 (oneof [choose ('a', 'z'), choose ('0', '9'), pure '-'])
        return $ first : rest
      idno = arbitraryOctal 7

  shrink (Ownership oname gname oid gid) =
    [ Ownership oname' gname' oid' gid'
    | (oname', gname', oid', gid') <- shrink (oname, gname, oid, gid) ]

instance Arbitrary Format where
  arbitrary = elements [V7Format, UstarFormat, GnuFormat]
  shrink GnuFormat = []
  shrink _ = [GnuFormat]

--arbitraryOctal :: (Integral n, Random n) => Int -> Gen n
arbitraryOctal n =
    oneof [ pure 0
          , choose (0, upperBound)
          , pure upperBound
          ]
  where
    upperBound = 8^n-1

-- For QC tests it's useful to have a way to limit the info to that which can
-- be expressed in the old V7 format
limitToV7FormatCompat :: Entry -> Entry
limitToV7FormatCompat entry@Entry { entryFormat = V7Format } =
    entry {
      entryContent = case entryContent entry of
        CharacterDevice _ _ -> OtherEntryType  '3' LBS.empty 0
        BlockDevice     _ _ -> OtherEntryType  '4' LBS.empty 0
        Directory           -> OtherEntryType  '5' LBS.empty 0
        NamedPipe           -> OtherEntryType  '6' LBS.empty 0
        other               -> other,

      entryOwnership = (entryOwnership entry) {
        groupName = "",
        ownerName = ""
      },

      entryTarPath = let TarPath name _prefix = entryTarPath entry
                      in TarPath name mempty
    }
limitToV7FormatCompat entry = entry
