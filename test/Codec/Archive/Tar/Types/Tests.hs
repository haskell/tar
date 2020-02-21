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
module Codec.Archive.Tar.Types.Tests (
  limitToV7FormatCompat
  ) where

import Codec.Archive.Tar.Types

import qualified Data.ByteString       as BS
import qualified Data.ByteString.Lazy  as LBS

import qualified System.FilePath as FilePath.Native
         ( joinPath, splitDirectories, addTrailingPathSeparator )
import qualified System.FilePath.Posix as FilePath.Posix
         ( joinPath, splitPath, splitDirectories, hasTrailingPathSeparator
         , addTrailingPathSeparator )

import Test.QuickCheck
import Control.Applicative ((<$>), (<*>), pure)
import Data.Word (Word16)

instance Arbitrary Entry where
  arbitrary = Entry <$> arbitrary <*> arbitrary <*> arbitraryPermissions
                    <*> arbitrary <*> arbitraryEpochTime <*> arbitrary
    where
      arbitraryPermissions :: Gen Permissions
      arbitraryPermissions = fromIntegral <$> (arbitrary :: Gen Word16)

      arbitraryEpochTime :: Gen EpochTime
      arbitraryEpochTime = arbitraryOctal 11

  shrink (Entry path content perms author time format) =
      [ Entry path' content' perms author' time' format
      | (path', content', author', time') <-
         shrink (path, content, author, time) ]
   ++ [ Entry path content perms' author time format
      | perms' <- shrinkIntegral perms ]

instance Arbitrary TarPath where
  arbitrary = these (error . show) id (flip const)
            . toTarPath False
            . FilePath.Posix.joinPath
          <$> listOf1ToN (255 `div` 5)
                         (elements (map (replicate 4) "abcd"))

  shrink = map (these (error . show) id (flip const) . toTarPath False)
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

instance Arbitrary EntryContent where
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
                      in TarPath name BS.empty
    }
limitToV7FormatCompat entry = entry
