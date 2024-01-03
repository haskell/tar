{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PackageImports #-}
{-# OPTIONS_HADDOCK hide #-}

module Codec.Archive.Tar.LongNames
  ( encodeLongNames
  , decodeLongNames
  , DecodeLongNamesError(..)
  ) where

import Codec.Archive.Tar.PackAscii
import Codec.Archive.Tar.Types
import Control.Exception
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import "os-string" System.OsString.Posix (PosixString, PosixChar)
import qualified "os-string" System.OsString.Posix as PS

-- | Errors raised by 'decodeLongNames'.
--
-- @since 0.6.0.0
data DecodeLongNamesError
  = TwoTypeKEntries
  -- ^ Two adjacent 'OtherEntryType' @\'K\'@ nodes.
  | TwoTypeLEntries
  -- ^ Two adjacent 'OtherEntryType' @\'L\'@ nodes.
  | NoLinkEntryAfterTypeKEntry
  -- ^ 'OtherEntryType' @\'K\'@ node is not followed by a 'SymbolicLink' / 'HardLink'.
  deriving (Eq, Ord, Show)

instance Exception DecodeLongNamesError

-- | Translate high-level entries with POSIX 'FilePath's for files and symlinks
-- into entries suitable for serialization by emitting additional
-- 'OtherEntryType' @\'K\'@ and 'OtherEntryType' @\'L\'@ nodes.
--
-- Input 'FilePath's must be POSIX file names, not native ones.
--
-- @since 0.6.0.0
encodeLongNames
  :: GenEntry FilePath FilePath
  -> [Entry]
encodeLongNames e = maybe id (:) mEntry $ maybe id (:) mEntry' [e'']
  where
    (mEntry, e') = encodeLinkTarget e
    (mEntry', e'') = encodeTarPath e'

encodeTarPath
  :: GenEntry FilePath linkTarget
  -> (Maybe (GenEntry TarPath whatever), GenEntry TarPath linkTarget)
  -- ^ (LongLink entry, actual entry)
encodeTarPath e = case toTarPath' (entryTarPath e) of
  FileNameEmpty -> (Nothing, e { entryTarPath = TarPath mempty mempty })
  FileNameOK tarPath -> (Nothing, e { entryTarPath = tarPath })
  FileNameTooLong tarPath -> (Just $ longLinkEntry $ entryTarPath e, e { entryTarPath = tarPath })

encodeLinkTarget
  :: GenEntry tarPath FilePath
  -> (Maybe (GenEntry TarPath LinkTarget), GenEntry tarPath LinkTarget)
  -- ^ (LongLink symlink entry, actual entry)
encodeLinkTarget e = case entryContent e of
  NormalFile x y -> (Nothing, e { entryContent = NormalFile x y })
  Directory -> (Nothing, e { entryContent = Directory })
  SymbolicLink lnk -> let (mEntry, lnk') = encodeLinkPath lnk in
    (mEntry, e { entryContent = SymbolicLink lnk' })
  HardLink lnk -> let (mEntry, lnk') = encodeLinkPath lnk in
    (mEntry, e { entryContent = HardLink lnk' })
  CharacterDevice x y -> (Nothing, e { entryContent = CharacterDevice x y })
  BlockDevice x y -> (Nothing, e { entryContent = BlockDevice x y })
  NamedPipe -> (Nothing, e { entryContent = NamedPipe })
  OtherEntryType x y z -> (Nothing, e { entryContent = OtherEntryType x y z })

encodeLinkPath
  :: FilePath
  -> (Maybe (GenEntry TarPath LinkTarget), LinkTarget)
encodeLinkPath lnk = case toTarPath' lnk of
  FileNameEmpty -> (Nothing, LinkTarget mempty)
  FileNameOK (TarPath name prefix)
    | PS.null prefix -> (Nothing, LinkTarget name)
    | otherwise -> (Just $ longSymLinkEntry lnk, LinkTarget name)
  FileNameTooLong (TarPath name _) ->
    (Just $ longSymLinkEntry lnk, LinkTarget name)

-- | Translate low-level entries (usually freshly deserialized) into
-- high-level entries with POSIX 'FilePath's for files and symlinks
-- by parsing and eliminating
-- 'OtherEntryType' @\'K\'@ and 'OtherEntryType' @\'L\'@ nodes.
--
-- Resolved 'FilePath's are still POSIX file names, not native ones.
--
-- @since 0.6.0.0
decodeLongNames
  :: Entries e
  -> GenEntries FilePath FilePath (Either e DecodeLongNamesError)
decodeLongNames = go Nothing Nothing
  where
    go :: Maybe FilePath -> Maybe FilePath -> Entries e -> GenEntries FilePath FilePath (Either e DecodeLongNamesError)
    go _ _ (Fail err) = Fail (Left err)
    go _ _ Done = Done

    go Nothing Nothing (Next e rest) = case entryContent e of
      OtherEntryType 'K' fn _ ->
        go (Just (otherEntryPayloadToFilePath fn)) Nothing rest
      OtherEntryType 'L' fn _ ->
        go Nothing (Just (otherEntryPayloadToFilePath fn)) rest
      _ ->
        Next (castEntry e) (go Nothing Nothing rest)

    go Nothing (Just path) (Next e rest) = case entryContent e of
      OtherEntryType 'K' fn _ ->
        go (Just (otherEntryPayloadToFilePath fn)) (Just path) rest
      OtherEntryType 'L' _ _ ->
        Fail $ Right TwoTypeLEntries
      _ -> Next ((castEntry e) { entryTarPath = path }) (go Nothing Nothing rest)

    go (Just link) Nothing (Next e rest) = case entryContent e of
      OtherEntryType 'K' _ _ ->
        Fail $ Right TwoTypeKEntries
      OtherEntryType 'L' fn _ ->
        go (Just link) (Just (otherEntryPayloadToFilePath fn)) rest
      SymbolicLink{} ->
        Next ((castEntry e) { entryContent = SymbolicLink link }) (go Nothing Nothing rest)
      HardLink{} ->
        Next ((castEntry e) { entryContent = HardLink link }) (go Nothing Nothing rest)
      _ ->
        Fail $ Right NoLinkEntryAfterTypeKEntry

    go (Just link) (Just path) (Next e rest) = case entryContent e of
      OtherEntryType 'K' _ _ ->
        Fail $ Right TwoTypeKEntries
      OtherEntryType 'L' _ _ ->
        Fail $ Right TwoTypeLEntries
      SymbolicLink{} ->
        Next ((castEntry e) { entryTarPath = path, entryContent = SymbolicLink link }) (go Nothing Nothing rest)
      HardLink{} ->
        Next ((castEntry e) { entryTarPath = path, entryContent = HardLink link }) (go Nothing Nothing rest)
      _ ->
        Fail $ Right NoLinkEntryAfterTypeKEntry

otherEntryPayloadToFilePath :: BL.ByteString -> FilePath
otherEntryPayloadToFilePath =
  fromPosixString . byteToPosixString . B.takeWhile (/= '\0') . BL.toStrict

castEntry :: Entry -> GenEntry FilePath FilePath
castEntry e = e
  { entryTarPath = fromTarPathToPosixPath (entryTarPath e)
  , entryContent = castEntryContent (entryContent e)
  }

castEntryContent :: EntryContent -> GenEntryContent FilePath
castEntryContent = \case
  NormalFile x y -> NormalFile x y
  Directory -> Directory
  SymbolicLink linkTarget -> SymbolicLink $ fromLinkTargetToPosixPath linkTarget
  HardLink linkTarget -> HardLink $ fromLinkTargetToPosixPath linkTarget
  CharacterDevice x y -> CharacterDevice x y
  BlockDevice x y -> BlockDevice x y
  NamedPipe -> NamedPipe
  OtherEntryType x y z -> OtherEntryType x y z
