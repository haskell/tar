## 0.6.3.0 Bodigrim <andrew.lelechenko@gmail.com> June 2024

  * [Speed up `deserialize`](https://github.com/haskell/tar/pull/95).

## 0.6.2.0 Bodigrim <andrew.lelechenko@gmail.com> March 2024

  * Fix issues with Unicode support in filenames.

## 0.6.1.0 Bodigrim <andrew.lelechenko@gmail.com> January 2024

  * Support Unicode in filenames (encoded as UTF-8).
  * Reduce peak memory consumption when unpacking large files.

## 0.6.0.0 Bodigrim <andrew.lelechenko@gmail.com> December 2023

  This release features support for long file paths and symlinks
  (thanks to Julian Ospald) and variety of changes and improvements
  across entire package, fixing multiple causes of silent data corruption.

  Breaking changes:

  * Generalize `Entries`, `Entry` and `EntryContent` to `GenEntries`, `GenEntry` and `GenEntryContent`.
    * Functions working on entries have been generalized to more polymorphic types,
      where possible.
    * Modules which used to `import Codec.Archive.Tar (Entry(..))` should now
      `import Codec.Archive.Tar (Entry, pattern Entry)` and similar for other `Gen`-types.
      Another option is to import the entire module qualified.
  * Redesign `Codec.Archive.Tar.Check`.
    * Change types of `checkSecurity`, `checkTarbomb`, `checkPortability`.
    * Add offending path as new field to `TarBombError` constructor.
    * Extend `FileNameError` with `UnsafeLinkTarget` constructor.
  * Drop deprecated `emptyIndex` and `finaliseIndex`.

  Examples of migration:

  * [`hackage-security`](https://github.com/haskell/hackage-security/commit/24693ce115c9769fe3c6ec9ca1d137d14d0d27ff)
  * [`archive-backpack`](https://github.com/vmchale/archive-backpack/commit/4b3d1bdff15fcf044d6171ca649a930c775d491b)
  * [`keter`](https://github.com/snoyberg/keter/commit/20a33d9276d5781ca6993b857d8d097085983ede)
  * [`libarchive`](https://github.com/vmchale/libarchive/commit/c0e101fede924a6e12f1d726587626c48444e65d)
  * [`cabal-install`](https://github.com/haskell/cabal/commit/51e6483f95ecb4f395dce36e47af296902a75143)
  * [`ghcup`](https://github.com/haskell/ghcup-hs/commit/6ae312c1f9dd054546e4afe4c969c37cd54b09a9)
  * [`hackage-server`](https://github.com/haskell/hackage-server/commit/6b71d1659500aba50b6a1e48aa53039046720af8)

  Bug fixes:

  * Add support for over-long filepaths via GNU extension.
    * Now `entryPath` corresponds to an internal, low-level path, limited
      to 255 characters. To list filenames properly use `decodeLongNames`,
      followed by `entryTarPath`.
  * Fix handling of hardlinks and symlinks.
  * Handle > 8 GB files insted of silent corruption.
  * Prohibit non-ASCII file names instead of silent corruption.
  * Set permissions on extracted files.
  * Ignore FAT32 errors when setting modification time.
  * Switch to trailer parsing mode only after a full block of `NUL`.

  New API:

  * Add `Traversable Entries` instance.
  * Add `toTarPath'`, `ToTarPathResult`, `longLinkEntry`, `longSymLinkEntry`.
  * Add `packSymlinkEntry` and `symbolicLinkPermission`.
  * Add `packAndCheck` and `unpackAndCheck`.
  * Add `checkEntrySecurity`, `checkEntryTarbomb` and `checkEntryPortability`.
  * Add `encodeLongNames`, `decodeLongNames`, `DecodeLongNamesError`.

  Improvements:

  * Speed up `fromTarPath`, `fromTarPathToPosixPath` and `fromTarPathToWindowsPath`.
  * Alleviate leakage of file handles in `packFileEntry`.
  * Fix tests on 32-bit architectures.

## 0.5.1.1 Herbert Valerio Riedel <hvr@gnu.org> August 2019

  * Add support for GHC 8.8.1 / base-4.13

## 0.5.1.0 Herbert Valerio Riedel <hvr@gnu.org> March 2018

  * Add support for GHC 8.4.1 / base-4.11
  * Add `Semigroup` instance for `Entries`

## 0.5.0.3 Duncan Coutts <duncan@community.haskell.org> May 2016

  * Fix tarbomb logic to ignore special PAX entries. Was breaking many
    valid tarballs. https://github.com/haskell/cabal/issues/3390

## 0.5.0.2 Duncan Coutts <duncan@community.haskell.org> April 2016

  * Fix compatability when using ghc-7.4.x and directory >= 1.2.3

## 0.5.0.1 Duncan Coutts <duncan@community.haskell.org> January 2016

  * Fix compatability with directory-1.2.3+

## 0.5.0.0 Duncan Coutts <duncan@community.haskell.org> January 2016

  * Work with old version of bytestring (using bytestring-builder package).
  * Builds with GHC 6.10 -- 8.0.
  * Change type of Index.serialise to be simply strict bytestring.
  * Preserve file timestamps on unpack (with directory-1.2.3+)

## 0.4.5.0 Duncan Coutts <duncan@community.haskell.org> January 2016

  * Revert accidental minor API change in 0.4.x series (the type of the
    owner and group name strings). The 0.4.3.0 and 0.4.4.0 releases
    contained the accidental API change.
  * Add a handy foldlEntries function

## 0.4.4.0 Duncan Coutts <duncan@community.haskell.org> January 2016

  * Build and warning fixes for GHC 7.10 and 8.0
  * New Index module function `toList` to get all index entries

## 0.4.3.0 Duncan Coutts <duncan@community.haskell.org> January 2016

  * New Index function `unfinalise` to extend existing index
  * 9x  faster reading
  * 9x  faster index construction
  * 24x faster index extension
  * More compact entry types, using ByteStrings
  * More Eq and Show instances
  * Greater QC test coverage
  * Fix minor bug in reading non-standard v7 format entries

## 0.4.2.2 Edsko de Vries <edsko@well-typed.com> October 2015

  * Fix bug in Index

## 0.4.2.1 Duncan Coutts <duncan@community.haskell.org> July 2015

  * Fix tests for the Index modules (the code was right)

## 0.4.2.0 Duncan Coutts <duncan@community.haskell.org> July 2015

  * New Index module for random access to tar file contents
  * New lower level tar file I/O actions
  * New tarball file 'append' action

## 0.4.1.0 Duncan Coutts <duncan@community.haskell.org> January 2015

  * Build with GHC 7.10
  * Switch from old-time to time package
  * Added more instance for Entries type

## 0.4.0.1 Duncan Coutts <duncan@community.haskell.org> October 2012

  * fixes to work with directory 1.2
  * More Eq/Ord instances

## 0.4.0.0 Duncan Coutts <duncan@community.haskell.org> February 2012

  * More explicit error types and error handling
  * Support star base-256 number format
  * Improved API documentation
