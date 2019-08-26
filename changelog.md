See also http://pvp.haskell.org/faq

0.5.1.1 Herbert Valerio Riedel <hvr@gnu.org> March 2018

  * Add support for GHC 8.8.1 / base-4.13

0.5.1.0 Herbert Valerio Riedel <hvr@gnu.org> March 2018

  * Add support for GHC 8.4.1 / base-4.11
  * Add `Semigroup` instance for `Entries`

0.5.0.3 Duncan Coutts <duncan@community.haskell.org> May 2016

  * Fix tarbomb logic to ignore special PAX entries. Was breaking many
    valid tarballs. https://github.com/haskell/cabal/issues/3390

0.5.0.2 Duncan Coutts <duncan@community.haskell.org> April 2016

  * Fix compatability when using ghc-7.4.x and directory >= 1.2.3

0.5.0.1 Duncan Coutts <duncan@community.haskell.org> January 2016

  * Fix compatability with directory-1.2.3+

0.5.0.0 Duncan Coutts <duncan@community.haskell.org> January 2016

  * Work with old version of bytestring (using bytestring-builder package).
  * Builds with GHC 6.10 -- 8.0.
  * Change type of Index.serialise to be simply strict bytestring.
  * Preserve file timestamps on unpack (with directory-1.2.3+)

0.4.5.0 Duncan Coutts <duncan@community.haskell.org> January 2016

  * Revert accidental minor API change in 0.4.x series (the type of the
    owner and group name strings). The 0.4.3.0 and 0.4.4.0 releases
    contained the accidental API change.
  * Add a handy foldlEntries function

0.4.4.0 Duncan Coutts <duncan@community.haskell.org> January 2016

  * Build and warning fixes for GHC 7.10 and 8.0
  * New Index module function `toList` to get all index entries

0.4.3.0 Duncan Coutts <duncan@community.haskell.org> January 2016

  * New Index function `unfinalise` to extend existing index
  * 9x  faster reading
  * 9x  faster index construction
  * 24x faster index extension
  * More compact entry types, using ByteStrings
  * More Eq and Show instances
  * Greater QC test coverage
  * Fix minor bug in reading non-standard v7 format entries

0.4.2.2 Edsko de Vries <edsko@well-typed.com> October 2015

  * Fix bug in Index

0.4.2.1 Duncan Coutts <duncan@community.haskell.org> July 2015

  * Fix tests for the Index modules (the code was right)

0.4.2.0 Duncan Coutts <duncan@community.haskell.org> July 2015

  * New Index module for random access to tar file contents
  * New lower level tar file I/O actions
  * New tarball file 'append' action

0.4.1.0 Duncan Coutts <duncan@community.haskell.org> January 2015

  * Build with GHC 7.10
  * Switch from old-time to time package
  * Added more instance for Entries type

0.4.0.1 Duncan Coutts <duncan@community.haskell.org> October 2012

  * fixes to work with directory 1.2
  * More Eq/Ord instances

0.4.0.0 Duncan Coutts <duncan@community.haskell.org> February 2012

  * More explicit error types and error handling
  * Support star base-256 number format
  * Improved API documentation
