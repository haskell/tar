# tar [![Hackage](https://img.shields.io/hackage/v/tar.svg)](https://hackage.haskell.org/package/tar)

This library is for working with `.tar` archive files. It can read and write a range of common variations of archive format including V7, POSIX USTAR and GNU formats. It provides support for packing and unpacking portable archives and features for random access to archive content using an index.

For a quick start with the API look at `htar/htar.hs`,
which implements a very basic `tar` command-line tool.

To run benchmarks download [`01-index.tar`](https://hackage.haskell.org/01-index.tar) into the package folder:

```sh
wget https://hackage.haskell.org/01-index.tar
cabal bench
```
