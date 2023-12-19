#!/bin/sh
# Test that htar is capable to pack and unpack large files,
# without materialising them in memory in full.

set -eux
cabal build htar
HTAR=$(cabal list-bin htar)
cd "$(mktemp -d)"
mkdir input
for i in $(seq 0 4); do
  dd if=/dev/zero of="input/$i.txt" bs=1M count=2048
done;
$HTAR --create --verbose --file input.tar.gz input +RTS -s -M50M
rm -rf input
$HTAR --extract --verbose --file input.tar.gz +RTS -s -M50M
ls -l input
