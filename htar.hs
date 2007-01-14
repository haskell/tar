module Main where

import Codec.Archive.Tar

import Data.Binary

import Data.Bits
import System.Environment
import System.Exit
import System.IO


main :: IO ()
main = do args <- getArgs
          ma <- decodeFile $ head args
          case ma of
            Left err -> do hPutStrLn stderr $ show err
                           exitFailure
            Right a  -> putStr $ archiveFileInfo a

-- * Formatted information about archives

archiveHeaders :: TarArchive -> [TarHeader]
archiveHeaders = map entryHeader . archiveEntries

archiveFileNames :: TarArchive -> String
archiveFileNames = unlines . map tarFileName . archiveHeaders

archiveFileInfo :: TarArchive -> String
archiveFileInfo = unlines . map fileInfo . archiveHeaders
  where fileInfo hdr = unwords [typ ++ mode, owner, group, size, time, file] -- FIXME: nice padding
          where typ = case tarFileType hdr of
                        TarSymLink  -> "l"
                        TarCharDev  -> "c"
                        TarBlockDev -> "b"
                        TarDir      -> "d"
                        TarFIFO     -> "p"
                        _           -> "-"
                mode = concat [u,g,o] -- FIXME: handle setuid etc.
                  where m = tarFileMode hdr 
                        f x = [t 2 'r', t 1 'w', t 0 'x']
                          where t n c = if testBit x n then c else '-'
                        u = f (m `shiftR` 6)
                        g = f (m `shiftR` 3)
                        o = f m
                owner = let name = tarOwnerName hdr in if null name then show (tarOwnerID hdr) else name
                group = let name = tarGroupName hdr in if null name then show (tarGroupID hdr) else name
                size = show (tarFileSize hdr)
                time = show (tarModTime hdr)
                file = tarFileName hdr
