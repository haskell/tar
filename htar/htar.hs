module Main where

import Codec.Archive.Tar

import Data.Binary

import qualified Data.ByteString.Lazy as BS
import Data.ByteString.Lazy (ByteString)
import Control.Monad
import Data.Bits
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO

main :: IO ()
main = do args <- getArgs
          (opts, files) <- parseOptions args
          mainOpts opts files

parseOptions :: [String] -> IO (Options, [FilePath])
parseOptions args = 
   do let (fs, files, nonopts, errs) = getOpt' RequireOrder optDescr args
      when (not (null errs)) $ die errs
      case nonopts of
        []         -> return $ (foldl (flip ($)) defaultOptions fs, files)
        ["--help"] -> usage
        _          -> die (map (("unrecognized option "++).show) nonopts)

mainOpts :: Options -> [FilePath] -> IO ()
mainOpts (Options { optAction = Nothing }) files 
    = die ["No action given. Specify one of -c, -t or -x."]
mainOpts (Options { optFile = file, optAction = Just action }) files = 
    -- FIXME: catch errors and print out nicely
    case action of 
      Create  -> createTarData files >>= write
      -- FIXME: extract only the given files
      Extract -> read >>= extractTarData
      -- FIXME: list only the given files
      List    -> read >>= decodeOrFail >>= putStr . archiveFileInfo
  where read  = if file == "-" then BS.getContents else BS.readFile file
        write = if file == "-" then BS.putStr      else BS.writeFile file

decodeOrFail :: (Monad m, Binary a) => ByteString -> m a
decodeOrFail = either (fail . show) return . decode

die :: [String] -> IO a
die errs = do mapM_ (\e -> hPutStrLn stderr $ "htar: " ++ e) $ errs
              hPutStrLn stderr "Try `htar --help' for more information."
              exitFailure

usage :: IO a
usage = do putStrLn (usageInfo hdr optDescr)
           exitWith ExitSuccess
  where hdr = unlines ["htar can create and extract file archives.",
                       "",
                       "Usage: htar "]

-- * Options

data Options = Options 
    {
     optFile :: FilePath, -- "-" means stdin/stdout
     optAction :: Maybe Action
    }
 deriving Show

data Action = Create
            | Extract
            | List
  deriving Show

defaultOptions :: Options
defaultOptions = Options {
                          optFile = "-",
                          optAction = Nothing
                         }

optDescr :: [OptDescr (Options -> Options)]
optDescr = 
    [
     Option ['c'] ["create"] (action Create) "Create a new archive.",
     Option ['x'] ["extract","get"] (action Extract) "Extract files.",
     Option ['t'] ["list"] (action List) "List archive contents.",
     Option ['f'] ["file"] (ReqArg (\f o -> o { optFile = f}) "ARCHIVE")
            "Use archive file ARCHIVE."
    ]
 where action a = NoArg (\o -> o { optAction = Just a })

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
