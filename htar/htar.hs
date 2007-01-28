module Main where

import Codec.Archive.Tar

import Codec.Compression.GZip.GUnZip (gunzip)

import qualified Data.ByteString.Lazy as BS
import Data.ByteString.Lazy (ByteString)
import Control.Monad
import Data.Bits
import qualified Data.Set as Set
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
mainOpts (Options { optAction = Nothing }) _ 
    = die ["No action given. Specify one of -c, -t or -x."]
mainOpts (Options { optFile = file, 
                    optAction = Just action,
                    optCompression = compression, 
                    optVerbose = verbose }) files = 
    -- FIXME: catch errors and print out nicely
    case action of 
      Create  -> recurseDirectories files 
                 >>= mapM (createEntry verbose) 
                 >>= output . compress compression 
                         . writeTarArchive . TarArchive
      Extract -> inputEntries >>= mapM_ (extractEntry verbose)
      List    -> inputEntries >>= mapM_ (putStrLn . entryInfo verbose)
  where input  = if file == "-" then BS.getContents else BS.readFile file
        output = if file == "-" then BS.putStr      else BS.writeFile file
        inputEntries = input >>= return . readEntries files . decompress compression

compress :: Maybe Compression -> ByteString -> ByteString
compress Nothing      = id
compress (Just GZip)  = error "gzip compression is not supported yet"
compress (Just BZip2) = error "bzip2 compression is not supported yet"

decompress :: Maybe Compression -> ByteString -> ByteString
decompress Nothing      = id
decompress (Just GZip)  = gunzip
decompress (Just BZip2) = error "bzip2 decompression is not supported yet"

readEntries :: [FilePath] -> ByteString -> [TarEntry]
readEntries files = (if null files then id else keepFiles files) 
                    . archiveEntries . readTarArchive

-- FIXME: allow files names to differ in trailing slashes
keepFiles :: [FilePath] -> [TarEntry] -> [TarEntry]
keepFiles files = filter (p . entryHeader)
  where p = ((`Set.member` Set.fromList files) . tarFileName)

createEntry :: Bool -> FilePath -> IO TarEntry
createEntry verbose file =
    do when verbose $ putStrLn file
       createTarEntry file

extractEntry :: Bool -> TarEntry -> IO ()
extractEntry verbose e =
    do when verbose $ putStrLn $ tarFileName $ entryHeader e
       extractTarEntry e

die :: [String] -> IO a
die errs = do mapM_ (\e -> hPutStrLn stderr $ "htar: " ++ e) $ errs
              hPutStrLn stderr "Try `htar --help' for more information."
              exitFailure

usage :: IO a
usage = do putStrLn (usageInfo hdr optDescr)
           exitWith ExitSuccess
  where hdr = unlines ["htar creates and extract file archives.",
                       "",
                       "Usage: htar [OPTION ...] [FILE ...]"]

-- * Options

data Options = Options 
    {
     optFile :: FilePath, -- "-" means stdin/stdout
     optAction :: Maybe Action,
     optCompression :: Maybe Compression,
     optVerbose :: Bool
    }
 deriving Show

data Action = Create
            | Extract
            | List
  deriving Show

data Compression = GZip | BZip2
  deriving Show

defaultOptions :: Options
defaultOptions = Options {
                          optFile = "-",
                          optAction = Nothing,
                          optCompression = Nothing,
                          optVerbose = False
                         }

optDescr :: [OptDescr (Options -> Options)]
optDescr = 
    [
     Option ['c'] ["create"] (action Create) "Create a new archive.",
     Option ['x'] ["extract","get"] (action Extract) "Extract files.",
     Option ['t'] ["list"] (action List) "List archive contents.",
     Option ['z'] ["gzip","ungzip"] (compression GZip) "Use gzip compression.",
     Option ['j'] ["bzip2"] (compression BZip2) "Use bzip2 compression.",
     Option ['f'] ["file"] (ReqArg (\f o -> o { optFile = f}) "ARCHIVE")
            "Use archive file ARCHIVE.",
     Option ['v'] ["verbose"] (NoArg (\o -> o { optVerbose = True }))
            "Increase output verbosity."
    ]
 where action a = NoArg (\o -> o { optAction = Just a })
       compression c = NoArg (\o -> o { optCompression = Just c })

-- * Formatted information about archives

entryInfo :: Bool -- ^ Verbosity
          -> TarEntry -> String
entryInfo True  = detailedInfo . entryHeader
entryInfo False = tarFileName  . entryHeader

detailedInfo :: TarHeader -> String
detailedInfo hdr =
    unwords [typ ++ mode, owner, group, size, time, name] -- FIXME: nice padding
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
          owner = nameOrID (tarOwnerName hdr) (tarOwnerID hdr)
          group = nameOrID (tarGroupName hdr) (tarGroupID hdr)
          nameOrID n i = if null n then show i else n
          size = show (tarFileSize hdr)
          time = show (tarModTime hdr)
          name = tarFileName hdr
