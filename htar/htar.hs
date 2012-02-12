module Main where

import qualified Codec.Archive.Tar       as Tar
import qualified Codec.Archive.Tar.Entry as Tar

import qualified Codec.Compression.GZip as GZip (compress, decompress)
import qualified Codec.Compression.BZip as BZip (compress, decompress)

import Control.Exception     (throwIO)
import qualified Data.ByteString.Lazy as BS
import Data.ByteString.Lazy  (ByteString)
import Data.Bits             (testBit)
import Data.Char             (toUpper)
import System.Console.GetOpt (OptDescr(..), ArgDescr(..), ArgOrder(..),
                              getOpt', usageInfo)
import System.Environment    (getArgs)
import System.Exit           (exitFailure)
import System.IO             (hPutStrLn, stderr)
import System.Locale         (defaultTimeLocale)
import System.Time           (ClockTime(..), toUTCTime, formatCalendarTime)

main :: IO ()
main = do
  (opts, files) <- parseOptions =<< getArgs
  main' opts files

main' :: Options -> [FilePath] -> IO ()
main' (Options { optFile        = file,
                 optDir         = dir,
                 optAction      = action,
                 optCompression = compression,
                 optVerbosity   = verbosity }) files =
  case action of
    NoAction -> die ["No action given. Specify one of -c, -t or -x."]
    Help     -> printUsage
    Create   -> output . compress compression
                       . Tar.write =<< Tar.pack dir files
    Extract  -> Tar.unpack dir . Tar.read . decompress compression =<< input
    List     -> printEntries . Tar.read . decompress compression =<< input
  where
    input  = if file == "-" then BS.getContents else BS.readFile  file
    output = if file == "-" then BS.putStr      else BS.writeFile file

    printEntries = Tar.foldEntries (\entry rest -> printEntry entry >> rest)
                                   (return ()) throwIO
    printEntry = putStrLn . entryInfo verbosity

data Compression = None | GZip | BZip
  deriving Show

compress :: Compression -> ByteString -> ByteString
compress None = id
compress GZip = GZip.compress
compress BZip = BZip.compress

decompress :: Compression -> ByteString -> ByteString
decompress None = id
decompress GZip = GZip.decompress
decompress BZip = BZip.decompress

data Verbosity = Verbose | Concise

------------------------
-- List archive contents

entryInfo :: Verbosity -> Tar.Entry -> String
entryInfo Verbose = detailedInfo
entryInfo Concise = Tar.entryPath

detailedInfo :: Tar.Entry -> String
detailedInfo entry =
  unwords [ typeCode : permissions
          , justify 19 (owner ++ '/' : group) size
          , time
          , name ++ link ]
  where
    typeCode = case Tar.entryContent entry of
      Tar.HardLink        _   -> 'h'
      Tar.SymbolicLink    _   -> 'l'
      Tar.CharacterDevice _ _ -> 'c'
      Tar.BlockDevice     _ _ -> 'b'
      Tar.Directory           -> 'd'
      Tar.NamedPipe           -> 'p'
      _                       -> '-'
    permissions = concat [userPerms, groupPerms, otherPerms]
      where
        userPerms  = formatPerms 8 7 6 11 's'
        groupPerms = formatPerms 5 4 3 10 's'
        otherPerms = formatPerms 2 1 0  9 't'
        formatPerms r w x s c =
          [if testBit m r then 'r' else '-'
          ,if testBit m w then 'w' else '-'
          ,if testBit m s
             then if testBit m x then c   else toUpper c
             else if testBit m x then 'x' else '-']
        m = Tar.entryPermissions entry
    owner = nameOrID ownerName ownerId
    group = nameOrID groupName groupId
    (Tar.Ownership ownerName groupName ownerId groupId) =
      Tar.entryOwnership entry
    nameOrID n i = if null n then show i else n
    size = case Tar.entryContent entry of
             Tar.NormalFile _ fileSize -> show fileSize
             _                         -> "0"

    time = formatEpochTime "%Y-%m-%d %H:%M" (Tar.entryTime entry)
    name = Tar.entryPath entry
    link = case Tar.entryContent entry of
      Tar.HardLink     l -> " link to " ++ Tar.fromLinkTarget l
      Tar.SymbolicLink l -> " -> "      ++ Tar.fromLinkTarget l
      _                  -> ""

justify :: Int -> String -> String -> String
justify width left right = left ++ padding ++ right
  where
    padding  = replicate padWidth ' '
    padWidth = max 1 (width - length left - length right)

formatEpochTime :: String -> Tar.EpochTime -> String
formatEpochTime f =
    formatCalendarTime defaultTimeLocale f . toUTCTime . epochTimeToClockTime

epochTimeToClockTime :: Tar.EpochTime -> ClockTime
epochTimeToClockTime e = TOD s (truncate (1000000000 * f))
    where (s,f) = properFraction (toRational e)

------------------------
-- Command line handling

data Options = Options {
    optFile        :: FilePath, -- "-" means stdin/stdout
    optDir         :: FilePath,
    optAction      :: Action,
    optCompression :: Compression,
    optVerbosity   :: Verbosity
  }

defaultOptions :: Options
defaultOptions = Options {
    optFile        = "-",
    optDir         = "",
    optAction      = NoAction,
    optCompression = None,
    optVerbosity   = Concise
  }

data Action = NoAction
            | Help
            | Create
            | Extract
            | List
  deriving Show

optDescr :: [OptDescr (Options -> Options)]
optDescr =
  [ Option ['c'] ["create"]
      (action Create)
      "Create a new archive."
  , Option ['x'] ["extract", "get"]
      (action Extract)
      "Extract files from an archive."
  , Option ['t'] ["list"]
      (action List)
      "List the contents of an archive."
  , Option ['f'] ["file"]
      (ReqArg (\f o -> o { optFile = f}) "ARCHIVE")
      "Use archive file ARCHIVE."
  , Option ['C'] ["directory"]
      (ReqArg (\d o -> o { optDir = d }) "DIR")
      "Create or extract relative to DIR."
  , Option ['z'] ["gzip", "gunzip", "ungzip"]
      (compression GZip)
      "Use gzip compression."
  , Option ['j'] ["bzip2"]
      (compression BZip)
      "Use bzip2 compression."
  , Option ['v'] ["verbose"]
      (NoArg (\o -> o { optVerbosity = Verbose }))
      "Verbosely list files processed."
  , Option ['h', '?'] ["help"]
      (action Help)
      "Print this help output."
  ]
  where
    action      a = NoArg (\o -> o { optAction = a })
    compression c = NoArg (\o -> o { optCompression = c })

printUsage :: IO ()
printUsage = putStrLn (usageInfo headder optDescr)
  where
    headder = unlines ["htar creates and extracts TAR archives.",
                       "",
                       "Usage: htar [OPTION ...] [FILE ...]"]

parseOptions :: [String] -> IO (Options, [FilePath])
parseOptions args =
  let (fs, files, nonopts, errors) = getOpt' Permute optDescr args
  in case (nonopts, errors) of
       ([], [])    -> return $ (foldl (flip ($)) defaultOptions fs, files)
       (_ , (_:_)) -> die errors
       (_ ,  _)    -> die (map (("unrecognized option "++).show) nonopts)

die :: [String] -> IO a
die errs = do
  mapM_ (\e -> hPutStrLn stderr $ "htar: " ++ e) $ errs
  hPutStrLn stderr "Try `htar --help' for more information."
  exitFailure
