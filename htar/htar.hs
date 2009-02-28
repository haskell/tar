module Main where

import qualified Codec.Archive.Tar       as Tar
import qualified Codec.Archive.Tar.Entry as Tar

import qualified Codec.Compression.GZip as GZip (compress, decompress)
import qualified Codec.Compression.BZip as BZip (compress, decompress)

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
                 optAction      = action,
                 optCompression = compression,
                 optVerbosity   = verbosity }) files =
  case action of
    NoAction -> die ["No action given. Specify one of -c, -t or -x."]
    Help     -> printUsage
    Create   -> output . compress compression
                       . Tar.write =<< Tar.pack "." (head files)
                       --FIXME: don't use head here, allow all files
    Extract  -> Tar.unpack "." . Tar.read . decompress compression =<< input
    List     -> printEntries . Tar.read . decompress compression =<< input
  where
    input  = if file == "-" then BS.getContents else BS.readFile  file
    output = if file == "-" then BS.putStr      else BS.writeFile file

    printEntries = Tar.foldEntries (\entry rest -> printEntry entry >> rest)
                                   (return ()) fail
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
entryInfo Concise = Tar.fileName

detailedInfo :: Tar.Entry -> String
detailedInfo entry =
  unwords [ typeCode : permissions
          , justify 19 (owner ++ '/' : group) size
          , time
          , name ++ link ]
  where
    typeCode = case Tar.fileType entry of
      Tar.HardLink        -> 'h'
      Tar.SymbolicLink    -> 'l'
      Tar.CharacterDevice -> 'c'
      Tar.BlockDevice     -> 'b'
      Tar.Directory       -> 'd'
      Tar.FIFO            -> 'p'
      _                   -> '-'
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
        m = Tar.fileMode entry
    owner = nameOrID ownerName (Tar.ownerId entry)
    group = nameOrID groupName (Tar.groupId entry)
    ownerName = case Tar.headerExt entry of
      Tar.UstarHeader { Tar.ownerName = n } -> n
      Tar.GnuHeader   { Tar.ownerName = n } -> n
      _                                      -> ""
    groupName = case Tar.headerExt entry of
      Tar.UstarHeader { Tar.groupName = n } -> n
      Tar.GnuHeader   { Tar.groupName = n } -> n
      _                                      -> ""
    nameOrID n i = if null n then show i else n
    size = show (Tar.fileSize entry)
    time = formatEpochTime "%Y-%m-%d %H:%M" (Tar.modTime entry)
    name = Tar.fileName entry
    link = case Tar.fileType entry of
             Tar.HardLink     -> " link to " ++ Tar.linkTarget entry
             Tar.SymbolicLink -> " -> " ++ Tar.linkTarget entry
             _                -> ""

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
    optAction      :: Action,
    optCompression :: Compression,
    optVerbosity   :: Verbosity
  }

defaultOptions :: Options
defaultOptions = Options {
    optFile        = "-",
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
