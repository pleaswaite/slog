{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
import Data.Char(toUpper)
import Data.ConfigFile
import Data.Maybe(fromMaybe)
import Data.String.Utils(split)
import System(ExitCode(..), exitWith)
import System.Console.GetOpt
import System.Environment(getArgs)

import DB(connect, addQSO)
import qualified Formats.ADIF.Types as ADIF
import Lookup.Lookup
import QSO

--
-- CONFIG FILE PROCESSING CODE
--

data Config = Config {
   confDB :: String,
   confQTHUser :: String,
   confQTHPass :: String }

readConfigFile :: FilePath -> IO Config
readConfigFile f = do
    contents <- readFile f
    let config = do
        c <- readstring emptyCP contents
        database <- get c "DEFAULT" "database"
        qthUser <- get c "Lookup" "username"
        qthPass <- get c "Lookup" "password"
        return Config { confDB = database,
                        confQTHUser = qthUser,
                        confQTHPass = qthPass }

    case config of
        Left cperr  -> fail $ show cperr
        Right c     -> return c

--
-- OPTION PROCESSING CODE
--

data Options = Options {
    optConfigFile :: String,
    optDate :: Maybe String,
    optTime :: Maybe String,
    optFreq :: Maybe Double,
    optRxFreq :: Maybe Double,
    optMode :: Maybe ADIF.Mode,
    optDXCC :: Maybe String,
    optGrid :: Maybe String,
    optState :: Maybe String,
    optName :: Maybe String,
    optNotes :: Maybe String,
    optXcIn :: Maybe String,
    optXcOut :: Maybe String,
    optRST_Rcvd :: Maybe String,
    optRST_Sent :: Maybe String,
    optIOTA :: Maybe String,
    optITU :: Maybe Integer,
    optWAZ :: Maybe Integer,
    optCall :: Maybe String,
    optSatName :: Maybe String,
    optSatMode :: Maybe String }
 deriving (Show)
type OptAction = (Options -> IO Options)

defaultOptions :: Options
defaultOptions = Options {
    optConfigFile = "slog.conf",
    optDate = Nothing,
    optTime = Nothing,
    optFreq = Nothing,
    optRxFreq = Nothing,
    optMode = Nothing,
    optDXCC = Nothing,
    optGrid = Nothing,
    optState = Nothing,
    optName = Nothing,
    optNotes = Nothing,
    optXcIn = Nothing,
    optXcOut = Nothing,
    optRST_Rcvd = Nothing,
    optRST_Sent = Nothing,
    optIOTA = Nothing,
    optITU = Nothing,
    optWAZ = Nothing,
    optCall = Nothing,
    optSatName = Nothing,
    optSatMode = Nothing }

opts :: [OptDescr OptAction]
opts = [
    Option [] ["config"]        (ReqArg (\arg opt -> return opt { optConfigFile = arg }) "CONFIG")
           "the location of the config file",

    Option ['d'] ["date"]       (ReqArg (\arg opt -> return opt { optDate = Just arg }) "DATE")
           "date the QSO was made (in UTC) (REQUIRED)",
    Option ['f'] ["freq"]       (ReqArg (\arg opt -> do let sp = splitArg arg
                                                        return opt { optFreq = maybe Nothing strToDouble (fst sp),
                                                                     optRxFreq = maybe Nothing strToDouble (snd sp) })
                                        "FREQ")
           "frequency used (or freq:rxfreq for split mode) (REQUIRED)",
    Option ['l'] ["call"]       (ReqArg (\arg opt -> return opt { optCall = Just arg }) "CALL")
           "their call sign (REQUIRED)",
    Option ['m'] ["mode"]       (ReqArg (\arg opt -> return opt { optMode = Just $ (read (map toUpper arg) :: ADIF.Mode) }) "MODE")
           "mode used (REQUIRED)",
    Option ['r'] ["rst"]        (ReqArg (\arg opt -> do let sp = splitArg arg
                                                        return opt { optRST_Rcvd = fst sp, optRST_Sent = snd sp })
                                        "RST")
           "rst rcvd:rst sent (REQUIRED)",
    Option ['t'] ["time"]       (ReqArg (\arg opt -> return opt { optTime = Just arg }) "TIME")
           "time the QSO was made (in UTC) (REQUIRED)",

    Option ['c'] ["dxcc"]       (ReqArg (\arg opt -> return opt { optDXCC = Just $ arg }) "DXCC")
           "their DXCC entity",
    Option ['g'] ["grid"]       (ReqArg (\arg opt -> return opt { optGrid = Just arg }) "GRID")
           "their grid square",
    Option ['h'] ["help"]       (NoArg  (\_ -> do
                                                   putStrLn (usageInfo "qsoadd" opts)
                                                   exitWith ExitSuccess))
           "print program usage",
    Option [] ["iota"]          (ReqArg (\arg opt -> return opt { optIOTA = Just arg }) "IOTA")
           "their IOTA number",
    Option ['i'] ["itu"]        (ReqArg (\arg opt -> return opt { optITU = strToInteger arg }) "ITU")
           "their ITU zone",
    Option ['n'] ["name"]       (ReqArg (\arg opt -> return opt { optName = Just arg }) "NAME")
           "their name",
    Option ['o'] ["notes"]      (ReqArg (\arg opt -> return opt { optNotes = Just arg }) "NOTES")
           "notes",
    Option ['s'] ["state"]      (ReqArg (\arg opt -> return opt { optState = Just arg }) "STATE")
           "their state",
    Option ['w'] ["waz"]        (ReqArg (\arg opt -> return opt { optWAZ = strToInteger arg }) "WAZ")
           "their WAZ zone",
    Option ['x'] ["xc"]         (ReqArg (\arg opt -> do let sp = splitArg arg
                                                        return opt { optXcIn = fst sp, optXcOut = snd sp })
                                        "EXCHANGE")
           "exchange in:exchange out"
 ]

handleOpts :: [String] -> IO ([OptAction], [String])
handleOpts argv =
    case getOpt RequireOrder opts argv of
        (o, n, [])   -> return (o, n)
        (_, _, errs) -> ioError (userError (concat errs ++ usageInfo header opts))
                        where header = "Usage: qsoadd [OPTIONS] file"

strToDouble :: String -> Maybe Double
strToDouble s =
    case reads s :: [(Double, String)] of
        [tup]   -> Just $ fst tup
        _       -> Nothing

strToInteger :: String -> Maybe Integer
strToInteger s =
    case reads s :: [(Integer, String)] of
        [tup]   -> Just $ fst tup
        _       -> Nothing

splitArg :: String -> (Maybe String, Maybe String)
splitArg s = 
    if length eles == 1 then (Just $ eles !! 0, Nothing)
    else (Just $ eles !! 0, Just $ eles !! 1)
 where
    eles = split ":" s

--
-- FINALLY, THE MAIN PROGRAM
--

doLookup :: String -> String -> String -> IO (Maybe RadioAmateur)
doLookup call user pass = do
    sid <- login user pass
    maybe (return Nothing) (lookupCall call) sid

-- Construct a new QSO structure, taking into account the defaults we can get
-- from looking up a call sign, from the command line, and what is required.
buildQSO :: RadioAmateur -> Options -> QSO
buildQSO ra opt = QSO {
    qDate       = optDate opt <!> "You must specify a date of the form YYYY-MM-DD.",
    qTime       = optTime opt <!> "You must specify a time of the form HHMM.",
    qFreq       = optFreq opt <!> "You must specify a frequency.",
    qRxFreq     = optRxFreq opt <?> Nothing,
    qMode       = optMode opt <!> "You must specify a valid mode.",
    qDXCC       = raCountry ra ||| optDXCC opt <?> Nothing,
    qGrid       = raGrid ra ||| optGrid opt <?> Nothing,
    qState      = raUSState ra ||| optState opt <?> Nothing,
    qName       = raNick ra ||| optName opt <?> Nothing,
    qNotes      = optNotes opt <?> Nothing,
    qXcIn       = optXcIn opt <?> Nothing,
    qXcOut      = optXcOut opt <?> Nothing,
    qRST_Rcvd   = optRST_Rcvd opt <!> "You must specify a received signal report.",
    qRST_Sent   = optRST_Sent opt <!> "You must specify a sent signal report.",
    qIOTA       = raIOTA ra ||| optIOTA opt <?> Nothing,
    qITU        = raITU ra ||| optITU opt <?> Nothing,
    qWAZ        = raWAZ ra ||| optWAZ opt <?> Nothing,
    qCall       = raCall ra ||| optCall opt <!> "You must specify a call sign.",
    qSatName    = optSatName opt <?> Nothing,
    qSatMode    = optSatMode opt <?> Nothing }
 where
    -- If the left side has a value, use it.  Otherwise, use the right side.
    Just v  ||| _       = Just v
    Nothing ||| Just v  = Just v
    Nothing ||| Nothing = Nothing

    -- Extract the value from a chain of ||| calls, or use the default.
    Just v  <?> _   = Just v
    _       <?> v   = v

    -- Extract the value from a chain of ||| calls, or error.
    Just v <!> _    = v
    _ <!> msg       = error msg

main :: IO ()
main = do
    -- Process command line arguments.
    (actions, _) <- getArgs >>= handleOpts
    cmdline <- foldl (>>=) (return defaultOptions) actions

    -- Read in the config file.
    conf <- readConfigFile (optConfigFile cmdline)

    -- Open the database.  We do not have to close the database since that happens
    -- automatically.
    dbh <- connect $ confDB conf

    case optCall cmdline of
        Just call   -> do ra <- doLookup call (confQTHUser conf) (confQTHPass conf)
                          addQSO dbh $ buildQSO (fromMaybe emptyRadioAmateur ra) cmdline
                          return ()
        _           -> ioError (userError "You must specify a call sign.")
