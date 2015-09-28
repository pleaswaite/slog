{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

import Data.Maybe(fromJust, fromMaybe)
import Data.String.Utils(split)
import System.Exit(exitSuccess)
import System.Console.GetOpt
import System.Environment(getArgs)

import Slog.DB(QsosId, addQSO, initDB)
import Slog.DXCC(idFromName)
import qualified Slog.Formats.ADIF.Types as ADIF
import Slog.Lookup.Lookup
import Slog.QSO
import Slog.Utils(stringToDouble, stringToInteger, uncolonifyTime, undashifyDate, uppercase)

import ToolLib.Config

--
-- OPTION PROCESSING CODE
--

data Options = Options {
    optDate :: Maybe String,
    optTime :: Maybe String,
    optFreq :: Maybe Double,
    optRxFreq :: Maybe Double,
    optMode :: Maybe ADIF.Mode,
    optDXCC :: Maybe Integer,
    optGrid :: Maybe String,
    optState :: Maybe String,
    optName :: Maybe String,
    optNotes :: Maybe String,
    optXcIn :: Maybe String,
    optXcOut :: Maybe String,
    optRSTRcvd :: Maybe String,
    optRSTSent :: Maybe String,
    optITU :: Maybe Integer,
    optWAZ :: Maybe Integer,
    optCall :: Maybe String,
    optPropMode :: Maybe ADIF.Propagation,
    optSatName :: Maybe String,
    optAntenna :: Maybe String,
    optMyCall :: Maybe String,
    optMyQTH :: Maybe String }
 deriving (Show)

type OptAction = (Options -> IO Options)

defaultOptions :: Options
defaultOptions = Options {
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
    optRSTRcvd = Nothing,
    optRSTSent = Nothing,
    optITU = Nothing,
    optWAZ = Nothing,
    optCall = Nothing,
    optPropMode = Nothing,
    optSatName = Nothing,
    optAntenna = Nothing,
    optMyCall = Nothing,
    optMyQTH = Nothing }

opts :: [OptDescr OptAction]
opts = [
    Option "A" ["antenna"]    (ReqArg (\arg opt -> return opt { optAntenna = Just arg }) "ANTENNA")
           "antenna used for QSO",
    Option "a" ["satellite"]  (ReqArg (\arg opt -> return opt { optSatName = Just arg,
                                                                optPropMode = Just ADIF.SAT })
                                      "SATELLITE")
           "name of satellite",
    Option "C" ["mycall"]     (ReqArg (\arg opt -> return opt { optMyCall = Just arg }) "MYCALL")
           "the call sign this contact was made using (REQUIRED)",
    Option "c" ["dxcc"]       (ReqArg (\arg opt -> return opt { optDXCC = stringToInteger arg }) "DXCC")
           "their DXCC entity",
    Option "d" ["date"]       (ReqArg (\arg opt -> return opt { optDate = Just arg }) "DATE")
           "date the QSO was made (in UTC) (REQUIRED)",
    Option "f" ["freq"]       (ReqArg (\arg opt -> do let sp = splitArg arg
                                                      return opt { optFreq = maybe Nothing stringToDouble (fst sp),
                                                                   optRxFreq = maybe Nothing stringToDouble (snd sp) })
                                      "FREQ")
           "frequency used (or their freq:your freq for split mode) (REQUIRED)",
    Option "g" ["grid"]       (ReqArg (\arg opt -> return opt { optGrid = Just arg }) "GRID")
           "their grid square",
    Option "h" ["help"]       (NoArg  (\_ -> do
                                                 putStrLn (usageInfo "qsoadd" opts)
                                                 exitSuccess))
           "print program usage",
    Option "i" ["itu"]        (ReqArg (\arg opt -> return opt { optITU = stringToInteger arg }) "ITU")
           "their ITU zone",
    Option "l" ["call"]       (ReqArg (\arg opt -> return opt { optCall = Just arg }) "CALL")
           "their call sign (REQUIRED)",
    Option "m" ["mode"]       (ReqArg (\arg opt -> return opt { optMode = Just (read (uppercase arg) :: ADIF.Mode) }) "MODE")
           "mode used (REQUIRED)",
    Option "n" ["name"]       (ReqArg (\arg opt -> return opt { optName = Just arg }) "NAME")
           "their name",
    Option "o" ["notes"]      (ReqArg (\arg opt -> return opt { optNotes = Just arg }) "NOTES")
           "notes",
    Option "q" ["qth"]        (ReqArg (\arg opt -> return opt { optTime = Just arg }) "QTH")
           "where this contact was made from (must match what LoTW expects) (REQUIRED)",
    Option "r" ["rst"]        (ReqArg (\arg opt -> do let sp = splitArg arg
                                                      return opt { optRSTRcvd = fst sp, optRSTSent = snd sp })
                                      "RST")
           "rst rcvd:rst sent (REQUIRED)",
    Option "s" ["state"]      (ReqArg (\arg opt -> return opt { optState = Just arg }) "STATE")
           "their state",
    Option "t" ["time"]       (ReqArg (\arg opt -> return opt { optTime = Just arg }) "TIME")
           "time the QSO was made (in UTC) (REQUIRED)",
    Option "w" ["waz"]        (ReqArg (\arg opt -> return opt { optWAZ = stringToInteger arg }) "WAZ")
           "their WAZ zone",
    Option "x" ["xc"]         (ReqArg (\arg opt -> do let sp = splitArg arg
                                                      return opt { optXcIn = fst sp, optXcOut = snd sp })
                                      "EXCHANGE")
           "exchange rcvd:exchange sent"
 ]

handleOpts :: [String] -> IO ([OptAction], [String])
handleOpts argv =
    case getOpt RequireOrder opts argv of
        (o, n, [])   -> return (o, n)
        (_, _, errs) -> ioError (userError (concat errs ++ usageInfo header opts))
                        where header = "Usage: qsoadd [OPTIONS] file"

splitArg :: String -> (Maybe String, Maybe String)
splitArg s | length eles == 1 = (Just $ head eles, Nothing)
           | otherwise        = (Just $ head eles, Just $ eles !! 1)
 where
    eles = split ":" s

processArgs :: Monad m => m ([Options -> m Options], t) -> m Options
processArgs argsFunc = do
    (actions, _) <- argsFunc
    foldl (>>=) (return defaultOptions) actions

--
-- FINALLY, THE MAIN PROGRAM
--

doLookup :: String -> String -> String -> IO (Maybe RadioAmateur)
doLookup call user pass = do
    sid <- login user pass
    maybe (return Nothing) (lookupCall call) sid

-- Construct a new QSO structure, taking into account the defaults we can get
-- from looking up a call sign, from the command line, and what is required.
buildQSO :: RadioAmateur -> Options -> Either String QSO
buildQSO RadioAmateur{..} Options{..} = do
    date <- optDate <!> "You must specify a date of the form YYYY-MM-DD."
    time <- optTime <!> "You must specify a time of the form HH:MM."
    freq <- optFreq <!> "You must specify a frequency."
    mode <- optMode <!> "You must specify a valid mode."
    rstR <- optRSTRcvd <!> "You must specify a received signal report."
    rstS <- optRSTSent <!> "You must specify a sent signal report."
    call <- optCall ||| raCall <!> "You must specify a call sign."
    myCall <- optMyCall <!> "You must specify your call sign."
    qth  <- optMyQTH <!> "You must specify your QTH."
    Right $ doBuildQSO (date, time, freq, mode, rstR, rstS, call, myCall, qth)
 where
    doBuildQSO (date, time, freq, mode, rstR, rstS, call, myCall, qth) =
        QSO { qDate     = undashifyDate date,
              qTime     = uncolonifyTime time,
              qFreq     = freq,
              qRxFreq   = optRxFreq <?> Nothing,
              qMode     = mode,
              qDXCC     = (raCountry >>= idFromName) ||| optDXCC <?> Nothing,
              qGrid     = raGrid ||| optGrid <?> Nothing,
              qState    = raUSState ||| optState <?> Nothing,
              qName     = raNick ||| optName <?> Nothing,
              qNotes    = optNotes <?> Nothing,
              qXcIn     = optXcIn <?> Nothing,
              qXcOut    = optXcOut <?> Nothing,
              qRST_Rcvd = rstR,
              qRST_Sent = rstS,
              qITU      = raITU ||| optITU <?> Nothing,
              qWAZ      = raWAZ ||| optWAZ <?> Nothing,
              qCall     = call,
              qPropMode = optPropMode <?> Nothing,
              qSatName  = optSatName <?> Nothing,
              qAntenna  = optAntenna <?> Nothing,
              qMyCall   = myCall,
              qMyQTH    = qth }

    -- If the left side has a value, use it.  Otherwise, use the right side.
    Just v  ||| _       = Just v
    Nothing ||| Just v  = Just v
    Nothing ||| Nothing = Nothing

    -- Extract the value from a chain of ||| calls, or use the default.
    Just v  <?> _   = Just v
    _       <?> v   = v

    -- Extract the value from a chain of ||| calls, or error.
    Just v <!> _    = Right v
    _ <!> msg       = Left msg

lookupAndAddQSO :: Config -> Options -> IO (Either String QsosId)
lookupAndAddQSO Config{..} cmdline = do
    -- Get the on-disk location of the database.
    let fp = confDB

    ra <- doLookup call confQTHUser confQTHPass
    case buildQSO (fromMaybe emptyRadioAmateur ra) cmdline of
        Left err  -> return $ Left err
        Right qso -> do ndx <- addQSO fp qso
                        return $ Right ndx
 where
    -- We don't have to worry about call being Nothing here.  That's checked before
    -- this function is even called.
    call = fromJust $ optCall cmdline

main :: IO ()
main = do
    -- Process command line arguments.
    cmdline <- processArgs (getArgs >>= handleOpts)

    -- Read in the config file.
    conf <- readConfig
    initDB (confDB conf)

    lookupAndAddQSO conf cmdline >>= \case
        Left err   -> ioError (userError err)
        Right _    -> return ()
