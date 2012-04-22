{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
import Control.Monad(liftM)
import Data.Char(toUpper)
import Data.ConfigFile
import Data.Maybe(fromMaybe)
import Data.String.Utils(split)
import Data.Time.Clock(UTCTime(..), getCurrentTime)
import Data.Time.Format(formatTime)
import Graphics.UI.Gtk hiding (get, set)
import Graphics.UI.Gtk.Builder
import System.Exit(ExitCode(..), exitWith)
import System.Console.GetOpt
import System.Directory(getHomeDirectory)
import System.Environment(getArgs)
import System.Locale(defaultTimeLocale)

import DB(connect, addQSO)
import DXCC(DXCC(..), idFromName)
import qualified Formats.ADIF.Types as ADIF
import Lookup.Lookup
import QSO
import Utils(uncolonifyTime, undashifyDate)

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
    optGraphical :: Bool,
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
    optGraphical = False,
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

    Option ['c'] ["dxcc"]       (ReqArg (\arg opt -> return opt { optDXCC = strToInteger arg }) "DXCC")
           "their DXCC entity",
    Option ['g'] ["graphical"]  (NoArg  (\opt -> return opt { optGraphical = True }))
           "run in graphical mode",
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
    Option ['r'] ["grid"]       (ReqArg (\arg opt -> return opt { optGrid = Just arg }) "GRID")
           "their grid square",
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
-- UI CODE
--

data ProgramWidgets = ProgramWidgets {
    pwCall :: Entry,
    pwRSTRcvd :: Entry,
    pwRSTSent :: Entry,
    pwExchangeRcvd :: Entry,
    pwExchangeSent :: Entry,
    pwFrequency :: Entry,
    pwDate :: Entry,
    pwTime :: Entry,

    pwModeCombo :: ComboBox,

    pwStatus :: Statusbar,

    pwCancel :: Button,
    pwAdd :: Button }

setDateTime w = do
    -- Store the current UTC date and time in the appropriate entries.
    currentUTC <- getCurrentTime

    entrySetText (pwDate w) (formatTime defaultTimeLocale "%F" currentUTC)
    entrySetText (pwTime w) (formatTime defaultTimeLocale "%R" currentUTC)

setDefaultMode combo = comboBoxSetActive combo 3

-- This function is called when the Cancel button is clicked in order to blank
-- out the UI and prepare it for starting over.  Expected user behavior is that
-- Cancel is for when you've entered bad information and need to try again
-- (missed a QSO, etc.) and quitting the application is for when you want to quit.
clearUI :: ProgramWidgets -> IO ()
clearUI w = do
    -- Blank out all the text entry widgets.
    let widgets = [pwCall w, pwRSTRcvd w, pwRSTSent w, pwExchangeRcvd w, pwExchangeSent w,
                   pwFrequency w, pwDate w, pwTime w]
    mapM_ (flip entrySetText "") widgets

    -- Set the mode combo back to SSB.
    setDefaultMode (pwModeCombo w)

    -- Remove the status bar message.
    statusbarRemoveAll (pwStatus w) 0

    setDateTime w

-- Some UI setup is best done in code instead of in the glade file.  This
-- function is called only once, upon program startup.
initUI :: ProgramWidgets -> IO ()
initUI w = do
    -- Populate the mode combo box with our valid modes.  We don't want to
    -- support all billion that ADIF does, since that'd be very unwieldy.
    let combo = pwModeCombo w
    store <- comboBoxSetModelText combo
    mapM_ (comboBoxAppendText combo) ["AM", "CW", "FM", "SSB"]
    setDefaultMode combo

    setDateTime w

buildArgList :: ProgramWidgets -> IO [String]
buildArgList w = do
    liftM concat $ sequence [getOneEntry pwCall "-l",
                             getTwoEntries pwRSTRcvd pwRSTSent "-r",
                             getTwoEntries pwExchangeRcvd pwExchangeSent "-x",
                             getOneEntry pwFrequency "-f",
                             getOneEntry pwDate "-d",
                             getOneEntry pwTime "-t"]
 where
    getEntryText f = do
        entryGetText (f w)
    getOneEntry f optName = do
        s <- getEntryText f
        if s == "" then return [] else return [optName, s]
    getTwoEntries f1 f2 optName = do
        [s1, s2] <- mapM getEntryText [f1, f2]
        if s1 == "" then return [] else
            if s2 == "" then return [optName, s1] else return [optName, s1 ++ ":" ++ s2]

-- This function is called when the Add button is clicked in order to add a
-- QSO into the database.
addQSOFromUI :: ProgramWidgets -> IO ()
addQSOFromUI w = do
    -- Convert the UI elements into a list of command line arguments, like the program
    -- would normally take.  This is a little lame, but it avoids having to come up with
    -- all that validation code again.
    args <- buildArgList w

    -- TODO:  INPUT VALIDATION HERE

    -- Clear out the UI for next time.
    clearUI w

    -- And update the status bar so the user knows what got added.
    statusbarPush (pwStatus w) 0 ("QSO added to database.")

    putStrLn $ show args

loadWidgets :: Builder -> IO ProgramWidgets
loadWidgets builder = do
    [pwCall, pwRSTRcvd, pwRSTSent, pwExchangeRcvd, pwExchangeSent, pwFrequency,
     pwDate, pwTime] <- mapM (getO castToEntry)
                             ["callEntry", "rstRcvdEntry", "rstSentEntry", "exchangeRcvdEntry",
                              "exchangeSentEntry", "frequencyEntry", "dateEntry", "timeEntry"]

    [pwModeCombo] <- mapM (getO castToComboBox) ["modeComboBox"]
    [pwCancel, pwAdd] <- mapM (getO castToButton) ["cancelButton", "addButton"]
    [pwStatus] <- mapM (getO castToStatusbar) ["statusBar"]

    return $ ProgramWidgets pwCall pwRSTRcvd pwRSTSent pwExchangeRcvd pwExchangeSent
                            pwFrequency pwDate pwTime pwModeCombo pwStatus pwCancel pwAdd
 where
    getO cast = builderGetObject builder cast

runGUI :: IO ()
runGUI = do
    initGUI

    -- Load the glade file.
    builder <- builderNew
    builderAddFromFile builder "data/qsoadd.ui"

    widgets <- loadWidgets builder

    -- Set up the initial state of the UI.
    initUI widgets

    -- Set up GTK signal handlers to do something.
    window <- builderGetObject builder castToWindow "window1"
    onDestroy window mainQuit

    onClicked (pwCancel widgets) $ clearUI widgets
    onClicked (pwAdd widgets) $ addQSOFromUI widgets

    -- And away we go!
    widgetShowAll window
    mainGUI

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
    qDate       = undashifyDate $ optDate opt <!> "You must specify a date of the form YYYY-MM-DD.",
    qTime       = uncolonifyTime $ optTime opt <!> "You must specify a time of the form HHMM.",
    qFreq       = optFreq opt <!> "You must specify a frequency.",
    qRxFreq     = optRxFreq opt <?> Nothing,
    qMode       = optMode opt <!> "You must specify a valid mode.",
    qDXCC       = (raCountry ra >>= idFromName) ||| optDXCC opt <?> Nothing,
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
    homeDir <- getHomeDirectory
    conf <- readConfigFile (homeDir ++ "/.slog")

    -- Open the database.  We do not have to close the database since that happens
    -- automatically.
    dbh <- connect $ confDB conf

    -- Are we running in graphical mode or cmdline mode?
    if optGraphical cmdline then runGUI
    else case optCall cmdline of
                  Just call   -> do ra <- doLookup call (confQTHUser conf) (confQTHPass conf)
                                    addQSO dbh $ buildQSO (fromMaybe emptyRadioAmateur ra) cmdline
                                    return ()
                  _           -> ioError (userError "You must specify a call sign.")
