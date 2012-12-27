{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# LANGUAGE DoAndIfThenElse #-}
import Control.Exception(bracket)
import Control.Monad(liftM)
import Control.Monad.Reader(runReaderT)
import Data.ConfigFile
import Data.Maybe(fromJust, fromMaybe, isJust)
import Data.String.Utils(split)
import Data.Time.Clock(UTCTime(..), getCurrentTime)
import Data.Time.Format(formatTime)
import Database.HDBC(IConnection)
import Graphics.UI.Gtk hiding (get, set)
import Graphics.UI.Gtk.Builder
import System.Exit(ExitCode(..), exitWith)
import System.Console.GetOpt
import System.Directory(getHomeDirectory)
import System.Environment(getArgs)
import System.Locale(defaultTimeLocale)

import Slog.DB(connect, addQSO)
import Slog.DXCC(idFromName)
import qualified Slog.Formats.ADIF.Types as ADIF
import Slog.Lookup.Lookup
import Slog.QSO
import qualified Slog.Rigctl.Commands.Ask as Ask
import qualified Slog.Rigctl.Commands.Tell as Tell
import qualified Slog.Rigctl.Rigctl as R
import Slog.Utils(stringToDouble, stringToInteger, uncolonifyTime, undashifyDate, uppercase)

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
    optIOTA :: Maybe Integer,
    optITU :: Maybe Integer,
    optWAZ :: Maybe Integer,
    optCall :: Maybe String,
    optPropMode :: Maybe ADIF.Propagation,
    optSatName :: Maybe String }
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
    optPropMode = Nothing,
    optSatName = Nothing }

opts :: [OptDescr OptAction]
opts = [
    Option ['a'] ["satellite"]  (ReqArg (\arg opt -> return opt { optSatName = Just arg,
                                                                  optPropMode = Just ADIF.SAT })
                                        "SATELLITE")
           "name of satellite",
    Option ['c'] ["dxcc"]       (ReqArg (\arg opt -> return opt { optDXCC = stringToInteger arg }) "DXCC")
           "their DXCC entity",
    Option ['d'] ["date"]       (ReqArg (\arg opt -> return opt { optDate = Just arg }) "DATE")
           "date the QSO was made (in UTC) (REQUIRED)",
    Option ['f'] ["freq"]       (ReqArg (\arg opt -> do let sp = splitArg arg
                                                        return opt { optFreq = maybe Nothing stringToDouble (fst sp),
                                                                     optRxFreq = maybe Nothing stringToDouble (snd sp) })
                                        "FREQ")
           "frequency used (or their freq:your freq for split mode) (REQUIRED)",
    Option ['g'] ["graphical"]  (NoArg  (\opt -> return opt { optGraphical = True }))
           "run in graphical mode",
    Option ['h'] ["help"]       (NoArg  (\_ -> do
                                                   putStrLn (usageInfo "qsoadd" opts)
                                                   exitWith ExitSuccess))
           "print program usage",
    Option ['i'] ["itu"]        (ReqArg (\arg opt -> return opt { optITU = stringToInteger arg }) "ITU")
           "their ITU zone",
    Option ['l'] ["call"]       (ReqArg (\arg opt -> return opt { optCall = Just arg }) "CALL")
           "their call sign (REQUIRED)",
    Option ['m'] ["mode"]       (ReqArg (\arg opt -> return opt { optMode = Just $ (read (uppercase arg) :: ADIF.Mode) }) "MODE")
           "mode used (REQUIRED)",
    Option ['n'] ["name"]       (ReqArg (\arg opt -> return opt { optName = Just arg }) "NAME")
           "their name",
    Option ['o'] ["notes"]      (ReqArg (\arg opt -> return opt { optNotes = Just arg }) "NOTES")
           "notes",
    Option ['r'] ["rst"]        (ReqArg (\arg opt -> do let sp = splitArg arg
                                                        return opt { optRST_Rcvd = fst sp, optRST_Sent = snd sp })
                                        "RST")
           "rst rcvd:rst sent (REQUIRED)",
    Option ['s'] ["state"]      (ReqArg (\arg opt -> return opt { optState = Just arg }) "STATE")
           "their state",
    Option ['t'] ["time"]       (ReqArg (\arg opt -> return opt { optTime = Just arg }) "TIME")
           "time the QSO was made (in UTC) (REQUIRED)",
    Option ['q'] ["grid"]       (ReqArg (\arg opt -> return opt { optGrid = Just arg }) "GRID")
           "their grid square",
    Option ['w'] ["waz"]        (ReqArg (\arg opt -> return opt { optWAZ = stringToInteger arg }) "WAZ")
           "their WAZ zone",
    Option ['x'] ["xc"]         (ReqArg (\arg opt -> do let sp = splitArg arg
                                                        return opt { optXcIn = fst sp, optXcOut = snd sp })
                                        "EXCHANGE")
           "exchange rcvd:exchange sent",
    Option [] ["iota"]          (ReqArg (\arg opt -> return opt { optIOTA = stringToInteger arg }) "IOTA")
           "their IOTA number"
 ]

handleOpts :: [String] -> IO ([OptAction], [String])
handleOpts argv =
    case getOpt RequireOrder opts argv of
        (o, n, [])   -> return (o, n)
        (_, _, errs) -> ioError (userError (concat errs ++ usageInfo header opts))
                        where header = "Usage: qsoadd [OPTIONS] file"

splitArg :: String -> (Maybe String, Maybe String)
splitArg s = 
    if length eles == 1 then (Just $ eles !! 0, Nothing)
    else (Just $ eles !! 0, Just $ eles !! 1)
 where
    eles = split ":" s

processArgs argsFunc = do
    (actions, _) <- argsFunc
    foldl (>>=) (return defaultOptions) actions

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

    pwRigctld :: CheckButton,
    pwCurrentDT :: CheckButton,

    pwModeCombo :: ComboBox,

    pwStatus :: Statusbar,

    pwDateLabel :: Label,
    pwTimeLabel :: Label,
    pwFreqLabel :: Label,
    pwModeLabel :: Label,

    pwCancel :: Button,
    pwAdd :: Button }

formatDateTime :: String -> UTCTime -> IO String
formatDateTime spec utc = return $ formatTime defaultTimeLocale spec utc

theTime :: IO String
theTime = getCurrentTime >>= formatDateTime "%R"

theDate :: IO String
theDate = getCurrentTime >>= formatDateTime "%F"

setDateTime :: ProgramWidgets -> IO ()
setDateTime w = do
    theDate >>= entrySetText (pwDate w)
    theTime >>= entrySetText (pwTime w)

setDefaultMode :: ComboBoxClass self => self -> IO ()
setDefaultMode combo = comboBoxSetActive combo 4

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

    -- Set the current date/time checkbox back to active.
    toggleButtonSetActive (pwCurrentDT w) True

    -- Set the rigctld checkbox to active, but only if it's running.
    running <- R.isRigctldRunning
    toggleButtonSetActive (pwRigctld w) running
    setFreqModeSensitivity w

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

    comboBoxSetModelText combo
    mapM_ (comboBoxAppendText combo) ["AM", "CW", "FM", "PSK31", "SSB"]
    setDefaultMode combo

    clearUI w

rigctldActive :: ProgramWidgets -> IO Bool
rigctldActive w = toggleButtonGetActive (pwRigctld w)

currentDTActive :: ProgramWidgets -> IO Bool
currentDTActive w = toggleButtonGetActive (pwCurrentDT w)

-- If the checkbox is active, the individual frequency and mode entries are not.
-- This is how we decide whether to get this data from rigctld or not.
setFreqModeSensitivity :: ProgramWidgets -> IO ()
setFreqModeSensitivity w = do
    running <- R.isRigctldRunning

    -- If rigctld is not running, we shouldn't allow the user to even check
    -- the box, though we should at least tell them why.
    if not running then do
        toggleButtonSetActive (pwRigctld w) False
        statusbarPush (pwStatus w) 0 "rigctld is not running."
        return ()
    else do
        active <- rigctldActive w
        widgetSetSensitive (pwFrequency w) (not active)
        widgetSetSensitive (pwModeCombo w) (not active)
        widgetSetSensitive (pwFreqLabel w) (not active)
        widgetSetSensitive (pwModeLabel w) (not active)

-- If the checkbox is active, the individual date and time entries are not.  This
-- is how we decide which way to get the time for the QSO.
setDateTimeSensitivity :: ProgramWidgets -> IO ()
setDateTimeSensitivity w = do
    active <- currentDTActive w
    widgetSetSensitive (pwDate w) (not active)
    widgetSetSensitive (pwTime w) (not active)
    widgetSetSensitive (pwDateLabel w) (not active)
    widgetSetSensitive (pwTimeLabel w) (not active)

buildArgList :: ProgramWidgets -> IO [String]
buildArgList w = do
    liftM concat $ sequence [getOneEntry pwCall "-l",
                             getTwoEntries pwRSTRcvd pwRSTSent "-r",
                             getTwoEntries pwExchangeRcvd pwExchangeSent "-x",
                             getFromRigctl Ask.Frequency
                                           (\(Tell.Frequency f) -> return ["-f", show f])
                                           (getOneEntry pwFrequency "-f"),
                             getDT (do d <- theDate ; return ["-d", d]) (getOneEntry pwDate "-d"),
                             getDT (do t <- theTime ; return ["-t", t]) (getOneEntry pwTime "-t"),
                             getFromRigctl Ask.Mode
                                           (\(Tell.Mode mode _) -> return ["-m", show mode])
                                           (getCombo pwModeCombo "-m")]
 where
    getDT f fallback = do
        active <- currentDTActive w
        if active then f else fallback

    getFromRigctl inCmd getOutCmd fallback = do
        active <- rigctldActive w
        if active then do bracket (R.connect "localhost" 4532)
                                  (R.disconnect)
                                  doAsk
        else fallback
     where
        doAsk st = do
            result <- runReaderT (R.ask inCmd) st
            either (\_ -> fallback)
                   getOutCmd
                   result

    getOneEntry f opt = do
        s <- entryGetText (f w)
        if s == "" then return [] else return [opt, s]

    getTwoEntries f1 f2 opt = do
        [s1, s2] <- mapM entryGetText [f1 w, f2 w]
        if s1 == "" then return [] else
            if s2 == "" then return [opt, s1] else return [opt, s1 ++ ":" ++ s2]

    getCombo f opt = do
        text <- comboBoxGetActiveText (f w)
        if isJust text then return [opt, fromJust text] else return []

-- This function is called when the Add button is clicked in order to add a
-- QSO into the database.
addQSOFromUI :: ProgramWidgets -> (Options -> IO (Either String Integer)) -> IO ()
addQSOFromUI w addFunc = do
    -- Convert the UI elements into a list of command line arguments, like the program
    -- would normally take.  This is a little lame, but it avoids having to come up with
    -- all that validation code again.
    cmdline <- processArgs (buildArgList w >>= handleOpts)

    case optCall cmdline of
        Just call   -> doAdd addFunc call cmdline
        _           -> showErrorDialog "You must specify a call sign."
 where
    doAdd fn call cmdline = do
        result <- fn cmdline
        case result of
            Left err    -> showErrorDialog err
            _           -> do clearUI w
                              statusbarPush (pwStatus w) 0 ("QSO with " ++ call ++ " added to database.")
                              return ()

    showErrorDialog msg =
        bracket (messageDialogNew Nothing [DialogModal] MessageError ButtonsOk msg)
                (widgetDestroy)
                (\dlg -> dialogRun dlg >> return ())

loadWidgets :: Builder -> IO ProgramWidgets
loadWidgets builder = do
    [call, rstRcvd, rstSent, exchangeRcvd, exchangeSent, frequency,
     date, time] <- mapM (getO castToEntry)
                             ["callEntry", "rstRcvdEntry", "rstSentEntry", "exchangeRcvdEntry",
                              "exchangeSentEntry", "frequencyEntry", "dateEntry", "timeEntry"]

    [rigctld, currentDT] <- mapM (getO castToCheckButton) ["getFromRigCheckbox", "currentDateTimeCheckbox"]
    [modeCombo] <- mapM (getO castToComboBox) ["modeComboBox"]
    [cancel, addButton] <- mapM (getO castToButton) ["cancelButton", "addButton"]
    [status] <- mapM (getO castToStatusbar) ["statusBar"]

    [dateLabel, timeLabel, freqLabel, modeLabel] <- mapM (getO castToLabel)
                                                          ["dateLabel", "timeLabel", "frequencyLabel", "modeLabel"]

    return $ ProgramWidgets call rstRcvd rstSent exchangeRcvd exchangeSent frequency
                            date time rigctld currentDT modeCombo status dateLabel
                            timeLabel freqLabel modeLabel cancel addButton
 where
    getO cast = builderGetObject builder cast

runGUI :: IConnection conn => conn -> Config -> IO ()
runGUI dbh conf = do
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
    onClicked (pwAdd widgets) $ addQSOFromUI widgets (lookupAndAddQSO dbh conf)
    onToggled (pwRigctld widgets) $ setFreqModeSensitivity widgets
    onToggled (pwCurrentDT widgets) $ setDateTimeSensitivity widgets

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
buildQSO :: RadioAmateur -> Options -> Either String QSO
buildQSO ra opt = do
    date <- optDate opt <!> "You must specify a date of the form YYYY-MM-DD."
    time <- optTime opt <!> "You must specify a time of the form HH:MM."
    freq <- optFreq opt <!> "You must specify a frequency."
    mode <- optMode opt <!> "You must specify a valid mode."
    rstR <- optRST_Rcvd opt <!> "You must specify a received signal report."
    rstS <- optRST_Sent opt <!> "You must specify a sent signal report."
    call <- optCall opt ||| raCall ra <!> "You must specify a call sign."
    Right $ doBuildQSO (date, time, freq, mode, rstR, rstS, call)
 where
    doBuildQSO (date, time, freq, mode, rstR, rstS, call) =
        QSO { qDate     = undashifyDate date,
              qTime     = uncolonifyTime time,
              qFreq     = freq,
              qRxFreq   = optRxFreq opt <?> Nothing,
              qMode     = mode,
              qDXCC     = (raCountry ra >>= idFromName) ||| optDXCC opt <?> Nothing,
              qGrid     = raGrid ra ||| optGrid opt <?> Nothing,
              qState    = raUSState ra ||| optState opt <?> Nothing,
              qName     = raNick ra ||| optName opt <?> Nothing,
              qNotes    = optNotes opt <?> Nothing,
              qXcIn     = optXcIn opt <?> Nothing,
              qXcOut    = optXcOut opt <?> Nothing,
              qRST_Rcvd = rstR,
              qRST_Sent = rstS,
              qIOTA     = raIOTA ra ||| optIOTA opt <?> Nothing,
              qITU      = raITU ra ||| optITU opt <?> Nothing,
              qWAZ      = raWAZ ra ||| optWAZ opt <?> Nothing,
              qCall     = call,
              qPropMode = optPropMode opt <?> Nothing,
              qSatName  = optSatName opt <?> Nothing }

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

-- FIXME:  Give a type signature.
lookupAndAddQSO :: IConnection conn => conn -> Config -> Options -> IO (Either String Integer)
lookupAndAddQSO dbh conf cmdline = do
    ra <- doLookup call user password
    case buildQSO (fromMaybe emptyRadioAmateur ra) cmdline of
        Left err  -> return $ Left err
        Right qso -> do ndx <- addQSO dbh qso
                        return $ Right ndx
 where
    -- We don't have to worry about call being Nothing here.  That's checked before
    -- this function is even called.
    call = fromJust $ optCall cmdline
    user = confQTHUser conf
    password = confQTHPass conf

main :: IO ()
main = do
    -- Process command line arguments.
    cmdline <- processArgs (getArgs >>= handleOpts)

    -- Read in the config file.
    homeDir <- getHomeDirectory
    conf <- readConfigFile (homeDir ++ "/.slog")

    -- Open the database.  We do not have to close the database since that happens
    -- automatically.
    dbh <- connect $ confDB conf

    -- Are we running in graphical mode or cmdline mode?
    if optGraphical cmdline then runGUI dbh conf
    else do result <- lookupAndAddQSO dbh conf cmdline
            case result of
                Left err   -> ioError (userError err)
                Right _    -> return ()
