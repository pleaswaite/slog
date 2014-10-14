{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}

import Control.Exception(bracket_)
import Control.Monad(liftM, when, void)
import Data.Maybe(fromJust, isJust)
import Data.Time.Clock(UTCTime(..), getCurrentTime)
import Data.Time.Format(formatTime)
import Graphics.UI.Gtk
import Prelude hiding(lookup)
import System.Locale(defaultTimeLocale)

import Slog.DB(getQSOsByCall)
import Slog.Lookup.Lookup(RadioAmateur(..), RAUses(Yes), login, lookupCall)
import Slog.Utils(colonifyTime, dashifyDate, uppercase)
import Slog.QSO(QSO(..))
import ToolLib.Config

--
-- WORKING WITH TIME
--

formatDateTime :: String -> UTCTime -> String
formatDateTime spec utc = formatTime defaultTimeLocale spec utc

theTime :: IO String
theTime = liftM (formatDateTime "%R") getCurrentTime

theDate :: IO String
theDate = liftM (formatDateTime "%F") getCurrentTime

--
-- UI TYPES
--

-- A record to hold all the widgets we'll want out of the glade file.
data Widgets = Widgets {
    wCall :: Entry,
    wFreq :: Entry,
    wRxFreq :: Entry,
    wMode :: Entry,
    wAntenna :: Entry,
    wRSTRcvd :: Entry,
    wRSTSent :: Entry,
    wXCRcvd :: Entry,
    wXCSent :: Entry,

    wCurrent :: CheckButton,

    wDateLabel :: Label,
    wDate :: Entry,
    wTimeLabel :: Label,
    wTime :: Entry,

    wPrevious :: Frame,
    wDXCC :: Frame,
    wGrid :: Frame,

    wLookup :: Button,
    wClear :: Button,
    wAdd :: Button,

    wPreviousView :: TreeView,

    wStatus :: Statusbar,

    wNewQSOGrid :: Table,
    wDXCCGrid :: Table,
    wGridGrid :: Table
 }

data PreviousRow = PreviousRow { pDate      :: String,
                                 pTime      :: String,
                                 pCall      :: String,
                                 pFreq      :: Double,
                                 pRxFreq    :: Maybe Double,
                                 pMode      :: String,
                                 pAntenna   :: Maybe String,
                                 pConfirmed :: Bool }

--
-- UI HELPERS
--

addCheckToTable :: Table -> Int -> Int -> IO ()
addCheckToTable tbl col row = do
    img <- imageNewFromStock stockApply IconSizeButton
    tableAttach tbl img
                col (col+1) row (row+1)
                [] []
                0 0
    widgetShowAll tbl
    return ()

blockUI :: Widgets -> Bool -> IO ()
blockUI widgets b = do
    -- The "not" is here because it reads a lot better to write "blockUI True" than to pass
    -- the correct value.
    set (wCurrent widgets) [ widgetSensitive := not b ]
    mapM_ (\widget -> set widget [ widgetSensitive := not b ])
          [wLookup widgets, wClear widgets, wAdd widgets]

-- Create the columns and renderers for the previous QSOs with a given call area.
-- This function should only be called once or each column will appear multiple times.
createPreviousArea :: ListStore PreviousRow -> TreeView -> IO ()
createPreviousArea store view = do
    treeViewSetModel view store

    -- DATE
    dateCol <- newTextColumn store "Date" pDate
    treeViewAppendColumn view dateCol

    -- TIME
    timeCol <- newTextColumn store "Time" pTime
    treeViewAppendColumn view timeCol

    -- CALL SIGN
    callCol <- newTextColumn store "Call" pCall
    treeViewAppendColumn view callCol

    -- FREQUENCY & RX FREQUENCY
    freqCol <- newTextColumn store "Frequency" (show . pFreq)
    treeViewAppendColumn view freqCol
    rxFreqCol <- newTextColumn store "Rcvd Frequency" $ \row -> maybe "" show (pRxFreq row)
    treeViewAppendColumn view rxFreqCol

    -- MODE
    modeCol <- newTextColumn store "Mode" pMode
    treeViewAppendColumn view modeCol

    -- ANTENNA
    antennaCol <- newTextColumn store "Antenna" $ \row -> maybe "" id (pAntenna row)
    treeViewAppendColumn view antennaCol

    -- CONFIRMED
    uploadedCol <- treeViewColumnNew
    uploadedCell <- cellRendererToggleNew
    cellLayoutPackStart uploadedCol uploadedCell False
    cellLayoutSetAttributes uploadedCol uploadedCell store $ \row -> [ cellToggleActive := pConfirmed row ]
    treeViewAppendColumn view uploadedCol
    treeViewColumnSetTitle uploadedCol "Confirmed?"
 where
    newTextColumn :: ListStore PreviousRow -> String -> (PreviousRow -> String) -> IO TreeViewColumn
    newTextColumn model title fn = do
        col <- treeViewColumnNew
        cell <- cellRendererTextNew
        cellLayoutPackStart col cell True
        cellLayoutSetAttributes col cell model $ \row -> [ cellText := fn row,
                                                           cellXPad := 6 ]
        treeViewColumnSetTitle col title
        return col

populatePreviousArea :: ListStore PreviousRow -> [QSO] -> IO ()
populatePreviousArea store qsos = do
    -- Clear out any previously existing model.
    listStoreClear store

    -- Populate the new model.
    mapM_ (\q -> listStoreAppend store $ PreviousRow { pDate=dashifyDate $ qDate q,
                                                       pTime=colonifyTime $ qTime q,
                                                       pCall=qCall q,
                                                       pFreq=qFreq q,
                                                       pRxFreq=qRxFreq q,
                                                       pMode=show $ qMode q,
                                                       pAntenna=qAntenna q,
                                                       pConfirmed=False })
          qsos

--
-- FUNCTIONS FOR QUERYING WIDGET STATE
--

currentActive :: Widgets -> IO Bool
currentActive w = get (wCurrent w) toggleButtonActive

--
-- SIGNAL HANDLERS
--

-- When the "Use current date & time" button is toggled, change sensitivity on widgets underneath it.
currentToggled :: Widgets -> IO ()
currentToggled widgets = do
    active <- currentActive widgets
    set (wDateLabel widgets) [ widgetSensitive := not active ]
    set (wDate widgets)      [ widgetSensitive := not active ]
    set (wTimeLabel widgets) [ widgetSensitive := not active ]
    set (wTime widgets)      [ widgetSensitive := not active ]

-- When the "Lookup" button next to the call sign entry is clicked, we want to look that call up
-- in HamQTH and fill in some information on the screen.  This is called as a callback in an idle
-- handler so the lookup can proceed while the UI continues to refresh.
lookupCallsign :: Widgets -> ListStore PreviousRow -> Config -> IO Bool
lookupCallsign widgets store conf = do
    call <- get (wCall widgets) entryText
    ra <- lookup (uppercase call)
                 (confQTHUser conf)
                 (confQTHPass conf)

    -- Strip off the Maybe bit from the result now.
    when (isJust ra) $ updateUI (fromJust ra)

    -- Return false to remove this handler from the main loop.
    return False
 where
    -- The on-disk database location.
    fp = confDB conf

    lookup :: String -> String -> String -> IO (Maybe RadioAmateur)
    lookup call user pass = do
        sid <- login user pass
        maybe (return Nothing) (lookupCall call) sid

    updateUI ra' = do
        statusbarPush (wStatus widgets) 0 ("Lookup of " ++ call ++ " finished.")

        -- And now that we've discovered something, we can update the UI to reflect what we found.

        -- If they're a LOTW user, put a nice big check mark image in there.
        when (raLOTW ra' == Just Yes) $ addCheckToTable (wNewQSOGrid widgets) 1 7

        -- Put their call, dxcc entity, and grid in the appropriate labels.
        when (isJust $ raCall ra')    $ set (wPrevious widgets) [ widgetSensitive := True, frameLabel := "Previous contacts with " ++ call ]
        when (isJust $ raCountry ra') $ set (wDXCC widgets)     [ widgetSensitive := True, frameLabel := (fromJust . raCountry) ra' ++ " status" ]
        when (isJust $ raGrid ra')    $ set (wGrid widgets)     [ widgetSensitive := True, frameLabel := shortGrid ++ " status" ]

        -- Populate the list of previous QSOs we've had with this person.
        when (isJust $ raCall ra') $ do
            qsos <- getQSOsByCall fp call
            populatePreviousArea store qsos
     where
        call = uppercase . fromJust $ raCall ra'

        shortGrid = uppercase $ take 4 $ fromJust $ raGrid ra'

--
-- INIT UI
--

loadWidgets :: Builder -> IO Widgets
loadWidgets builder = do
    [call, freq, rxFreq, mode, antenna, rst_rcvd,
     rst_sent, xc_rcvd, xc_sent, date, time] <- mapM (getO castToEntry) ["callEntry", "freqEntry", "rxFreqEntry", "modeEntry",
                                                                         "antennaEntry", "rstRcvdEntry", "rstSentEntry",
                                                                         "xcRcvdEntry", "xcSentEntry", "dateEntry", "timeEntry"]

    [current] <- mapM (getO castToCheckButton) ["useCurrentDateButton"]
    [dateLabel, timeLabel] <- mapM (getO castToLabel) ["dateLabel", "timeLabel"]

    [previous, dxcc, grid] <- mapM (getO castToFrame) ["previousFrame", "dxccFrame", "gridFrame"]

    [lookupB, clearB, addB] <- mapM (getO castToButton) ["lookupButton", "clearButton", "addButton"]

    [previousV] <- mapM (getO castToTreeView) ["previousTreeView"]

    [status] <- mapM (getO castToStatusbar) ["statusBar"]

    [newQSO, dxccGrid, gridGrid] <- mapM (getO castToTable) ["newQSOGrid", "dxccGrid", "gridGrid"]

    return $ Widgets call freq rxFreq mode antenna rst_rcvd rst_sent xc_rcvd xc_sent
                     current
                     dateLabel date timeLabel time
                     previous dxcc grid
                     lookupB clearB addB
                     previousV
                     status
                     newQSO dxccGrid gridGrid
 where
    getO cast = builderGetObject builder cast

-- This function is called when the Clear button is clicked in order to blank out the
-- UI and prepare it for starting over.  Expected user behavior is that Clear is for
-- when you've entered bad information and need to try again (missed a QSO, etc.) and
-- quitting the application is for when you really want to quit.
clearUI :: Widgets -> IO ()
clearUI widgets = do
    -- Blank out most of the text entry widgets.  Mode we want to set back to SSB, and
    -- frequency/rx frequency we want to leave alone.  This makes it easier to operate
    -- as the station with a pile up.
    mapM_ (flip set [ entryText := "" ]) entryWidgets
    set (wMode widgets) [ entryText := "SSB" ]

    -- Set the current date/time checkbox back to active.
    set (wCurrent widgets) [ toggleButtonActive := True ]

    -- Set the titles on the various frames to something boring.
    set (wPrevious widgets) [ widgetSensitive := False, frameLabel := "Previous contacts with remote station" ]
    set (wDXCC widgets)     [ widgetSensitive := False, frameLabel := "Entity status" ]
    set (wGrid widgets)     [ widgetSensitive := False, frameLabel := "Grid status" ]

    -- Display the current date and time as the default values.
    setDateTime widgets

    -- Remove any status bar message.
    statusbarRemoveAll (wStatus widgets) 0

    -- Remove all checkmarks images found in the various tables.
    mapM_ (\cont -> containerForeach cont (removeImage cont))
          [wNewQSOGrid widgets, wDXCCGrid widgets, wGridGrid widgets]
 where
    entryWidgets = [wCall widgets, wAntenna widgets, wRSTRcvd widgets, wRSTSent widgets, wXCRcvd widgets,
                    wXCSent widgets, wDate widgets, wTime widgets]

    setDateTime w = sequence_ [theDate >>= entrySetText (wDate w),
                               theTime >>= entrySetText (wTime w)]

    removeImage container widget = do
        when (isA widget gTypeImage) $ do containerRemove container widget

runGUI :: Config -> IO ()
runGUI conf = do
    initGUI

    -- Load the glade file.
    builder <- builderNew
    builderAddFromFile builder "data/slog.ui"
    widgets <- loadWidgets builder

    -- Set up GTK signal handlers to do something.
    window <- builderGetObject builder castToWindow "window1"
    onDestroy window mainQuit

    -- Create the previous QSOs view stuff.
    store <- listStoreNew ([] :: [PreviousRow])
    createPreviousArea store (wPreviousView widgets)

    on (wCurrent widgets) toggled (currentToggled widgets)
    on (wClear widgets) buttonActivated (clearUI widgets)
    on (wLookup widgets) buttonActivated (void $ idleAdd (bracket_ (blockUI widgets True)
                                                                   (blockUI widgets False)
                                                                   (lookupCallsign widgets store conf))
                                                         priorityDefaultIdle)

    -- Initialize the widgets to their first state.
    clearUI widgets

    -- Start up the UI.
    widgetShowAll window
    mainGUI

main :: IO ()
main = do
    -- Read in the config file.
    conf <- readConfig

    runGUI conf
