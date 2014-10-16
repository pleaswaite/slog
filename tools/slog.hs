{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}

import Control.Applicative((<$>))
import Control.Exception(bracket_)
import Control.Monad(liftM, when, void)
import Data.Maybe(fromJust, isJust)
import Data.Time.Clock(UTCTime(..), getCurrentTime)
import Data.Time.Format(formatTime)
import Graphics.UI.Gtk
import Prelude hiding(lookup)
import System.Locale(defaultTimeLocale)

import Slog.DB(DBResult, getAllQSOs, getQSOsByCall, getQSOsByDXCC, getQSOsByGrid)
import Slog.DXCC(DXCC(dxccEntity), entityFromID, idFromName)
import Slog.Formats.ADIF.Types(Band(..))
import Slog.Formats.ADIF.Utils(freqToBand)
import Slog.Lookup.Lookup(RadioAmateur(..), RAUses(Yes), login, lookupCall)
import Slog.Utils(colonifyTime, dashifyDate, uppercase)
import Slog.QSO(Confirmation(..), QSO(..), isConfirmed)
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
    wAllView :: TreeView,

    wStatus :: Statusbar,

    wNewQSOGrid :: Table,
    wDXCCGrid :: Table,
    wGridGrid :: Table
 }

-- The data type stored in a ListStore and displayed in one of two places:  On the main
-- screen as part of the previous QSOs with a given call, and on the secondary screen as
-- part of the all QSOs list.
data DisplayRow = DisplayRow { dDate      :: String,
                               dTime      :: String,
                               dCall      :: String,
                               dFreq      :: Double,
                               dRxFreq    :: Maybe Double,
                               dMode      :: String,
                               dDXCC      :: Maybe Integer,
                               dGrid      :: Maybe String,
                               dXcIn      :: Maybe String,
                               dXcOut     :: Maybe String,
                               dAntenna   :: Maybe String,
                               dConfirmed :: Bool }

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

addEntityCheck :: Widgets -> Maybe Band -> IO ()
addEntityCheck widgets band | band == Just Band160M = addCheckToTable (wDXCCGrid widgets) 0  2
                            | band == Just Band80M  = addCheckToTable (wDXCCGrid widgets) 1  2
                            | band == Just Band60M  = addCheckToTable (wDXCCGrid widgets) 2  2
                            | band == Just Band40M  = addCheckToTable (wDXCCGrid widgets) 3  2
                            | band == Just Band30M  = addCheckToTable (wDXCCGrid widgets) 4  2
                            | band == Just Band20M  = addCheckToTable (wDXCCGrid widgets) 5  2
                            | band == Just Band17M  = addCheckToTable (wDXCCGrid widgets) 6  2
                            | band == Just Band15M  = addCheckToTable (wDXCCGrid widgets) 7  2
                            | band == Just Band12M  = addCheckToTable (wDXCCGrid widgets) 8  2
                            | band == Just Band10M  = addCheckToTable (wDXCCGrid widgets) 9  2
                            | band == Just Band6M   = addCheckToTable (wDXCCGrid widgets) 10 2
                            | otherwise        = return ()

addGridCheck :: Widgets -> Maybe Band -> IO ()
addGridCheck widgets band | band == Just Band6M        = addCheckToTable (wGridGrid widgets) 0 2
                          | band == Just Band2M        = addCheckToTable (wGridGrid widgets) 1 2
                          | band == Just Band1Point25M = addCheckToTable (wGridGrid widgets) 2 2
                          | band == Just Band70CM      = addCheckToTable (wGridGrid widgets) 3 2
                          | otherwise             = return ()

blockUI :: Widgets -> Bool -> IO ()
blockUI widgets b = do
    -- The "not" is here because it reads a lot better to write "blockUI True" than to pass
    -- the correct value.
    set (wCurrent widgets) [ widgetSensitive := not b ]
    mapM_ (\widget -> set widget [ widgetSensitive := not b ])
          [wLookup widgets, wClear widgets, wAdd widgets]

clearChecks :: Widgets -> IO ()
clearChecks widgets =
    mapM_ (\cont -> containerForeach cont (removeImage cont))
          [wNewQSOGrid widgets, wDXCCGrid widgets, wGridGrid widgets]
 where
    removeImage container widget = do
        when (isA widget gTypeImage) $ do containerRemove container widget

-- Create the columns and renderers for the previous QSOs with a given call area.
-- Create the columns and renderers for a given TreeView, leaving it ready to be filled
-- with data.  This function should only be called once or each column will appear
-- multiple times.
initTreeView :: ListStore DisplayRow -> TreeView -> IO ()
initTreeView store view = do
    treeViewSetModel view store

    -- DATE
    dateCol <- newTextColumn store "Date" dDate
    treeViewAppendColumn view dateCol

    -- TIME
    timeCol <- newTextColumn store "Time" dTime
    treeViewAppendColumn view timeCol

    -- CALL SIGN
    callCol <- newTextColumn store "Call" dCall
    treeViewAppendColumn view callCol

    -- FREQUENCY & RX FREQUENCY
    freqCol <- newTextColumn store "Frequency" (show . dFreq)
    treeViewAppendColumn view freqCol
    rxFreqCol <- newTextColumn store "Rcvd Frequency" $ \row -> maybe "" show (dRxFreq row)
    treeViewAppendColumn view rxFreqCol

    -- MODE
    modeCol <- newTextColumn store "Mode" dMode
    treeViewAppendColumn view modeCol

    -- DXCC
    dxccCol <- newTextColumn store "DXCC" $ \row -> if (isJust $ dDXCC row) then maybe "" dxccEntity (entityFromID $ fromJust $ dDXCC row)
                                                    else ""
    treeViewAppendColumn view dxccCol

    -- GRID
    gridCol <- newTextColumn store "Grid" $ \row -> maybe "" id (dGrid row)
    treeViewAppendColumn view gridCol

    -- EXCHANGE
    xcInCol <- newTextColumn store "Rcvd XC" $ \row -> maybe "" id (dXcIn row)
    treeViewAppendColumn view xcInCol

    xcOutCol <- newTextColumn store "XC" $ \row -> maybe "" id (dXcOut row)
    treeViewAppendColumn view xcOutCol

    -- ANTENNA
    antennaCol <- newTextColumn store "Antenna" $ \row -> maybe "" id (dAntenna row)
    treeViewAppendColumn view antennaCol

    -- CONFIRMED
    uploadedCol <- treeViewColumnNew
    uploadedCell <- cellRendererToggleNew
    cellLayoutPackStart uploadedCol uploadedCell False
    cellLayoutSetAttributes uploadedCol uploadedCell store $ \row -> [ cellToggleActive := dConfirmed row ]
    treeViewAppendColumn view uploadedCol
    treeViewColumnSetTitle uploadedCol "Confirmed?"
 where
    newTextColumn :: ListStore DisplayRow -> String -> (DisplayRow -> String) -> IO TreeViewColumn
    newTextColumn model title fn = do
        col <- treeViewColumnNew
        cell <- cellRendererTextNew
        cellLayoutPackStart col cell True
        cellLayoutSetAttributes col cell model $ \row -> [ cellText := fn row,
                                                           cellXPad := 6 ]
        treeViewColumnSetTitle col title
        return col

-- Given an existing ListStore, add all the results to it.  This populates the view that should have
-- been created earlier with initTreeView .  This function can be called as many times as needed
-- since it will first clear out the store.
populateTreeView :: ListStore DisplayRow -> [DBResult] -> IO ()
populateTreeView store results = do
    -- Clear out any previously existing model.
    listStoreClear store

    -- Populate the new model.
    mapM_ (\(_, q, c) -> listStoreAppend store $ DisplayRow { dDate=dashifyDate $ qDate q,
                                                              dTime=colonifyTime $ qTime q,
                                                              dCall=qCall q,
                                                              dFreq=qFreq q,
                                                              dRxFreq=qRxFreq q,
                                                              dMode=show $ qMode q,
                                                              dDXCC=qDXCC q,
                                                              dGrid=qGrid q,
                                                              dXcIn=qXcIn q,
                                                              dXcOut=qXcOut q,
                                                              dAntenna=qAntenna q,
                                                              dConfirmed=isConfirmed c})
          results

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
lookupCallsign :: Widgets -> ListStore DisplayRow -> Config -> IO Bool
lookupCallsign widgets store conf = do
    call <- get (wCall widgets) entryText

    when (not $ null call) $ do
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
        -- We start with clearing out the checkmarks.  This is so you can lookup a call, see what
        -- previous contacts may have been made, and then do another lookup without first having
        -- to hit the clear button.
        clearChecks widgets

        -- If they're a LOTW user, put a nice big check mark image in there.
        when (raLOTW ra' == Just Yes) $ addCheckToTable (wNewQSOGrid widgets) 1 7

        -- Put their call in the label, and then populate the list of previous QSOs we've
        -- had with this station.
        when (isJust $ raCall ra') $ do
            set (wPrevious widgets) [ widgetSensitive := True, frameLabel := "Previous contacts with " ++ call ]

            results <- getQSOsByCall fp call
            populateTreeView store results

        -- Put their call in the label, and then add check marks in for DXCC entity
        -- and grid confirmations.
        when (isJust $ raCountry ra') $ do
            set (wDXCC widgets) [ widgetSensitive := True, frameLabel := (fromJust . raCountry) ra' ++ " status" ]

            let dxcc = raCountry ra' >>= idFromName
            when (isJust dxcc) $ do
                -- Get a list of all QSOs we've had with this entity, narrow it down to just what's
                -- been confirmed, and then put checkmarks in where appropriate.
                results <- filter confirmed <$> getQSOsByDXCC fp (fromInteger $ fromJust dxcc)
                let confirmedBands = map getBand results
                mapM_ (addEntityCheck widgets) confirmedBands

        -- FIXME: This is not necessarily the right grid.  We really need to know the grid they're
        -- in first, since a station could be in a different grid than their QTH.  We won't know that
        -- until asking them personally or looking at the cluster, though.  For now this will have
        -- to do.
        when (isJust $ raGrid ra') $ do
            set (wGrid widgets) [ widgetSensitive := True, frameLabel := shortGrid ++ " status" ]

            results <- filter confirmed <$> getQSOsByGrid fp shortGrid
            let confirmedBands = map getBand results
            mapM_ (addGridCheck widgets) confirmedBands
     where
        call = uppercase . fromJust $ raCall ra'

        confirmed (_, _, c) = isJust $ qLOTW_RDate c

        getBand (_, q, _) = freqToBand $ qFreq q

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

    [previousV, allV] <- mapM (getO castToTreeView) ["previousTreeView", "allTreeView"]

    [status] <- mapM (getO castToStatusbar) ["statusBar"]

    [newQSO, dxccGrid, gridGrid] <- mapM (getO castToTable) ["newQSOGrid", "dxccGrid", "gridGrid"]

    return $ Widgets call freq rxFreq mode antenna rst_rcvd rst_sent xc_rcvd xc_sent
                     current
                     dateLabel date timeLabel time
                     previous dxcc grid
                     lookupB clearB addB
                     previousV allV
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
    clearChecks widgets
 where
    entryWidgets = [wCall widgets, wAntenna widgets, wRSTRcvd widgets, wRSTSent widgets, wXCRcvd widgets,
                    wXCSent widgets, wDate widgets, wTime widgets]

    setDateTime w = sequence_ [theDate >>= entrySetText (wDate w),
                               theTime >>= entrySetText (wTime w)]

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
    previousStore <- listStoreNew ([] :: [DisplayRow])
    initTreeView previousStore (wPreviousView widgets)

    -- Create the all QSOs view stuff.  Unlike the previous QSOs view, we want to populate
    -- this one right now.
    allStore <- listStoreNew ([] :: [DisplayRow])
    initTreeView allStore (wAllView widgets)

    allQSOs <- getAllQSOs $ confDB conf
    populateTreeView allStore allQSOs

    on (wCurrent widgets) toggled (currentToggled widgets)
    on (wClear widgets) buttonActivated (clearUI widgets)
    on (wLookup widgets) buttonActivated (void $ idleAdd (bracket_ (blockUI widgets True)
                                                                   (blockUI widgets False)
                                                                   (lookupCallsign widgets previousStore conf))
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
