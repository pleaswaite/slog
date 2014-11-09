{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind -fno-warn-wrong-do-bind #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE LambdaCase #-}

import Control.Applicative((<$>), (<*))
import Control.Exception(bracket_)
import Control.Monad((>=>), liftM, unless, void, when)
import Data.IORef(IORef)
import Data.List(isSuffixOf)
import Data.Maybe(fromJust, fromMaybe, isJust, isNothing)
import Data.Monoid(First(..), getFirst, mconcat)
import qualified Data.Text as T
import Data.Time.Clock(UTCTime(..), getCurrentTime)
import Data.Time.Format(formatTime)
import Graphics.UI.Gtk
import Prelude hiding(lookup)
import System.Locale(defaultTimeLocale)

import Slog.DB
import Slog.DXCC(DXCC(dxccEntity), entityFromID, idFromName)
import Slog.Formats.ADIF.Types(Band(..), Mode)
import Slog.Formats.ADIF.Utils(freqToBand)
import Slog.Lookup.Lookup(RadioAmateur(..), RAUses(Yes), login, lookupCall)
import Slog.Utils
import Slog.QSO(Confirmation(..), QSO(..), isConfirmed)

import ToolLib.Config

import Contest
import State
import Types

{-# ANN loadWidgets "HLint: ignore Eta reduce" #-}
{-# ANN initTreeView "HLint: ignore Use fromMaybe" #-}

--
-- WORKING WITH TIME
--

formatDateTime :: String -> UTCTime -> String
formatDateTime = formatTime defaultTimeLocale

theTime :: IO String
theTime = liftM (formatDateTime "%R") getCurrentTime

theDate :: IO String
theDate = liftM (formatDateTime "%F") getCurrentTime

--
-- WORKING WITH CALL SIGNS
--

lookup :: String -> String -> String -> IO (Maybe RadioAmateur)
lookup call user pass = do
    sid <- login user pass
    maybe (return Nothing) (lookupCall call) sid

--
-- WORKING WITH COMBO BOXES
--

addAntennas :: Table -> Config -> IO ComboBox
addAntennas tbl conf = do
    combo <- comboBoxNewText
    mapM_ (comboBoxAppendText combo . T.strip . T.pack) (confAntennas conf)
    comboBoxSetActive combo 0

    tableAttach tbl combo
                3 4 2 3
                [Fill] []
                0 0

    widgetShowAll combo
    return combo

addModes :: Table -> IO ComboBox
addModes tbl = do
    combo <- comboBoxNewText
    mapM_ (comboBoxAppendText combo . T.pack) ["AM", "CW", "FM", "PSK31", "RTTY", "SSB"]
    comboBoxSetActive combo 5

    tableAttach tbl combo
                    1 2 2 3
                    [Fill] []
                    0 0

    widgetShowAll combo
    return combo

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
                            | otherwise             = return ()

addGridCheck :: Widgets -> Maybe Band -> IO ()
addGridCheck widgets band | band == Just Band6M        = addCheckToTable (wGridGrid widgets) 0 2
                          | band == Just Band2M        = addCheckToTable (wGridGrid widgets) 1 2
                          | band == Just Band1Point25M = addCheckToTable (wGridGrid widgets) 2 2
                          | band == Just Band70CM      = addCheckToTable (wGridGrid widgets) 3 2
                          | band == Just Band33CM      = addCheckToTable (wGridGrid widgets) 4 2
                          | band == Just Band23CM      = addCheckToTable (wGridGrid widgets) 5 2
                          | otherwise                  = return ()

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
    removeImage container widget =
        when (isA widget gTypeImage) $ containerRemove container widget

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
    dxccCol <- newTextColumn store "DXCC" $ \row -> if isJust $ dDXCC row then maybe "" dxccEntity (entityFromID $ fromJust $ dDXCC row)
                                                    else ""
    treeViewAppendColumn view dxccCol

    -- GRID
    gridCol <- newTextColumn store "Grid" (possiblyEmpty dGrid)
    treeViewAppendColumn view gridCol

    -- EXCHANGE
    xcOutCol <- newTextColumn store "XC" (possiblyEmpty dXcOut)
    treeViewAppendColumn view xcOutCol

    xcInCol <- newTextColumn store "Rcvd XC" (possiblyEmpty dXcIn)
    treeViewAppendColumn view xcInCol

    -- ANTENNA
    antennaCol <- newTextColumn store "Antenna" (possiblyEmpty dAntenna)
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

    possiblyEmpty :: (DisplayRow -> Maybe String) -> DisplayRow -> String
    possiblyEmpty accessor row = maybe "" id (accessor row)

-- Convert a DBResult tuple into a DisplayRow record, suitable for adding into a view.
dbToDR :: DBResult -> DisplayRow
dbToDR (_, q, c) = DisplayRow { dDate=dashifyDate $ qDate q,
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
                                dConfirmed=isConfirmed c }

-- Given an existing ListStore, add all the results to it.  This populates the view that should have
-- been created earlier with initTreeView .  This function can be called as many times as needed
-- since it will first clear out the store.
populateTreeView :: ListStore DisplayRow -> [DBResult] -> IO ()
populateTreeView store results = do
    listStoreClear store
    mapM_ (listStoreAppend store . dbToDR) results

--
-- INPUT CHECKS
--

infixr 1 <??>

(<??>) :: Bool -> String -> Maybe String
False <??> _ = Nothing
True  <??> s = Just s

checkRequiredText :: Entry -> String -> IO (Maybe String)
checkRequiredText entry errorStr = do
    s <- get entry entryText
    return $ T.null s <??> errorStr

checkCall :: Widgets -> IO (Maybe String)
checkCall widgets = checkRequiredText (wCall widgets) "Call sign is empty."

checkFreq :: Widgets -> IO (Maybe String)
checkFreq widgets = do
    s <- get (wFreq widgets) entryText
    return $ T.null s || isNothing (stringToDouble (T.unpack s) >>= freqToBand) <??> "Frequency is empty or does not fall in a ham band."

checkRxFreq :: Widgets -> IO (Maybe String)
checkRxFreq widgets = do
    s <- get (wRxFreq widgets) entryText
    return $ not (T.null s) && isNothing (stringToDouble (T.unpack s) >>= freqToBand) <??> "Rx Frequency does not fall in a ham band."

checkRxRST :: Widgets -> IO (Maybe String)
checkRxRST widgets = checkRequiredText (wRSTRcvd widgets) "Received RST is empty."

checkRST :: Widgets -> IO (Maybe String)
checkRST widgets = checkRequiredText (wRSTSent widgets) "Sent RST is empty."

-- FIXME: we could be smarter about this
checkDate :: Widgets -> IO (Maybe String)
checkDate widgets = checkRequiredText (wDate widgets) "Date is empty."

-- FIXME: we could be smarter about this
checkTime :: Widgets -> IO (Maybe String)
checkTime widgets = checkRequiredText (wTime widgets) "Time is empty."

addQSOChecks :: [Widgets -> IO (Maybe String)]
addQSOChecks = [checkCall, checkFreq, checkRxFreq, checkRxRST, checkRST, checkDate, checkTime]

--
-- FUNCTIONS FOR QUERYING WIDGET STATE
--

currentActive :: Widgets -> IO Bool
currentActive w = get (wCurrent w) toggleButtonActive

contestActive :: CWidgets -> IO Bool
contestActive w = get (cwEnable w) toggleButtonActive

--
-- SIGNAL HANDLERS
--

addQSOFromUI :: IORef PState -> IO ()
addQSOFromUI state = do
    widgets <- readState state psWidgets
    conf <- readState state psConf
    contestMode <- readState state psContestMode

    -- First, check everything the user put into the UI.  If anything's wrong, display an error
    -- message in the info bar and bail out.
    err <- getFirst . mconcat . map First <$> mapM ($ widgets) addQSOChecks

    if isJust err then do statusbarRemoveAll (wStatus widgets) 0
                          void $ statusbarPush (wStatus widgets) 0 (fromJust err)
    else do
        call <- get (wCall widgets) entryText

        -- Oh, we also have to look up the call sign yet again.  There's nowhere to store
        -- a previous lookup, and there's not any guarantee a lookup has been performed.  Some
        -- of the data we get back will get put into the database.
        ra <- lookup call (confQTHUser conf) (confQTHPass conf)

        -- And then a bunch of annoying UI field grabbing.
        date <- undashifyDate <$> getDate widgets
        time <- uncolonifyTime <$> getTime widgets
        freq <- get (wFreq widgets) entryText
        rxFreq <- getMaybe (wRxFreq widgets)
        xcIn <- getMaybe (wXCRcvd widgets)
        xcOut <- getMaybe (wXCSent widgets)
        rstIn <- get (wRSTRcvd widgets) entryText
        rstOut <- get (wRSTSent widgets) entryText
        mode <- comboBoxGetActiveText (wMode widgets)
        antenna <- comboBoxGetActiveText (wAntenna widgets)

        -- We've got everything we need now, so assemble a new QSO record.  We can assume that
        -- all the UI fields have something in them, since that was the point of the failure
        -- checking at the beginning of this function.
        let q = QSO { qDate     = date,
                      qTime     = time,
                      qFreq     = fromJust $ stringToDouble freq,
                      qRxFreq   = maybe Nothing stringToDouble rxFreq,
                      qMode     = (fst . head) ((reads $ maybe "SSB" T.unpack mode) :: [(Mode, String)]),
                      qDXCC     = maybe Nothing (raCountry >=> idFromName) ra,
                      qGrid     = maybe Nothing raGrid ra,
                      qState    = maybe Nothing raUSState ra,
                      qName     = maybe Nothing raNick ra,
                      qNotes    = Nothing,
                      qXcIn     = xcIn,
                      qXcOut    = xcOut,
                      qRST_Rcvd = rstIn,
                      qRST_Sent = rstOut,
                      qITU      = maybe Nothing raITU ra,
                      qWAZ      = maybe Nothing raWAZ ra,
                      qCall     = call,
                      qPropMode = Nothing,
                      qSatName  = Nothing,
                      qAntenna  = fmap T.unpack antenna }

        -- We can finally add the QSO.  Afterwards, make sure to clear out the UI for another
        -- go around and update the list of all QSOs to include the latest.
        addQSO (confDB conf) q
        clearUI state
        statusbarPush (wStatus widgets) 0 ("QSO with " ++ uppercase call ++ " added to database.")

        -- And then if we are in contest mode, we need to compute a value for the next exchange
        -- and put that into the UI (where it'll be read from when we actually add the next QSO)
        -- and into the program state.
        when contestMode $ do
            contestVal <- contestNext <$> readState state psContestVal
            modifyState state (\v -> v { psContestVal = contestVal })
            withStateWidget_ state wXCSent (\w -> set w [ entryText := (T.pack . contestStr) contestVal ])
 where
    getMaybe :: Entry -> IO (Maybe String)
    getMaybe entry = do
        s <- get entry entryText
        return $ if null s then Nothing else Just s

    getDate :: Widgets -> IO String
    getDate widgets = do
        useCurrent <- currentActive widgets
        if useCurrent then theDate else get (wDate widgets) entryText

    getTime :: Widgets -> IO String
    getTime widgets = do
        useCurrent <- currentActive widgets
        if useCurrent then theTime else get (wTime widgets) entryText

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
    call <- uppercase <$> get (wCall widgets) entryText

    unless (null call) $ do
        result <- lookup call (confQTHUser conf) (confQTHPass conf)
        maybe (void $ statusbarPush (wStatus widgets) 0 ("Nothing found for callsign " ++ call))
              (updateUI call)
              result

    -- Return false to remove this handler from the main loop.
    return False
 where
    -- The on-disk database location.
    fp = confDB conf

    updateUI call ra' = do
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
        set (wPrevious widgets) [ widgetSensitive := True, frameLabel := "Previous contacts with " ++ call ]
        qsos <- getQSOsByCall fp call
        populateTreeView store qsos

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
        confirmed (_, _, c) = isJust $ qLOTW_RDate c

        getBand (_, q, _) = freqToBand $ qFreq q

        shortGrid = uppercase $ take 4 $ fromJust $ raGrid ra'

runContestDialog :: IORef PState -> IO ()
runContestDialog state = do
    dlg <- readState state (wContestDlg . psWidgets)
    rc <- dialogRun dlg

    -- If the user clicked OK, we need to grab the values out of the fields and
    -- return the data needed to run the contest state.
    when (rc == ResponseOk) $ do
        -- Are we even operating in contest mode?
        cw <- readState state psCWidgets
        contestMode <- contestActive cw

        -- If yes, we need to store new values for the exchange-generating function into
        -- program state.  If no, don't worry about it.  Checking psContestMode first will
        -- mean whatever else contest-related is stored won't matter.  We figure out which
        -- values to use by looking at what page the notebook is on.
        contestVal <- if contestMode then do
            notebook <- readState state (cwNotebook . psCWidgets)
            get notebook notebookPage >>= \case
                1 -> do v <- stringToInteger <$> get (cwSerialSerial cw) entryText
                        return $ mkSerialContest (fromMaybe 0 v)
                2 -> do serial <- stringToInteger <$> get (cwSweepsSerial cw) entryText
                        prec <- head <$> get (cwSweepsPrec cw) entryText
                        call <- get (cwSweepsCall cw) entryText
                        check <- truncate <$> get (cwSweepsCheck cw) spinButtonValue
                        section <- get (cwSweepsSection cw) entryText
                        return $ mkSweepsContest (Sweeps (fromMaybe 0 serial) prec call check section)
                3 -> mkZoneContest <$> truncate <$> get (cwZoneZone cw) spinButtonValue
                _ -> mkGridContest <$> get (cwGridGrid cw) entryText
         else
             return $ mkNoneContest ""

        -- And then after all this, we should update the sent XC field immediately.
        withStateWidget_ state wXCSent (\w -> set w [ entryText := (T.pack . contestStr) contestVal ])

        -- Oh, RST in a contest is almost always 59/59, so just pre-fill that too so the user
        -- doesn't have to fill it in manually.  It's all about speed.
        withStateWidget_ state wRSTRcvd (\w -> set w [ entryText := if contestMode then "59" else "" ])
        withStateWidget_ state wRSTSent (\w -> set w [ entryText := if contestMode then "59" else "" ])

        -- Write the new state value with what we just found out.
        modifyState state (\v -> v { psContestMode = contestMode,
                                     psContestVal = contestVal })

    widgetHide dlg

-- When a message is pushed into the status bar, check it to see if it's the message that'd be
-- written when a new QSO has been added to the database.  If so, grab that QSO and add it to the
-- all QSOs view.  This is kind of roundabout when we could just do this right after adding the QSO,
-- but that would mean passing the store all over the place.
updateAllQSOsView :: ListStore DisplayRow -> Config -> IO ()
updateAllQSOsView store conf = do
    result <- getLatestQSO (confDB conf)
    listStorePrepend store $ dbToDR result

--
-- INIT UI
--

addSignalHandlers :: IORef PState -> IO ()
addSignalHandlers state = do
    allStore <- readState state psAllStore
    conf <- readState state psConf
    prevStore <- readState state psPrevStore
    w <- readState state psWidgets

    -- Install a bunch of regular signal handlers.
    onDestroy (wMainWindow w) mainQuit

    on (wCurrent w) toggled         (currentToggled w)
    on (wClear w)   buttonActivated (do clearUI state
                                        populateTreeView prevStore []
                                        widgetGrabFocus (wCall w))
    on (wAdd w)     buttonActivated (do addQSOFromUI state
                                        widgetGrabFocus (wCall w))
    on (wLookup w)  buttonActivated (void $ idleAdd (bracket_ (blockUI w True)
                                                              (blockUI w False)
                                                              (lookupCallsign w prevStore conf <* widgetGrabFocus (wCall w)))
                                                    priorityDefaultIdle)

    -- This signal is how we watch for a new QSO being added to the database and then
    -- updating the view of all QSOs.  This is to prevent having to pass stores all
    -- around (even though we're doing that to get it into this function already).
    on (wStatus w)  textPushed (\_ s -> when (" added to database." `isSuffixOf` s) $ do
                                            updateAllQSOsView allStore conf
                                            populateTreeView prevStore [])

    -- These signal handlers are for menu items.
    on (wContestMenu w) actionActivated (runContestDialog state)

    return ()

loadContestWidgets :: Builder -> IO CWidgets
loadContestWidgets builder = do
    [box] <- mapM (builderGetObject builder castToBox) ["contestDialogBox"]

    [gridGrid, serialSerial, sweepsSerial, sweepsPrec,
     sweepsCall, sweepsSection] <- mapM (builderGetObject builder castToEntry)
                                        ["gridGridEntry", "serialSerialEntry", "sweepsSerialEntry",
                                         "sweepsPrecEntry", "sweepsCallEntry", "sweepsSectionEntry"]

    [enable] <- mapM (builderGetObject builder castToRadioButton) ["enableContestButton"]

    [notebook] <- mapM (builderGetObject builder castToNotebook) ["contestNotebook"]

    [sweepsCheck, zoneZone] <- mapM (builderGetObject builder castToSpinButton) ["sweepsCheck", "zoneZone"]

    return $ CWidgets box
                      enable
                      notebook
                      gridGrid
                      serialSerial
                      sweepsSerial sweepsPrec sweepsCall sweepsCheck sweepsSection
                      zoneZone

loadWidgets :: Builder -> ComboBox -> ComboBox -> IO Widgets
loadWidgets builder antennas modes = do
    [call, freq, rxFreq, rst_rcvd,
     rst_sent, xc_rcvd, xc_sent, date, time] <- mapM (builderGetObject builder castToEntry)
                                                     ["callEntry", "freqEntry", "rxFreqEntry",
                                                      "rstRcvdEntry", "rstSentEntry", "xcRcvdEntry",
                                                      "xcSentEntry", "dateEntry", "timeEntry"]

    [current] <- mapM (builderGetObject builder castToCheckButton) ["useCurrentDateButton"]
    [dateLabel, timeLabel] <- mapM (builderGetObject builder castToLabel) ["dateLabel", "timeLabel"]

    [previous, dxcc, grid] <- mapM (builderGetObject builder castToFrame) ["previousFrame", "dxccFrame", "gridFrame"]

    [lookupB, clearB, addB] <- mapM (builderGetObject builder castToButton) ["lookupButton", "clearButton", "addButton"]

    [previousV, allV] <- mapM (builderGetObject builder castToTreeView) ["previousTreeView", "allTreeView"]

    [status] <- mapM (builderGetObject builder castToStatusbar) ["statusBar"]

    [newQSO, dxccGrid, gridGrid] <- mapM (builderGetObject builder castToTable) ["newQSOGrid", "dxccGrid", "gridGrid"]

    [mainWindow] <- mapM (builderGetObject builder castToWindow) ["window1"]
    [contestDlg] <- mapM (builderGetObject builder castToDialog) ["contestDialog"]

    [contestMenu] <- mapM (builderGetObject builder castToAction) ["contestMenuItem"]

    return $ Widgets call freq rxFreq rst_rcvd rst_sent xc_rcvd xc_sent
                     current
                     dateLabel date timeLabel time
                     previous dxcc grid
                     lookupB clearB addB
                     previousV allV
                     status
                     newQSO dxccGrid gridGrid
                     antennas modes
                     mainWindow
                     contestDlg
                     contestMenu

loadFromGlade :: Config -> IO (Widgets, CWidgets)
loadFromGlade conf = do
    -- Read in the glade file.
    builder <- builderNew
    builderAddFromFile builder "data/slog.ui"

    -- Now that we have a builder, we can build a couple combo boxes that cannot be
    -- specified in glade and then add those into the Widgets record.  That will make
    -- it easier to deal with everywhere else.
    (antennaCombo, modeCombo) <- buildCombos builder
    widgets <- loadWidgets builder antennaCombo modeCombo
    cWidgets <- loadContestWidgets builder

    return (widgets, cWidgets)
 where
    buildCombos :: Builder -> IO (ComboBox, ComboBox)
    buildCombos builder = do
        table <- builderGetObject builder castToTable "newQSOGrid"
        antennaCombo <- addAntennas table conf
        modeCombo <- addModes table
        return (antennaCombo, modeCombo)

initContestDialog :: CWidgets -> IO ()
initContestDialog widgets = do
    -- Pack a combo box with a list of contest possibilities into the table.
    combo <- comboBoxNewText
    mapM_ (comboBoxAppendText combo . T.pack) ["ARRL VHF/UHF Contest",
                                               "ARRL Sweepstakes",
                                               "CQ WW DX",
                                               "Generic Grid-Based Contest",
                                               "Generic Serial-Based Contest"]
    set combo [ comboBoxActive := 4 ]
    set (cwNotebook widgets) [ notebookPage := 1 ]

    boxPackStart (cwBox widgets) combo PackNatural 0
    boxReorderChild (cwBox widgets) combo 3

    -- Then hook up a signal handler to only make it appear if contest mode is enabled.
    on (cwEnable widgets) toggled $ do active <- get (cwEnable widgets) toggleButtonActive
                                       set combo [ widgetVisible := active ]
                                       set (cwNotebook widgets) [ widgetVisible := active ]

    -- And this signal handler tells us which set of entries to display based on which
    -- item in the combo is chosen.
    on combo changed $ get combo comboBoxActive >>= \case
                           1 -> set (cwNotebook widgets) [ notebookPage := 2 ]
                           2 -> set (cwNotebook widgets) [ notebookPage := 3 ]
                           3 -> set (cwNotebook widgets) [ notebookPage := 0 ]
                           4 -> set (cwNotebook widgets) [ notebookPage := 1 ]
                           _ -> set (cwNotebook widgets) [ notebookPage := 0 ]

    return ()

-- This function is called when the Clear button is clicked in order to blank out the
-- UI and prepare it for starting over.  Expected user behavior is that Clear is for
-- when you've entered bad information and need to try again (missed a QSO, etc.) and
-- quitting the application is for when you really want to quit.
clearUI :: IORef PState -> IO ()
clearUI state = do
    widgets <- readState state psWidgets
    contestMode <- readState state psContestMode

    -- Blank out most of the text entry widgets.  Mode we want to set back to SSB, and
    -- frequency/rx frequency we want to leave alone.  This makes it easier to operate
    -- as the station with a pile up.  However if we are in contest mode, we don't want
    -- to blank out much at all.
    mapM_ (`set` [ entryText := "" ])
          (if contestMode then [wCall widgets, wDate widgets, wTime widgets]
                          else [wCall widgets, wRSTRcvd widgets, wRSTSent widgets, wXCRcvd widgets,
                                wXCSent widgets, wDate widgets, wTime widgets])
    set (wMode widgets) [ comboBoxActive := 5 ]

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
    setDateTime w = sequence_ [theDate >>= entrySetText (wDate w),
                               theTime >>= entrySetText (wTime w)]

--
-- THE MAIN PROGRAM
--

runGUI :: IORef PState -> IO ()
runGUI state = do
    -- Install a bunch of signal handlers.
    addSignalHandlers state

    -- Initialize the widgets to their first state.
    clearUI state

    withState state $ \ps -> do
        -- Populate some other dialogs we'll need.
        initContestDialog (psCWidgets ps)

        -- Start up the UI.
        widgetShowAll (wMainWindow . psWidgets $ ps)

    mainGUI

main :: IO ()
main = do
    initGUI

    -- Read in the config file.
    conf <- readConfig

    (widgets, cWidgets) <- loadFromGlade conf

    -- Create the previous QSOs view store but leave it empty.
    previousStore <- listStoreNew ([] :: [DisplayRow])
    initTreeView previousStore (wPreviousView widgets)

    -- Create the all QSOs view store right now, but this one we want to populate immediately.
    allStore <- listStoreNew ([] :: [DisplayRow])
    allQSOs <- getAllQSOs $ confDB conf
    initTreeView allStore (wAllView widgets)
    populateTreeView allStore allQSOs

    -- Now we have enough data to create the record that will be the program state.  Let's
    -- just pretend this isn't a big global data structure.
    ps <- newState PState { psConf = conf,
                            psWidgets = widgets,
                            psCWidgets = cWidgets,
                            psPrevStore = previousStore,
                            psAllStore = allStore,
                            psContestMode = False,
                            psContestVal = mkNoneContest "" }
    runGUI ps
