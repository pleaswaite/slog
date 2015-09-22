{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind -fno-warn-wrong-do-bind #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}

import           Control.Applicative((<$>), (<*))
import           Control.Conditional(ifM, unlessM)
import           Control.Exception(bracket_)
import           Control.Monad((>=>), liftM, void, when)
import           Data.Char(isAlphaNum)
import           Data.IORef(IORef)
import           Data.List(isSuffixOf)
import qualified Data.List as L(lookup)
import           Data.Maybe(fromJust, fromMaybe, isJust, isNothing)
import           Control.Monad.Trans(liftIO)
import           Data.Monoid(First(..), getFirst, mconcat)
import qualified Data.Text as T
import           Data.Time.Clock(UTCTime(..), getCurrentTime)
import           Data.Time.Format(formatTime)
import           Graphics.UI.Gtk hiding(disconnect)
import           Prelude hiding(lookup)
import           System.Locale(defaultTimeLocale)

import           Slog.DB
import           Slog.DXCC(DXCC(dxccEntity), entityFromID, idFromName)
import           Slog.Formats.ADIF.Types(Band(..), Mode)
import           Slog.Formats.ADIF.Utils(freqToBand)
import           Slog.Lookup.Lookup(RadioAmateur(..), RAUses(Yes), login, lookupCall)
import qualified Slog.Rigctl.Commands.Ask as Ask
import qualified Slog.Rigctl.Commands.Tell as Tell
import           Slog.Rigctl.Rigctl(ask, isRigctldRunning, runRigctld)
import           Slog.Utils
import           Slog.QSO(Confirmation(..), QSO(..), isConfirmed)

import ToolLib.Config

import Contest
import State
import Types
import UI(comboBoxSetActiveText, listStoreIndexOf)

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
-- WORKING WITH RIGCTL
--

getFreqs :: IO (Maybe Double, Maybe Double)
getFreqs = do
    freq <- ask Ask.Frequency >>= \case
                Right (Tell.Frequency f) -> return $ Just $ fromInteger f / 1000000
                _                        -> return Nothing
    rxFreq <- ask Ask.SplitFrequency >>= \case
                  Right (Tell.SplitFrequency f) -> return $ Just $ fromInteger f / 1000000
                  _                             -> return Nothing

    if rxFreq == freq then return (freq, Nothing) else return (freq, rxFreq)

--
-- WORKING WITH CALL SIGNS
--

lookup :: String -> String -> String -> IO (Maybe RadioAmateur)
lookup call user pass =
    login user pass >>= maybe (return Nothing) (lookupCall call)

--
-- WORKING WITH COMBO BOXES
--

loadAntennas :: ComboBox -> Config -> Maybe QTH -> IO ()
loadAntennas combo Config{..} qth = do
    -- Clear out the antenna store.  This could potentially change every time the
    -- QTH is changed.
    store <- comboBoxGetModelText combo
    listStoreClear store

    -- Add the antennas for the given QTH, or "Unknown" if none was provided.
    let antennas = maybe ["Unknown"] getAntennas qth
    mapM_ (comboBoxAppendText combo . T.strip . T.pack) antennas

    -- Set the default antenna to whatever is given by the config file.
    comboBoxSetActiveText combo (T.pack $ maybe "Unknown" qthDefaultAntenna qth)

    widgetShowAll combo

loadModes :: ComboBox -> Config -> IO ()
loadModes combo Config{..} = do
    -- Add all the modes we care about.
    mapM_ (comboBoxAppendText combo . T.pack) ["AM", "CW","JT65", "JT9", "FM", "PSK31", "RTTY", "SSB"]

    -- Set the default mode to whatever is given by the config flie.
    comboBoxSetActiveText combo (T.pack confDefaultMode)

    widgetShowAll combo

loadQTHs :: ComboBox -> Config -> IO ()
loadQTHs combo Config{..} = do
    -- Add all the QTHs given in the config file.
    mapM_ (comboBoxAppendText combo . T.pack)
          (map fst confQTHs)

    -- Set the default QTH to whatever is given by the config file.
    comboBoxSetActiveText combo (T.pack confDefaultQTH)

    widgetShowAll combo

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
addEntityCheck Widgets{..} band | band == Just Band160M = addCheckToTable wDXCCGrid 0  2
                                | band == Just Band80M  = addCheckToTable wDXCCGrid 1  2
                                | band == Just Band60M  = addCheckToTable wDXCCGrid 2  2
                                | band == Just Band40M  = addCheckToTable wDXCCGrid 3  2
                                | band == Just Band30M  = addCheckToTable wDXCCGrid 4  2
                                | band == Just Band20M  = addCheckToTable wDXCCGrid 5  2
                                | band == Just Band17M  = addCheckToTable wDXCCGrid 6  2
                                | band == Just Band15M  = addCheckToTable wDXCCGrid 7  2
                                | band == Just Band12M  = addCheckToTable wDXCCGrid 8  2
                                | band == Just Band10M  = addCheckToTable wDXCCGrid 9  2
                                | band == Just Band6M   = addCheckToTable wDXCCGrid 10 2
                                | otherwise             = return ()

addGridCheck :: Widgets -> Maybe Band -> IO ()
addGridCheck Widgets{..} band | band == Just Band6M        = addCheckToTable wGridGrid 0 2
                              | band == Just Band2M        = addCheckToTable wGridGrid 1 2
                              | band == Just Band1Point25M = addCheckToTable wGridGrid 2 2
                              | band == Just Band70CM      = addCheckToTable wGridGrid 3 2
                              | band == Just Band33CM      = addCheckToTable wGridGrid 4 2
                              | band == Just Band23CM      = addCheckToTable wGridGrid 5 2
                              | otherwise                  = return ()

addStateCheck :: Widgets -> Maybe Band -> IO ()
addStateCheck Widgets{..} band | band == Just Band160M      = addCheckToTable wStateGrid 0  2
                               | band == Just Band80M       = addCheckToTable wStateGrid 1  2
                               | band == Just Band60M       = addCheckToTable wStateGrid 2  2
                               | band == Just Band40M       = addCheckToTable wStateGrid 3  2
                               | band == Just Band30M       = addCheckToTable wStateGrid 4  2
                               | band == Just Band20M       = addCheckToTable wStateGrid 5  2
                               | band == Just Band17M       = addCheckToTable wStateGrid 6  2
                               | band == Just Band15M       = addCheckToTable wStateGrid 7  2
                               | band == Just Band12M       = addCheckToTable wStateGrid 8  2
                               | band == Just Band10M       = addCheckToTable wStateGrid 9  2
                               | band == Just Band6M        = addCheckToTable wStateGrid 10 2
                               | band == Just Band2M        = addCheckToTable wStateGrid 11 2
                               | band == Just Band1Point25M = addCheckToTable wStateGrid 12 2
                               | band == Just Band70CM      = addCheckToTable wStateGrid 13 2
                               | otherwise                  = return ()

antennaForFreq :: String -> Config -> String -> String
antennaForFreq qthName Config{..} text = let
    qth = L.lookup qthName confQTHs
 in
    case stringToDouble text >>= freqToBand of
        -- The frequency given doesn't fall in any ham band.  Return the default antenna for the
        -- given QTH.  If there's no default QTH, just return "Unknown".
        Nothing -> maybe "Unknown" qthDefaultAntenna qth
        -- The frequency given does fall into a ham band.  Look up the antenna for that band in
        -- the QTH's map.  If there's no antenna given, use the QTH's default.  If there's no
        -- default QTH, just return "Unknown".  Lots can go wrong here.
        Just b  -> maybe "Unknown" (\qth' -> fromMaybe (qthDefaultAntenna qth') (L.lookup b $ qthAntennaMap qth')) qth

modeForFreq :: Config -> String -> String
modeForFreq Config{..} text =
    case stringToDouble text >>= freqToBand of
        Nothing -> confDefaultMode
        Just b  -> fromMaybe confDefaultMode (L.lookup b confModeMap)

blockUI :: Widgets -> Bool -> IO ()
blockUI Widgets{..} b = do
    -- The "not" is here because it reads a lot better to write "blockUI True" than to pass
    -- the correct value.
    set wCurrent [ widgetSensitive := not b ]
    mapM_ (\widget -> set widget [ widgetSensitive := not b ])
          [wLookup, wClear, wAdd]

clearChecks :: Widgets -> IO ()
clearChecks Widgets{..} = do
    mapM_ (\widget -> set widget [ widgetSensitive := False ])
          [wDXCC, wGrid, wState]
    mapM_ (\cont -> containerForeach cont (removeImage cont))
          [wNewQSOGrid, wDXCCGrid, wGridGrid, wStateGrid]
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

    -- STATE
    stateCol <- newTextColumn store "State" (possiblyEmpty dState)
    treeViewAppendColumn view stateCol

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
dbToDR (_, QSO{..}, c) = DisplayRow { dDate=dashifyDate qDate,
                                      dTime=colonifyTime qTime,
                                      dCall=qCall,
                                      dFreq=qFreq,
                                      dRxFreq=qRxFreq,
                                      dMode=show qMode,
                                      dDXCC=qDXCC,
                                      dState=qState,
                                      dGrid=qGrid,
                                      dXcIn=qXcIn,
                                      dXcOut=qXcOut,
                                      dAntenna=qAntenna,
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
checkCall Widgets{..} = do
    s <- get wCall entryText
    if | T.null s                     -> return $ Just "Call sign is empty."
       | any invalidChar (T.unpack s) -> return $ Just "Call may only contain numbers, letters, and /"
       | otherwise                    -> return Nothing
 where
        invalidChar ch = not (isAlphaNum ch) && ch /= '/'

checkFreq :: Widgets -> IO (Maybe String)
checkFreq widgets@Widgets{..} =
    ifM (rigctlActive widgets)
        (return Nothing)
        (do s <- get wFreq entryText
            return $ T.null s || isNothing (stringToDouble (T.unpack s) >>= freqToBand) <??> "Frequency is empty or does not fall in a ham band.")

checkRxFreq :: Widgets -> IO (Maybe String)
checkRxFreq widgets@Widgets{..} =
    ifM (rigctlActive widgets)
        (return Nothing)
        (do s <- get wRxFreq entryText
            return $ not (T.null s) && isNothing (stringToDouble (T.unpack s) >>= freqToBand) <??> "Rx Frequency does not fall in a ham band.")

checkRxRST :: Widgets -> IO (Maybe String)
checkRxRST Widgets{..} = checkRequiredText wRSTRcvd "Received RST is empty."

checkRST :: Widgets -> IO (Maybe String)
checkRST Widgets{..} = checkRequiredText wRSTSent "Sent RST is empty."

-- FIXME: we could be smarter about this
checkDate :: Widgets -> IO (Maybe String)
checkDate Widgets{..} = checkRequiredText wDate "Date is empty."

-- FIXME: we could be smarter about this
checkTime :: Widgets -> IO (Maybe String)
checkTime Widgets{..} = checkRequiredText wTime "Time is empty."

checkXCIn :: Widgets -> IO (Maybe String)
checkXCIn Widgets{..} = checkRequiredText wXCRcvd "Received exchange is empty in contest mode."

addQSOChecks :: Bool -> [Widgets -> IO (Maybe String)]
addQSOChecks contestMode = allChecks ++ if contestMode then contestChecks else []
 where
    allChecks     = [checkCall, checkFreq, checkRxFreq, checkRxRST, checkRST, checkDate, checkTime]
    contestChecks = [checkXCIn]

--
-- FUNCTIONS FOR QUERYING WIDGET STATE
--

currentActive :: Widgets -> IO Bool
currentActive Widgets{..} = get wCurrent toggleButtonActive

contestActive :: CWidgets -> IO Bool
contestActive CWidgets{..} = get cwEnable toggleButtonActive

rigctlActive :: Widgets -> IO Bool
rigctlActive Widgets{..} = get wRigctl toggleButtonActive

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
    err <- getFirst . mconcat . map First <$> mapM ($ widgets) (addQSOChecks contestMode)

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
        (freq, rxFreq) <- getFreq widgets
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
                      qFreq     = fromJust freq,
                      qRxFreq   = rxFreq,
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
    getDate widgets =
        ifM (currentActive widgets)
            theDate
            (get (wDate widgets) entryText)

    getTime :: Widgets -> IO String
    getTime widgets =
        ifM (currentActive widgets)
            theTime
            (get (wTime widgets) entryText)

    getFreq :: Widgets -> IO (Maybe Double, Maybe Double)
    getFreq widgets =
        ifM (rigctlActive widgets)
            getFreqs
            (do f <- get (wFreq widgets) entryText
                rxF <- getMaybe (wRxFreq widgets)
                return (stringToDouble f, maybe Nothing stringToDouble rxF))

-- When the "Use current date & time" button is toggled, change sensitivity on widgets underneath it.
currentToggled :: Widgets -> IO ()
currentToggled widgets@Widgets{..} = do
    active <- currentActive widgets
    set wDateLabel [ widgetSensitive := not active ]
    set wDate      [ widgetSensitive := not active ]
    set wTimeLabel [ widgetSensitive := not active ]
    set wTime      [ widgetSensitive := not active ]

-- When the "Get frequency from radio" button is toggled, change sensitivity on widgets underneath it.
rigctlToggled :: Widgets -> IO ()
rigctlToggled widgets@Widgets{..} = do
    active <- rigctlActive widgets
    running <- isRigctldRunning

    if active && not running then do
        set wRigctl [ toggleButtonActive := False ]
        void $ statusbarPush wStatus 0 "Rigctld is not running."
    else do
        set wFreqLabel      [ widgetSensitive := not active ]
        set wFreq           [ widgetSensitive := not active ]
        set wRxFreqLabel    [ widgetSensitive := not active ]
        set wRxFreq         [ widgetSensitive := not active ]

-- When the "Lookup" button next to the call sign entry is clicked, we want to look that call up
-- in HamQTH and fill in some information on the screen.  This is called as a callback in an idle
-- handler so the lookup can proceed while the UI continues to refresh.
lookupCallsign :: Widgets -> ListStore DisplayRow -> Config -> IO Bool
lookupCallsign widgets@Widgets{..} store Config{..} = do
    call <- uppercase <$> get wCall entryText

    checkCall widgets >>= \case
        Nothing -> do result <- lookup call confQTHUser confQTHPass
                      maybe (void $ statusbarPush wStatus 0 ("Nothing found for callsign " ++ call))
                            (updateUI call)
                            result
        Just s  -> void $ statusbarPush wStatus 0 s

    -- Return false to remove this handler from the main loop.
    return False
 where
    -- The on-disk database location.
    fp = confDB

    updateUI call ra' = do
        statusbarPush wStatus 0 ("Lookup of " ++ call ++ " finished.")

        -- And now that we've discovered something, we can update the UI to reflect what we found.
        -- We start with clearing out the checkmarks.  This is so you can lookup a call, see what
        -- previous contacts may have been made, and then do another lookup without first having
        -- to hit the clear button.
        clearChecks widgets

        -- If they're a LOTW user, put a nice big check mark image in there.
        when (raLOTW ra' == Just Yes) $ addCheckToTable wNewQSOGrid 1 7

        -- Put their call in the label, and then populate the list of previous QSOs we've
        -- had with this station.
        set wPrevious [ widgetSensitive := True, frameLabel := "Previous contacts with " ++ call ]
        qsos <- getQSOsByCall fp call
        populateTreeView store qsos

        -- Put their call in the label, and then add check marks in for DXCC entity
        -- and grid confirmations.
        when (isJust $ raCountry ra') $ do
            set wDXCC [ widgetSensitive := True, frameLabel := (fromJust . raCountry) ra' ++ " status" ]

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
            set wGrid [ widgetSensitive := True, frameLabel := shortGrid ++ " status" ]

            results <- filter confirmed <$> getQSOsByGrid fp shortGrid
            let confirmedBands = map getBand results
            mapM_ (addGridCheck widgets) confirmedBands

        -- And then put their state and checkmarks into that table, too.
        when (isJust $ raUSState ra') $ do
            let state = (fromJust . raUSState) ra'

            set wState [ widgetSensitive := True, frameLabel := state ++ " status" ]

            results <- filter confirmed <$> getQSOsByState fp state
            let confirmedBands = map getBand results
            mapM_ (addStateCheck widgets) confirmedBands
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
                0 -> do serial <- stringToInteger <$> get (cwSweepsSerial cw) entryText
                        prec <- head <$> get (cwSweepsPrec cw) entryText
                        call <- get (cwSweepsCall cw) entryText
                        check <- truncate <$> get (cwSweepsCheck cw) spinButtonValue
                        section <- get (cwSweepsSection cw) entryText
                        return $ mkSweepsContest (Sweeps (fromMaybe 0 serial) prec call check section)

                1 -> mkTenMeterContest <$> get (cwTenState cw) entryText

                2 -> mkGridContest <$> get (cwGridGrid cw) entryText

                3 -> mkZoneContest <$> truncate <$> get (cwZoneZone cw) spinButtonValue

                4 -> do v <- stringToInteger <$> get (cwSerialSerial cw) entryText
                        return $ mkSerialContest (fromMaybe 0 v)
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

runQTHDialog :: IORef PState -> IO ()
runQTHDialog state = do
    currentQTH <- readState state psQTH
    conf <- readState state psConf
    CFGWidgets{..} <- readState state psCFGWidgets

    -- Set the dialog to display the current QTH and its call sign.
    comboBoxSetActiveText cfgQTHCombo (T.pack currentQTH)

    let call = maybe "Unknown" qthCall (L.lookup currentQTH $ confQTHs conf)
    set cfgQTHCall [ labelText := call]

    dlg <- readState state (wQTHDlg . psWidgets)
    dialogRun dlg
    widgetHide dlg

    -- Change the current QTH in the program state so that newly added QSOs will be correct
    -- in the database.  If something went wrong and there's no active text in the combo
    -- (no idea how that could happen), just use the old one.
    newQTH <- maybe currentQTH T.unpack <$> comboBoxGetActiveText cfgQTHCombo
    modifyState state (\v -> v { psQTH = newQTH })

-- When a message is pushed into the status bar, check it to see if it's the message that'd be
-- written when a new QSO has been added to the database.  If so, grab that QSO and add it to the
-- all QSOs view.  This is kind of roundabout when we could just do this right after adding the QSO,
-- but that would mean passing the store all over the place.
updateAllQSOsView :: ListStore DisplayRow -> Config -> IO ()
updateAllQSOsView store Config{..} = do
    result <- getLatestQSO confDB
    listStorePrepend store $ dbToDR result

--
-- INIT UI
--

addSignalHandlers :: IORef PState -> IO ()
addSignalHandlers state = do
    allStore <- readState state psAllStore
    conf <- readState state psConf
    prevStore <- readState state psPrevStore
    w@Widgets{..} <- readState state psWidgets
    CFGWidgets{..} <- readState state psCFGWidgets
    qthName <- readState state psQTH

    -- Install a bunch of regular signal handlers.
    onDestroy wMainWindow mainQuit

    on wCurrent toggled         (currentToggled w)
    on wRigctl  toggled         (rigctlToggled w)
    on wClear   buttonActivated (do clearUI state
                                    populateTreeView prevStore []
                                    widgetGrabFocus wCall)
    on wAdd     buttonActivated (do addQSOFromUI state
                                    widgetGrabFocus wCall)
    on wLookup  buttonActivated (void $ idleAdd (bracket_ (blockUI w True)
                                                          (blockUI w False)
                                                          (lookupCallsign w prevStore conf <* widgetGrabFocus wCall))
                                                 priorityDefaultIdle)

    -- This signal is how we watch for a new QSO being added to the database and then
    -- updating the view of all QSOs.  This is to prevent having to pass stores all
    -- around (even though we're doing that to get it into this function already).
    on wStatus  textPushed (\_ s -> when (" added to database." `isSuffixOf` s) $ do
                                         updateAllQSOsView allStore conf
                                         populateTreeView prevStore [])

    -- These signal handlers are for menu items.
    on wContestMenu actionActivated (runContestDialog state)
    on wQTHMenu     actionActivated (runQTHDialog state)

    -- When the QTH is changed, change the displayed call sign.
    on cfgQTHCombo  changed         (do let call = maybe "Unknown" qthCall (L.lookup qthName $ confQTHs conf)
                                        set cfgQTHCall [ labelText := call])

    -- When focus leaves the frequency text entry, change the selected antenna and mode to
    -- match.  Note that this enforces that if you want to specify a different antenna or
    -- mode, you have to enter the frequency first.  That's probably what people will do
    -- most of the time though.
    on wFreq    focusOutEvent $ tryEvent $ liftIO $ do
        text <- get wFreq entryText
        comboBoxSetActiveText wAntenna (T.pack $ antennaForFreq qthName conf text)
        comboBoxSetActiveText wMode    (T.pack $ modeForFreq conf text)

    return ()

loadConfigWidgets :: Builder -> IO CFGWidgets
loadConfigWidgets builder = do
    [qthCombo] <- mapM (builderGetObject builder castToComboBox) ["qthCombo"]
    void $ comboBoxSetModelText qthCombo

    [callSignLabel] <- mapM (builderGetObject builder castToLabel) ["qthCallSignLabel"]

    return $ CFGWidgets qthCombo
                        callSignLabel

loadContestWidgets :: Builder -> IO CWidgets
loadContestWidgets builder = do
    [box] <- mapM (builderGetObject builder castToBox) ["contestDialogBox"]

    [gridGrid, serialSerial, sweepsSerial, sweepsPrec, sweepsCall,
     sweepsSection, tenState] <- mapM (builderGetObject builder castToEntry)
                                        ["gridGridEntry", "serialSerialEntry", "sweepsSerialEntry",
                                         "sweepsPrecEntry", "sweepsCallEntry", "sweepsSectionEntry",
                                         "tenStateEntry"]

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
                      tenState

loadWidgets :: Builder -> IO Widgets
loadWidgets builder = do
    [call, freq, rxFreq, rst_rcvd,
     rst_sent, xc_rcvd, xc_sent, date, time] <- mapM (builderGetObject builder castToEntry)
                                                     ["callEntry", "freqEntry", "rxFreqEntry",
                                                      "rstRcvdEntry", "rstSentEntry", "xcRcvdEntry",
                                                      "xcSentEntry", "dateEntry", "timeEntry"]

    [current, rigctl] <- mapM (builderGetObject builder castToCheckButton) ["useCurrentDateButton", "useRigctlButton"]
    [dateLabel, timeLabel, freqLabel, rxFreqLabel] <- mapM (builderGetObject builder castToLabel)
                                                           ["dateLabel", "timeLabel", "freqLabel", "rxFreqLabel"]

    [previous, dxcc, grid, state] <- mapM (builderGetObject builder castToFrame) ["previousFrame", "dxccFrame", "gridFrame", "stateFrame"]

    [lookupB, clearB, addB] <- mapM (builderGetObject builder castToButton) ["lookupButton", "clearButton", "addButton"]

    [previousV, allV] <- mapM (builderGetObject builder castToTreeView) ["previousTreeView", "allTreeView"]

    [status] <- mapM (builderGetObject builder castToStatusbar) ["statusBar"]

    [newQSO, dxccGrid, gridGrid, stateGrid] <- mapM (builderGetObject builder castToTable) ["newQSOGrid", "dxccGrid", "gridGrid", "stateGrid"]

    [antennas, modes] <- mapM (builderGetObject builder castToComboBox) ["antennaCombo", "modeCombo"]
    void $ comboBoxSetModelText antennas
    void $ comboBoxSetModelText modes

    [mainWindow] <- mapM (builderGetObject builder castToWindow) ["window1"]
    [contestDlg, qthDlg] <- mapM (builderGetObject builder castToDialog) ["contestDialog", "configQTHDialog"]

    [contestMenu, qthMenu] <- mapM (builderGetObject builder castToAction) ["contestMenuItem", "qthMenuItem"]

    return $ Widgets call freqLabel freq rxFreqLabel rxFreq
                     rst_rcvd rst_sent xc_rcvd xc_sent
                     current rigctl
                     dateLabel date timeLabel time
                     previous dxcc grid state
                     lookupB clearB addB
                     previousV allV
                     status
                     newQSO dxccGrid gridGrid stateGrid
                     antennas modes
                     mainWindow
                     contestDlg qthDlg
                     contestMenu qthMenu

loadFromGlade :: IO (Widgets, CWidgets, CFGWidgets)
loadFromGlade = do
    -- Read in the glade file.
    builder <- builderNew
    builderAddFromFile builder "data/slog.ui"

    widgets <- loadWidgets builder
    cWidgets <- loadContestWidgets builder
    cfgWidgets <- loadConfigWidgets builder

    return (widgets, cWidgets, cfgWidgets)

initContestDialog :: CWidgets -> IO ()
initContestDialog CWidgets{..} = do
    -- Pack a combo box with a list of contest possibilities into the table.
    combo <- comboBoxNewText
    mapM_ (comboBoxAppendText combo . T.pack) ["ARRL Sweepstakes",
                                               "ARRL Ten Meter Contest",
                                               "ARRL VHF/UHF Contest",
                                               "CQ WW DX",
                                               "Generic Grid-Based Contest",
                                               "Generic Serial-Based Contest"]
    set combo [ comboBoxActive := 5 ]
    set cwNotebook [ notebookPage := 4 ]

    boxPackStart cwBox combo PackNatural 0
    boxReorderChild cwBox combo 3

    -- Then hook up a signal handler to only make it appear if contest mode is enabled.
    on cwEnable toggled $ do active <- get cwEnable toggleButtonActive
                             set combo [ widgetVisible := active ]
                             set cwNotebook [ widgetVisible := active ]

    -- And this signal handler tells us which set of entries to display based on which
    -- item in the combo is chosen.
    on combo changed $ get combo comboBoxActive >>= \case
                           0 -> set cwNotebook [ notebookPage := 0 ]         -- arrl sweepstakes
                           1 -> set cwNotebook [ notebookPage := 1 ]         -- arrl ten meter contest
                           2 -> set cwNotebook [ notebookPage := 2 ]         -- arrl vhf/uhf contest
                           3 -> set cwNotebook [ notebookPage := 3 ]         -- cq ww dx
                           4 -> set cwNotebook [ notebookPage := 2 ]         -- generic grid-based contest
                           5 -> set cwNotebook [ notebookPage := 4 ]         -- generic serial-based contest

    return ()

-- This function is called when the Clear button is clicked in order to blank out the
-- UI and prepare it for starting over.  Expected user behavior is that Clear is for
-- when you've entered bad information and need to try again (missed a QSO, etc.) and
-- quitting the application is for when you really want to quit.
clearUI :: IORef PState -> IO ()
clearUI state = do
    widgets <- readState state psWidgets
    contestMode <- readState state psContestMode

    rigctlRunning <- isRigctldRunning

    -- Blank out most of the text entry widgets.  This makes it easier to operate as the
    -- station with a pile up.  However if we are in contest mode, we don't want to blank
    -- out much at all.
    mapM_ (`set` [ entryText := "" ])
          (if contestMode then [wCall widgets, wDate widgets, wTime widgets, wXCRcvd widgets]
                          else [wCall widgets, wRSTRcvd widgets, wRSTSent widgets, wXCRcvd widgets,
                                wXCSent widgets, wDate widgets, wTime widgets])

    when rigctlRunning $ do
        (freq, rxFreq) <- getFreqs
        set (wFreq widgets)     [ entryText := maybe "" show freq ]
        set (wRxFreq widgets)   [ entryText := maybe "" show rxFreq ]

    -- Set the current date/time checkbox back to active.
    set (wCurrent widgets) [ toggleButtonActive := True ]
    set (wRigctl widgets)  [ toggleButtonActive := rigctlRunning ]

    -- Set the titles on the various frames to something boring.
    set (wPrevious widgets) [ widgetSensitive := False, frameLabel := "Previous contacts with remote station" ]
    set (wDXCC widgets)     [ widgetSensitive := False, frameLabel := "Entity status" ]
    set (wGrid widgets)     [ widgetSensitive := False, frameLabel := "Grid status" ]
    set (wState widgets)    [ widgetSensitive := False, frameLabel := "State status" ]

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
    conf@Config{..} <- readConfig
    initDB confDB

    -- Try to start rigctld now so we can ask the radio for frequency.  There's no
    -- guarantee it will actually start (what if the radio's not on yet?) so we have
    -- to check all over the place anyway.
--    unlessM isRigctldRunning $
--        runRigctld (confRadioModel conf) (confRadioDev conf)

    (widgets, cWidgets, cfgWidgets) <- loadFromGlade

    -- Initialize the modes combo and the antenna combo.  The modes combo doesn't change,
    -- but the antenna combo can change whenever the QTH is changed.
    loadModes (wMode widgets) conf
    loadAntennas (wAntenna widgets) conf (L.lookup confDefaultQTH confQTHs)

    -- And then load up the QTH config dialog.
    loadQTHs (cfgQTHCombo cfgWidgets) conf

    -- Create the previous QSOs view store but leave it empty.
    previousStore <- listStoreNew ([] :: [DisplayRow])
    initTreeView previousStore (wPreviousView widgets)

    -- Create the all QSOs view store right now, but this one we want to populate immediately.
    allStore <- listStoreNew ([] :: [DisplayRow])
    allQSOs <- getAllQSOs confDB
    initTreeView allStore (wAllView widgets)
    populateTreeView allStore allQSOs

    -- Now we have enough data to create the record that will be the program state.  Let's
    -- just pretend this isn't a big global data structure.
    ps <- newState PState { psConf = conf,
                            psWidgets = widgets,
                            psCWidgets = cWidgets,
                            psCFGWidgets = cfgWidgets,
                            psPrevStore = previousStore,
                            psAllStore = allStore,
                            psContestMode = False,
                            psContestVal = mkNoneContest "",
                            psQTH = confDefaultQTH }
    runGUI ps
