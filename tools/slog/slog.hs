{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind -fno-warn-wrong-do-bind #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}

import           Control.Applicative((<$>), (<*))
import           Control.Arrow((&&&))
import           Control.Conditional((<||>), ifM, notM, whenM)
import           Control.Exception(finally)
import           Control.Monad(liftM, void, when)
import           Data.Char(isAlphaNum)
import           Data.Foldable(forM_)
import           Data.IORef(IORef)
import           Data.List(isSuffixOf)
import           Data.Maybe(fromJust, fromMaybe, isJust, isNothing)
import           Control.Monad.Trans(liftIO)
import           Data.Monoid(First(..), getFirst, mconcat)
import qualified Data.Text as T
import           Data.Time.Clock(UTCTime(..), getCurrentTime)
import           Data.Time.Format(formatTime)
import           Graphics.UI.Gtk hiding(disconnect)
import           System.Locale(defaultTimeLocale)

import           Slog.DB
import           Slog.DXCC(DXCC(dxccEntity), entityFromID)
import           Slog.Formats.ADIF.Types(Mode)
import           Slog.Formats.ADIF.Utils(freqToBand)
import           Slog.Lookup.Lookup(RadioAmateur(..), RAUses(Yes), login, lookupCall, lookupCallD)
import qualified Slog.Rigctl.Commands.Ask as Ask
import qualified Slog.Rigctl.Commands.Tell as Tell
import           Slog.Rigctl.Rigctl(RigctlSupport(..), ask, isRigctldRunning, killRigctld, rigctlSupportForModel, runRigctld)
import           Slog.Utils
import           Slog.QSO(Confirmation(..), QSO(..), isConfirmed)

import ToolLib.Config

import Cmdline(Options(..), processArgs)
import Contest(contestNext, contestStr, mkNoneContest)
import Dialogs.Contest(initContestDialog, loadContestWidgets, runContestDialog)
import Dialogs.QTH(loadQTHWidgets, runQTHDialog)
import Progress(addEntityCheck, addGridCheck, addStateCheck, clearChecks, populateAllTables)
import State
import Types
import UI(addCheckToTable, comboBoxSetActiveText)

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

getFreqsFromRigctl :: Maybe RigctlSupport -> IO (Maybe Double, Maybe Double)
getFreqsFromRigctl rs =
    -- No RigctlSupport record was given, so we must assume rigctl is not running.
    -- Thus return Nothing for both.  I don't know how you'd get here, but here we are.
    if isNothing rs then return (Nothing, Nothing)
    -- If the radio does not support getting the VFO status, we cannot tell if we're
    -- running split or not.  Thus return Nothing in that case, or the frequency if we
    -- know the radio is reliable.
    else do freq <- askFreq
            rxFreq <- if rsSupportsVFOCheck (fromJust rs) then askRxFreq else return Nothing
            if rxFreq == freq then return (freq, Nothing) else return (freq, rxFreq)
 where
    askFreq = ask Ask.Frequency >>= \case
        Right (Tell.Frequency f) -> return $ Just $ fromInteger f / 1000000
        _                        -> return Nothing

    askRxFreq = ask Ask.SplitFrequency >>= \case
        Right (Tell.SplitFrequency f) -> return $ Just $ fromInteger f / 1000000
        _                             -> return Nothing


updateFreqsFromRigctl :: Widgets -> Maybe RigctlSupport -> IO ()
updateFreqsFromRigctl Widgets{..} rs = do
    (freq, rxFreq) <- getFreqsFromRigctl rs
    set wFreq   [ entryText := maybe "" show freq ]
    set wRxFreq [ entryText := maybe "" show rxFreq ]

--
-- WORKING WITH CALL SIGNS
--

getRA :: String -> String -> String -> IO (Maybe RadioAmateur)
getRA call user pass =
    -- If doing the full lookup via lookupCall fails, use lookupCallD to get
    -- the most basic information.  Either way, return a RadioAmateur on success.
    login user pass >>= \case
        Nothing     -> lookupCallD call
        Just sid    -> do result <- lookupCall call sid
                          if isJust result then return result
                          else lookupCallD call

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
    let antennas = maybe ["Unknown"] qthAntennas qth
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

antennaForFreq :: String -> Config -> String -> String
antennaForFreq qthName Config{..} text = let
    qth = lookup qthName confQTHs
 in
    case stringToDouble text >>= freqToBand of
        -- The frequency given doesn't fall in any ham band.  Return the default antenna for the
        -- given QTH.  If there's no default QTH, just return "Unknown".
        Nothing -> maybe "Unknown" qthDefaultAntenna qth
        -- The frequency given does fall into a ham band.  Look up the antenna for that band in
        -- the QTH's map.  If there's no antenna given, use the QTH's default.  If there's no
        -- default QTH, just return "Unknown".  Lots can go wrong here.
        Just b  -> maybe "Unknown" (\qth' -> fromMaybe (qthDefaultAntenna qth') (lookup b $ qthAntennaMap qth')) qth

modeForFreq :: Config -> String -> String
modeForFreq Config{..} text =
    case stringToDouble text >>= freqToBand of
        Nothing -> confDefaultMode
        Just b  -> fromMaybe confDefaultMode (lookup b confModeMap)

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
    qthName <- readState state psQTH

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
        ra <- getRA call (confQTHUser conf) (confQTHPass conf)

        -- And then a bunch of annoying UI field grabbing.
        date <- undashifyDate <$> getDate widgets
        time <- uncolonifyTime <$> getTime widgets
        (freq, rxFreq) <- withStateElement state psRigSupport (getFreq widgets)
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
                      qDXCC     = maybe Nothing raADIF ra,
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
                      qAntenna  = fmap T.unpack antenna,
                      qMyCall   = qthCall . fromJust $ lookup qthName (confQTHs conf),
                      qMyQTH    = qthName }

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

    getFreq :: Widgets -> Maybe RigctlSupport -> IO (Maybe Double, Maybe Double)
    getFreq widgets rs =
        ifM (rigctlActive widgets)
            (getFreqsFromRigctl rs)
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
-- in HamQTH and fill in some information on the screen.
lookupCallsign :: Widgets -> ListStore DisplayRow -> Config -> IO ()
lookupCallsign widgets@Widgets{..} store Config{..} = do
    call <- uppercase <$> get wCall entryText

    checkCall widgets >>= \case
        Nothing -> do result <- getRA call confQTHUser confQTHPass
                      maybe (void $ statusbarPush wStatus 0 ("Nothing found for callsign " ++ call))
                            (updateUI call)
                            result
        Just s  -> void $ statusbarPush wStatus 0 s
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
        when (raLOTW ra' == Just Yes) $ addCheckToTable wNewQSOGrid 1 8

        -- Put their call in the label, and then populate the list of previous QSOs we've
        -- had with this station.
        set wPrevious [ widgetSensitive := True, frameLabel := "Previous contacts with " ++ call ]
        qsos <- getQSOsByCall fp call
        populateTreeView store qsos

        -- Put their call in the label, and then add check marks in for DXCC entity
        -- and grid confirmations.
        when (isJust $ raCountry ra') $ do
            set wDXCC [ widgetSensitive := True, frameLabel := (fromJust . raCountry) ra' ++ " status" ]

            let dxcc = raADIF ra'
            when (isJust dxcc) $ do
                -- Get a list of all QSOs we've had with this entity, narrow it down to just what's
                -- been confirmed, and then put checkmarks in where appropriate.
                results <- filter confirmed <$> getQSOsByDXCC fp (fromInteger $ fromJust dxcc)
                let confirmations = map (getBand &&& getMode) results
                mapM_ (addEntityCheck widgets) confirmations

        -- FIXME: This is not necessarily the right grid.  We really need to know the grid they're
        -- in first, since a station could be in a different grid than their QTH.  We won't know that
        -- until asking them personally or looking at the cluster, though.  For now this will have
        -- to do.
        when (isJust $ raGrid ra') $ do
            set wGrid [ widgetSensitive := True, frameLabel := shortGrid ++ " status" ]

            results <- filter confirmed <$> getQSOsByGrid fp shortGrid
            let confirmations = map (getBand &&& getMode) results
            mapM_ (addGridCheck widgets) confirmations

        -- And then put their state and checkmarks into that table, too.
        when (isJust $ raUSState ra') $ do
            let state = (fromJust . raUSState) ra'

            set wState [ widgetSensitive := True, frameLabel := state ++ " status" ]

            results <- filter confirmed <$> getQSOsByState fp state
            let confirmations = map (getBand &&& getMode) results
            mapM_ (addStateCheck widgets) confirmations
     where
        confirmed (_, _, c) = isJust $ qLOTW_RDate c

        getBand (_, q, _) = freqToBand $ qFreq q

        getMode (_, q, _) = qMode q

        shortGrid = uppercase $ take 4 $ fromJust $ raGrid ra'

-- When a message is pushed into the status bar, check it to see if it's the message that'd be
-- written when a new QSO has been added to the database.  If so, grab that QSO and add it to the
-- all QSOs view.  This is kind of roundabout when we could just do this right after adding the QSO,
-- but that would mean passing the store all over the place.
updateAllQSOsView :: Config -> ListStore DisplayRow -> IO ()
updateAllQSOsView Config{..} store = do
    result <- getLatestQSO confDB
    listStorePrepend store $ dbToDR result

--
-- INIT UI
--

addSignalHandlers :: IORef PState -> IO ()
addSignalHandlers state = do
    prevStore <- readState state psPrevStore
    conf <- readState state psConf
    w@Widgets{..} <- readState state psWidgets
    QTHWidgets{..} <- readState state psQTHWidgets
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
    on wLookup  buttonActivated (lookupCallsign w prevStore conf <* widgetGrabFocus wCall)

    -- This signal is how we watch for a new QSO being added to the database and then
    -- updating the view of all QSOs.  This is to prevent having to pass stores all
    -- around (even though we're doing that to get it into this function already).
    on wStatus  textPushed (\_ s -> when (" added to database." `isSuffixOf` s) $ do
                                         withStateElement_ state psAllStore (updateAllQSOsView conf)
                                         populateTreeView prevStore [])

    -- These signal handlers are for menu items.
    on wContestMenu actionActivated (runContestDialog state)
    on wQTHMenu     actionActivated (do runQTHDialog state
                                        withStateElement_ state psQTH (\q -> loadAntennas wAntenna conf (lookup q $ confQTHs conf)))

    -- When the QTH is changed, change the displayed call sign.
    on qthCombo changed             (do let call = maybe "Unknown" qthCall (lookup qthName $ confQTHs conf)
                                        set qthCallLabel [ labelText := call])

    -- When focus leaves the frequency text entry, change the selected antenna and mode to
    -- match.  Note that this enforces that if you want to specify a different antenna or
    -- mode, you have to enter the frequency first.  That's probably what people will do
    -- most of the time though.
    on wFreq    focusOutEvent $ tryEvent $ liftIO $ do
        text <- get wFreq entryText
        comboBoxSetActiveText wAntenna (T.pack $ antennaForFreq qthName conf text)
        comboBoxSetActiveText wMode    (T.pack $ modeForFreq conf text)

    -- If rigctl is running, we can ask it for the frequency and then update the UI with
    -- that instead of leaving it at whatever frequency was set the very first time we
    -- ran.  This looks more responsive.  We only check that rigctl is running once, before
    -- adding the handler.  We then have to verify that the rigctl checkbox is active every
    -- time before updating the UI.
    void $ timeoutAdd (do whenM (rigctlActive w) $ withStateElement_ state psRigSupport (updateFreqsFromRigctl w)
                          return True)
                      350

    return ()

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
                     contestMenu qthMenu

loadFromGlade :: IO (Widgets, CWidgets, QTHWidgets)
loadFromGlade = do
    -- Read in the glade file.
    builder <- builderNew
    builderAddFromFile builder "data/slog.ui"

    widgets <- loadWidgets builder
    cWidgets <- loadContestWidgets builder
    cfgWidgets <- loadQTHWidgets builder

    return (widgets, cWidgets, cfgWidgets)

-- This function is called when the Clear button is clicked in order to blank out the
-- UI and prepare it for starting over.  Expected user behavior is that Clear is for
-- when you've entered bad information and need to try again (missed a QSO, etc.) and
-- quitting the application is for when you really want to quit.
clearUI :: IORef PState -> IO ()
clearUI state = do
    widgets <- readState state psWidgets
    contestMode <- readState state psContestMode

    -- Blank out most of the text entry widgets.  This makes it easier to operate as the
    -- station with a pile up.  However if we are in contest mode, we don't want to blank
    -- out much at all.
    mapM_ (`set` [ entryText := "" ])
          (if contestMode then [wCall widgets, wDate widgets, wTime widgets, wXCRcvd widgets]
                          else [wCall widgets, wRSTRcvd widgets, wRSTSent widgets, wXCRcvd widgets,
                                wXCSent widgets, wDate widgets, wTime widgets])

    -- Set the current date/time checkbox back to active.
    set (wCurrent widgets) [ toggleButtonActive := True ]
    set (wRigctl widgets)  [ toggleButtonActive :=> isRigctldRunning ]

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

    -- Grab command line settings.
    Options{..} <- processArgs

    -- Try to start rigctld now so we can ask the radio for frequency.  There's no
    -- guarantee it will actually start (what if the radio's not on yet?) so we have
    -- to check all over the place anyway.  And then if we've started rigctl, figure
    -- out what the radio supports so we can figure out what to do in the UI.
    --
    -- However, we also allow disabling rigctl support on the command line.  This is
    -- necessary if you want to run slog and some other program (like wsjt or fldigi)
    -- at the same time and rigctl support for your radio is poor.
    pid <- ifM (isRigctldRunning <||> notM (return optRigctl))
               (return Nothing)
               (runRigctld confRadioModel confRadioDev)
    let rs = if isNothing pid then Nothing else Just $ rigctlSupportForModel confRadioModel

    (widgets, cWidgets, qthWidgets) <- loadFromGlade
    populateAllTables widgets

    -- Initialize the modes combo and the antenna combo.  The modes combo doesn't change,
    -- but the antenna combo can change whenever the QTH is changed.
    loadModes (wMode widgets) conf
    loadAntennas (wAntenna widgets) conf (lookup confDefaultQTH confQTHs)

    -- And then load up the QTH config dialog.
    loadQTHs (qthCombo qthWidgets) conf

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
                            psQTHWidgets = qthWidgets,
                            psPrevStore = previousStore,
                            psAllStore = allStore,
                            psContestMode = False,
                            psContestVal = mkNoneContest "",
                            psQTH = confDefaultQTH,
                            psRigSupport = rs }
    runGUI ps `finally` forM_ pid killRigctld
