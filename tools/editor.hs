{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

import Control.Applicative((<$>))
import Control.Arrow((***))
import Control.Monad(unless, when)
import Control.Monad.Trans(liftIO)
import Database.Esqueleto hiding(count, on)
import Database.Persist.Sqlite(runSqlite)
import Database.Persist.TH
import Data.List(sort)
import Data.Maybe(fromJust, fromMaybe, isJust)
import Data.Text(Text, pack)
import Graphics.UI.Gtk hiding(get, set)

import Slog.DXCC(DXCC(dxccEntity), entityIDs, entityFromID, idFromName)
import Slog.Formats.ADIF.Utils(freqToBand)
import Slog.Utils(stringToDouble, stringToInteger, uppercase)

-- A template haskell description of the database format.
share [mkPersist sqlSettings] [persistLowerCase|
 Qsos id=qsoid
    qsoid Int
    date String
    time String
    freq Double
    rx_freq Double Maybe
    mode String
    dxcc Int Maybe
    grid String Maybe
    state String Maybe
    name String Maybe
    notes String Maybe
    xc_in String Maybe
    xc_out String Maybe
    rst_rcvd String
    rst_sent String
    itu Int Maybe
    waz Int Maybe
    call String
    prop_mode String Maybe
    sat_name String Maybe
    antenna String Maybe
    deriving Eq Show
 Confirmations id=confid
    confid Int
    qsoid Int
    qsl_rdate String Maybe
    qsl_sdate String Maybe
    qsl_rcvd_via String Maybe
    qsl_sent_via String Maybe
    lotw_rdate String Maybe
    lotw_sdate String Maybe
    UniqueQsoid qsoid
    deriving Eq Show
|]

-- And then GTK is terrible, so I get to duplicate as much of the above as I want in order to
-- provide a data type for the store.
data Row = Row { rQsoid    :: Int,
                 rDate     :: String,
                 rTime     :: String,
                 rCall     :: String,
                 rFreq     :: Double,
                 rRxFreq   :: Maybe Double,
                 rMode     :: String,
                 rRSTRcvd  :: String,
                 rRSTSent  :: String,
                 rDXCC     :: Maybe Int,
                 rITU      :: Maybe Int,
                 rWAZ      :: Maybe Int,
                 rAntenna  :: Maybe String,
                 rUploaded :: Bool,

                 rEdited :: Bool }
 deriving Show

-- Given a path to a database, open it and extract all the QSOs and their confirmation
-- information as a list of tuples.
readDB :: FilePath -> IO [(Qsos, Confirmations)]
readDB filename = runSqlite (pack filename) $ do
    rows <- select $ from $ \(q, c) -> do where_ (q ^. QsosQsoid ==. c ^. ConfirmationsQsoid)
                                          orderBy [desc (q ^. QsosDate), desc (q ^. QsosTime)]
                                          return (q, c)
    return $ map (entityVal *** entityVal) rows

-- Update a single row in the database.  This happens regardless of whether the row has changed
-- or not.  It is recommended to call editedRows first.
updateOne :: FilePath -> Row -> IO ()
updateOne filename row = runSqlite (pack filename) $ do
    liftIO $ print row
    update $ \r -> do
        set r [ QsosDate =. (val $ rDate row),
                QsosTime =. (val $ rTime row),
                QsosCall =. (val $ rCall row),
                QsosFreq =. (val $ rFreq row),
                QsosRx_freq =. (val $ rRxFreq row),
                QsosMode =. (val $ rMode row),
                QsosRst_rcvd =. (val $ rRSTRcvd row),
                QsosRst_sent =. (val $ rRSTSent row),
                QsosDxcc =. (val $ rDXCC row),
                QsosItu =. (val $ rITU row),
                QsosWaz =. (val $ rWAZ row),
                QsosAntenna =. (val $ rAntenna row)
              ]
        where_ (r ^. QsosQsoid ==. (val $ rQsoid row))

    -- For now, only support going from uploaded to not uploaded.  That's okay because
    -- typically this program will be used to re-upload things that failed.  I can't really
    -- see why you'd want to mark previously not uploaded things as uploaded.
    unless (rUploaded row) $
        update $ \r -> do
            set r [ ConfirmationsLotw_sdate =. val Nothing ]
            where_ (r ^.ConfirmationsQsoid ==. (val $ rQsoid row))

-- Return a list of all the rows in the store that were modified via the UI.  This then tells
-- us what rows need to be written back into the database.
editedRows :: ListStore Row -> IO [Row]
editedRows store =
    filter rEdited <$> listStoreToList store

-- Update all rows in the database that need it.
updateEdited :: ListStore Row -> IO ()
updateEdited store = do
    lst <- editedRows store
    mapM_ (updateOne "/home/chris/radio/qsos.db") lst

-- Because the glade format in gtk2 is so bad, we have to add all the columns in code instead of
-- in glade.  So here it goes.
addColumns :: TreeViewClass self => ListStore Row -> self -> IO ()
addColumns store view = do
    -- DATE
    (dateCol, dateCell) <- newTextColumn store "Date" rDate
    on dateCell edited $ editedCell store (\row text -> row { rDate=text, rEdited=True })
    treeViewAppendColumn view dateCol

    -- TIME
    (timeCol, timeCell) <- newTextColumn store "Time" rTime
    on timeCell edited $ editedCell store (\row text -> row { rTime=text, rEdited=True })
    treeViewAppendColumn view timeCol

    -- CALL SIGN
    (callCol, callCell) <- newTextColumn store "Call" rCall
    on callCell edited $ editedCell store (\row text -> row { rCall=uppercase text, rEdited=True })
    treeViewAppendColumn view callCol

    -- FREQUENCY & RX FREQUENCY
    (freqCol, freqCell) <- newTextColumn store "Frequency" (show . rFreq)
    -- only update the frequency if the text is a double and is in some amateur band.
    on freqCell edited $ editedCell store (\row text -> let d = stringToDouble text
                                                        in  if isJust $ fmap freqToBand d then maybe row (\v -> row { rFreq=v, rEdited=True }) d
                                                            else row)
    treeViewAppendColumn view freqCol

    (rxFreqCol, rxFreqCell) <- newTextColumn store "Rcvd Frequency" $ \row -> maybe "" show (rRxFreq row)
    -- only update the frequency if the text is a double and is in some amateur band.
    on rxFreqCell edited $ editedCell store (\row text -> let d = stringToDouble text
                                                          in  if isJust $ fmap freqToBand d then maybe row (\v -> row { rRxFreq=Just v, rEdited=True }) d
                                                              else row)
    treeViewAppendColumn view rxFreqCol

    -- MODE
    (modeCol, modeCell) <- newTextColumn store "Mode" rMode
    on modeCell edited $ editedCell store (\row text -> row { rMode=uppercase text, rEdited=True })
    treeViewAppendColumn view modeCol

    -- RST SENT & RECEIVED
    (rstRCol, rstRCell) <- newTextColumn store "RST Rcvd" rRSTRcvd
    on rstRCell edited $ editedCell store (\row text -> row { rRSTRcvd=text, rEdited=True })
    treeViewAppendColumn view rstRCol

    (rstSCol, rstSCell) <- newTextColumn store "RST Sent" rRSTSent
    on rstSCell edited $ editedCell store (\row text -> row { rRSTSent=text, rEdited=True })
    treeViewAppendColumn view rstSCol

    -- DXCC
    dxccModel <- listStoreNew $ sort $ map (dxccEntity . fromJust . entityFromID) entityIDs
    customStoreSetColumn dxccModel (makeColumnIdString 0) id

    dxccColumn <- treeViewColumnNew
    dxccCell <- cellRendererComboNew
    cellLayoutPackStart dxccColumn dxccCell True
    cellLayoutSetAttributes dxccColumn dxccCell store $ \row -> [ cellText := defaultEntityText (rDXCC row),
                                                                  cellTextEditable := True,
                                                                  cellComboHasEntry := False,
                                                                  cellComboTextModel := (dxccModel, makeColumnIdString 0) ]
    on dxccCell edited $ editedCell store (\row text -> row { rDXCC=fmap fromInteger (idFromName text), rEdited=True })
    treeViewAppendColumn view dxccColumn
    treeViewColumnSetTitle dxccColumn ("DXCC" :: Text)

    -- ITU
    (ituCol, ituCell) <- newTextColumn store "ITU" $ \row -> maybe "" show (rITU row)
    on ituCell edited $ editedCell store (\row text -> let i = stringToInteger text
                                                       in  maybe row (\v -> row { rITU=Just $ fromInteger v, rEdited=True }) i)
    treeViewAppendColumn view ituCol

    -- WAZ
    (wazCol, wazCell) <- newTextColumn store "WAZ" $ \row -> maybe "" show (rWAZ row)
    on wazCell edited $ editedCell store (\row text -> let i = stringToInteger text
                                                       in  maybe row (\v -> row { rWAZ=Just $ fromInteger v, rEdited=True }) i)
    treeViewAppendColumn view wazCol

    -- ANTENNA
    (antCol, antCell) <- newTextColumn store "Antenna" $ \row -> fromMaybe (rAntenna row)
    on antCell edited $ editedCell store (\row text -> row { rAntenna=Just text, rEdited=True })
    treeViewAppendColumn view antCol

    -- UPLOADED TO LOTW?
    uploadedCol <- treeViewColumnNew
    uploadedCell <- cellRendererToggleNew
    cellLayoutPackStart uploadedCol uploadedCell False
    cellLayoutSetAttributes uploadedCol uploadedCell store $ \row -> [ cellToggleActive := rUploaded row ]
    on uploadedCell cellToggled $ toggledCell store (\row -> if rUploaded row then row { rUploaded=False, rEdited=True }
                                                             else row)
    treeViewAppendColumn view uploadedCol
    treeViewColumnSetTitle uploadedCol ("Uploaded?" :: Text)
 where
    -- Create and return a new text column and cell renderer, setting a bunch of attributes
    -- along the way.  The fn argument is used to set additional parameters, such as how
    -- and where this renderer gets its text to display from.
    newTextColumn :: ListStore Row -> String -> (Row -> String) -> IO (TreeViewColumn, CellRendererText)
    newTextColumn model title fn = do
        col <- treeViewColumnNew
        cell <- cellRendererTextNew
        cellLayoutPackStart col cell True
        cellLayoutSetAttributes col cell model $ \row -> [ cellText := fn row,
                                                           cellTextEditable := True ]
        treeViewColumnSetTitle col title
        return (col, cell)

    -- Given a model and a TreePath, convert that into an iter and check for validity.  If it's
    -- valid, convert that into an index into the model and call the provided function.
    withTreePath :: ListStore Row -> TreePath -> (ListStore Row -> Int -> IO ()) -> IO ()
    withTreePath model path fn = do
        iter <- treeModelGetIter model path
        when (isJust iter) $ do let ndx = listStoreIterToIndex (fromJust iter)
                                fn model ndx

    -- Update a row in the store in response to some cell receiving the edited signal.  The provided
    -- fn does the hard part of actually doing the editing, since we moight want to do some sort of
    -- sanity checking on the cell's contents instead of blindly cramming it into a row.
    editedCell :: ListStore Row -> (Row -> String -> Row) -> TreePath -> String -> IO ()
    editedCell model fn path text =
        withTreePath model path $ \model' ndx -> do cur <- listStoreGetValue model' ndx
                                                    listStoreSetValue model' ndx (fn cur text)

    -- And then this is the same as editedCell, except for toggled cell renderers.  Note two differences
    -- from the above function:  (1) The toggled signal gives us a String instead of a TreePath, so that's
    -- got to get converted.  (2) There's obviously no text stored in the model for a toggled cell, so
    -- argument numbers are different everywhere.
    toggledCell :: ListStore Row -> (Row -> Row) -> String -> IO ()
    toggledCell model fn path =
        withTreePath model (stringToTreePath $ pack path) $
                     \model' ndx -> do cur <- listStoreGetValue model' ndx
                                       listStoreSetValue model' ndx (fn cur)

    -- Figure out the initial text for the DXCC column in each row.  We want to display the
    -- name of the entity, but can't assume what's in the database is totally right.  So this
    -- takes care to check various error conditions and default to 291 (USA) on error.
    defaultEntityText :: Maybe Int -> String
    defaultEntityText (Just i) = let i' = toInteger i
                                 in  entityNameFromID (if i' `elem` entityIDs then i' else 291)
    defaultEntityText _        = entityNameFromID 291

    entityNameFromID :: Integer -> String
    entityNameFromID = dxccEntity . fromJust . entityFromID

runGUI :: [(Qsos, Confirmations)] -> IO ()
runGUI pairs = do
    initGUI

    -- Grab what little bit of the UI can be defined in glade.
    builder <- builderNew
    builderAddFromFile builder "/home/chris/src/slog/data/editor.ui"

    window <- builderGetObject builder castToWindow ("mainWindow" :: Text)
    onDestroy window mainQuit

    -- Create the store here.  The glade format recognized is pretty lame, so I have to do
    -- all the construction by hand instead of in glade.  Oh well.
    store <- listStoreNew ([] :: [Row])
    view <- builderGetObject builder castToTreeView ("qsoView" :: Text)

    addColumns store view

    -- Add everything into the store.
    mapM_ (\(q, c) -> listStoreAppend store Row { rQsoid=qsosQsoid q,
                                                  rDate=qsosDate q,
                                                  rTime=qsosTime q,
                                                  rCall=qsosCall q,
                                                  rFreq=qsosFreq q,
                                                  rRxFreq=qsosRx_freq q,
                                                  rMode=qsosMode q,
                                                  rRSTRcvd=qsosRst_rcvd q,
                                                  rRSTSent=qsosRst_sent q,
                                                  rDXCC=qsosDxcc q,
                                                  rITU=qsosItu q,
                                                  rWAZ=qsosWaz q,
                                                  rAntenna=qsosAntenna q,
                                                  rUploaded=isJust $ confirmationsLotw_sdate c,
                                                  rEdited=False }
          )
          pairs

    -- Finally, hook up the view and store.
    treeViewSetModel view store

    -- Make the buttons do something.
    [cancelButton, okButton] <-  mapM (builderGetObject builder castToButton) ["cancelButton" :: Text, "okButton" :: Text]
    on cancelButton buttonActivated mainQuit
    on okButton buttonActivated (updateEdited store >> mainQuit)

    widgetShowAll window
    mainGUI

main :: IO ()
main = do
    rows <- readDB "/home/chris/radio/qsos.db"
    runGUI rows
