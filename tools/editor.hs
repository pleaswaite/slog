{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Applicative((<$>))
import Control.Monad(when)
import Data.List(sort)
import Data.Maybe(fromJust, fromMaybe, isJust)
import Data.Text(Text, pack)
import Graphics.UI.Gtk hiding(get, set)

import Paths_Slog(getDataFileName)

import Slog.DB(DBResult, QsosId, getAllQSOs, initDB, updateQSO)
import Slog.DXCC(DXCC(dxccEntity), entityIDs, entityFromID, idFromName)
import qualified Slog.Formats.ADIF.Types as ADIF
import Slog.Formats.ADIF.Utils(freqToBand)
import Slog.Utils(stringToDouble, stringToInteger, uppercase)
import Slog.QSO

import ToolLib.Config

data Row = Row { rQsoid    :: QsosId,
                 rDate     :: String,
                 rTime     :: String,
                 rFreq     :: Double,
                 rRxFreq   :: Maybe Double,
                 rMode     :: String,
                 rDXCC     :: Maybe Int,
                 rGrid     :: Maybe String,
                 rState    :: Maybe String,
                 rRSTRcvd  :: String,
                 rRSTSent  :: String,
                 rITU      :: Maybe Int,
                 rWAZ      :: Maybe Int,
                 rCall     :: String,
                 rAntenna  :: Maybe String,
                 rUploaded :: Bool,

                 rEdited :: Bool }
 deriving Show

-- Return a list of all the rows in the store that were modified via the UI.  This then tells
-- us what rows need to be written back into the database.
editedRows :: ListStore Row -> IO [Row]
editedRows store =
    filter rEdited <$> listStoreToList store

-- Update all rows in the database that need it.
updateEdited :: FilePath -> ListStore Row -> IO ()
updateEdited db store = do
    lst <- editedRows store
    mapM_ (\r@Row{..} -> updateQSO db (rQsoid, rowToQSO r, not rUploaded)) lst
 where
    rowToQSO :: Row -> QSO
    rowToQSO Row{..} =
        QSO rDate
            rTime
            rFreq
            rRxFreq
            (read rMode :: ADIF.Mode)
            (toInteger <$> rDXCC)
            rGrid
            rState
            Nothing
            Nothing
            Nothing
            Nothing
            rRSTRcvd
            rRSTSent
            (toInteger <$> rITU)
            (toInteger <$> rWAZ)
            rCall
            Nothing
            Nothing
            rAntenna

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
                                                                  cellComboTextModel := (dxccModel, makeColumnIdString 0 :: ColumnId row Text ) ]
    on dxccCell edited $ editedCell store (\row text -> row { rDXCC=fmap fromInteger (idFromName text), rEdited=True })
    treeViewAppendColumn view dxccColumn
    treeViewColumnSetTitle dxccColumn ("DXCC" :: Text)

    -- GRID
    (gridCol, gridCell) <- newTextColumn store "Grid" $ \row -> fromMaybe "" (rGrid row)
    on gridCell edited $ editedCell store (\row text -> row { rGrid=Just text, rEdited=True })
    treeViewAppendColumn view gridCol

    -- STATE
    (stateCol, stateCell) <- newTextColumn store "State" $ \row -> fromMaybe "" (rState row)
    on stateCell edited $ editedCell store (\row text -> row { rState=Just text, rEdited=True })
    treeViewAppendColumn view stateCol

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
    (antCol, antCell) <- newTextColumn store "Antenna" $ \row -> fromMaybe "" (rAntenna row)
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

runGUI :: [DBResult] -> FilePath -> IO ()
runGUI rows db = do
    initGUI

    -- Grab what little bit of the UI can be defined in glade.
    builder <- builderNew
    gladeFile <- getDataFileName "editor.ui"
    builderAddFromFile builder gladeFile

    window <- builderGetObject builder castToWindow ("mainWindow" :: Text)
    onDestroy window mainQuit

    -- Create the store here.  The glade format recognized is pretty lame, so I have to do
    -- all the construction by hand instead of in glade.  Oh well.
    store <- listStoreNew ([] :: [Row])
    view <- builderGetObject builder castToTreeView ("qsoView" :: Text)

    addColumns store view

    -- Add everything into the store.
    mapM_ (\(i, QSO{..}, c) -> listStoreAppend store Row { rQsoid=i,
                                                           rDate=qDate,
                                                           rTime=qTime,
                                                           rFreq=qFreq,
                                                           rRxFreq=qRxFreq,
                                                           rMode=show qMode,
                                                           rDXCC=fromInteger <$> qDXCC,
                                                           rGrid=qGrid,
                                                           rState=qState,
                                                           rRSTRcvd=qRST_Rcvd,
                                                           rRSTSent=qRST_Sent,
                                                           rITU=fromInteger <$> qITU,
                                                           rWAZ=fromInteger <$> qWAZ,
                                                           rCall=qCall,
                                                           rAntenna=qAntenna,
                                                           rUploaded=isJust $ qLOTW_SDate c,
                                                           rEdited=False }
          )
          rows

    -- Finally, hook up the view and store.
    treeViewSetModel view store

    -- Make the buttons do something.
    [cancelButton, okButton] <-  mapM (builderGetObject builder castToButton) ["cancelButton" :: Text, "okButton" :: Text]
    on cancelButton buttonActivated mainQuit
    on okButton buttonActivated (updateEdited db store >> mainQuit)

    widgetShowAll window
    mainGUI

main :: IO ()
main = do
    Config{..} <- readConfig
    initDB confDB

    rows <- getAllQSOs confDB
    runGUI rows confDB
