{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs  #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

-- | This module interfaces with the sqlite3 database that makes up the data store of
-- the library.  This database stores a serialized version of the 'QSO' record, plus
-- some information about whether individual records have been uploaded to LOTW or
-- eQSL, whether a QSL card was received or sent, and whether records have been
-- confirmed by the remote station.
module Slog.DB(QsosId,
               confirmQSO,
               findQSO,
               findQSOByDateTime,
               addQSO,
               getQSOByID,
               getAllQSOs,
               getQSOsByCall,
               getUnconfirmedQSOs,
               getUnsentQSOs,
               markQSOsAsSent)
 where

import Control.Monad.Trans(liftIO)
import Data.Text(pack)
import Data.Time.Clock
import Database.Esqueleto hiding(count)
import Database.Persist.Sqlite(runSqlite)
import Database.Persist.TH

import qualified Slog.Formats.ADIF.Types as ADIF
import Slog.QSO
import Slog.Utils(undashifyDate, uppercase)

-- A template haskell description of the database format.
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
 Qsos
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
 Confirmations
    qsoid QsosId
    qsl_rdate String Maybe
    qsl_sdate String Maybe
    qsl_rcvd_via String Maybe
    qsl_sent_via String Maybe
    lotw_rdate String Maybe
    lotw_sdate String Maybe
    UniqueQsoid qsoid
    deriving Eq Show
|]

--
-- FINDING THE ID FOR A GIVEN QSO
--

-- | A generic way of finding the ID for a QSO.  This function can take at
-- most a date, time, call sign, and frequency.  It returns the ID of the
-- first QSO meeting the given criteria.  It is assumed that only one QSO
-- can ever take place on a given frequency, with a given call sign, in a
-- given minute.  This seems to be a pretty strong assumption.
--
-- Regardless, this function returns multiple IDs given that the caller may
-- have only passed in some information - not enough to just return one value.
--
-- Note that the date and time must be given in YYYYMMDD and HHMM format -
-- see 'undashifyDate' and 'uncolonifyTime'.
findQSO :: FilePath -> Maybe ADIF.Date -> Maybe ADIF.Time -> Maybe String -> Maybe Double -> IO [QsosId]
findQSO _ Nothing Nothing Nothing Nothing = return []
findQSO filename date time call freq = runSqlite (pack filename) $ do
    rows <- select $ from $ \q -> do where_ (((isNothing $ val date) ||. ((just $ q ^. QsosDate) ==. val date)) &&.
                                             ((isNothing $ val time) ||. ((just $ q ^. QsosTime) ==. val time)) &&.
                                             ((isNothing $ val call) ||. ((just $ q ^. QsosCall) ==. val call)) &&.
                                             ((isNothing $ val freq) ||. ((just $ q ^. QsosFreq) ==. val freq)))
                                     return (q ^. QsosId)
    return $ map unValue rows

-- | Given a QSO date and time (in YYYYMMDD and HHMM format - see 'undashifyDate'
-- and 'uncolonifyTime'), look up and return the unique ID for the first QSO found.
-- It is assumed that only one QSO can ever happen in any given minute.
findQSOByDateTime :: FilePath -> ADIF.Date -> ADIF.Time -> IO (Maybe QsosId)
findQSOByDateTime filename date time = runSqlite (pack filename) $ do
    rows <- select $ from $ \q -> do where_ ((q ^. QsosDate ==. val date) &&.
                                             (q ^. QsosTime ==. val time))
                                     limit 1
                                     return (q ^. QsosId)
    if null rows then return Nothing
    else return $ Just $ unValue $ head rows

--
-- FINDING A QSO OBJECT
--

-- | Given a 'qsoid', return a 'QSO' record from the database.
getQSOByID :: FilePath -> QsosId -> IO QSO
getQSOByID filename qsoid = runSqlite (pack filename) $ do
    rows <- select $ from $ \q -> do where_ (q ^. QsosId ==. (val qsoid))
                                     limit 1
                                     return q
    return $ (sqlToQSO . entityVal) (head rows)

-- | Return a list of all 'QSO' records in the database, sorted by date and time.
getAllQSOs :: FilePath -> IO [QSO]
getAllQSOs filename = runSqlite (pack filename) $ do
    rows <- select $ from $ \q -> do orderBy [desc (q ^. QsosDate), desc (q ^. QsosTime)]
                                     return q
    return $ map (sqlToQSO . entityVal) rows

-- | Return a list of all 'QSO' records in the database for a given call sign.
getQSOsByCall :: FilePath -> String -> IO [QSO]
getQSOsByCall filename call = runSqlite (pack filename) $ do
    rows <- select $ from $ \q -> do where_ (q ^. QsosCall ==. (val call))
                                     orderBy [desc (q ^. QsosDate), desc (q ^. QsosTime)]
                                     return q
    return $ map (sqlToQSO . entityVal) rows

-- | Return a list of all 'QSO' records that have not yet been confirmed with LOTW.
getUnconfirmedQSOs :: FilePath -> IO [QSO]
getUnconfirmedQSOs filename = runSqlite (pack filename) $ do
    rows <- select $ from $ \(q `InnerJoin` c) -> do on (q ^. QsosId ==. c ^. ConfirmationsQsoid)
                                                     where_ (isNothing $ c ^. ConfirmationsLotw_rdate)
                                                     orderBy [asc (c ^. ConfirmationsLotw_sdate)]
                                                     return q
    return $ map (sqlToQSO . entityVal) rows

-- | Return a list of all 'QSO' records that have not been uploaded to LOTW.
getUnsentQSOs :: FilePath -> IO [QSO]
getUnsentQSOs filename = runSqlite (pack filename) $ do
    rows <- select $ from $ \(q `InnerJoin` c) -> do on (q ^. QsosId ==. c ^. ConfirmationsQsoid)
                                                     where_ (isNothing $ c ^. ConfirmationsLotw_sdate)
                                                     return q
    return $ map (sqlToQSO . entityVal) rows

--
-- MODIFYING THE DATABASE
--

-- | Insert the new 'QSO' record into the database and return the new row's unique ID.
-- If a row already exists with the record's date and time, an exception is raised.
addQSO :: FilePath -> QSO -> IO QsosId
addQSO filename qso = runSqlite (pack filename) $ do
    -- First, add the new QSO to the qsos table.
    qsoid <- insert $ Qsos (qDate qso) (qTime qso) (qFreq qso) (qRxFreq qso) (show $ qMode qso)
                           (fmap fromInteger $ qDXCC qso) (qGrid qso) (qState qso) (qName qso) (qNotes qso)
                           (qXcIn qso) (qXcOut qso) (qRST_Rcvd qso) (qRST_Sent qso)
                           (fmap fromInteger $ qITU qso) (fmap fromInteger $ qWAZ qso) (uppercase $ qCall qso)
                           (fmap show $ qPropMode qso) (qSatName qso) (qAntenna qso)

    -- And then add a reference in the confirmations table.
    insert $ Confirmations qsoid Nothing Nothing Nothing Nothing Nothing Nothing
    return qsoid

--
-- DEALING WITH CONFIRMATIONS
--

-- | Given a unique ID for a 'QSO' (which should have first been obtained by calling
-- 'findQSOByDateTime') and a confirmation date, update the database to reflect that the
-- 'QSO' record has been confirmed.  A confirmed record is one where the local station has
-- recorded an entry in the log, and the remote station has recorded and uploaded a
-- matching entry in their log.
confirmQSO :: FilePath -> QsosId -> ADIF.Date -> IO ()
confirmQSO filename qsoid qsl_date = runSqlite (pack filename) $ do
    update $ \r -> do
        set r [ ConfirmationsLotw_rdate =. (val $ Just qsl_date) ]
        where_ (r ^. ConfirmationsQsoid ==. (val qsoid))

-- | Given a list of previously unsent 'QSO' records (perhaps as returned by 'getUnsentQSOs'),
-- mark them as uploaded to LOTW in the database.  It is expected this function will be
-- called after 'LOTW.upload', as it makes sense to have the uploading succeed before
-- marking as sent.
markQSOsAsSent :: FilePath -> [QSO] -> IO ()
markQSOsAsSent filename qsos = do
    ids <- mapM (\qso -> findQSO filename (Just $ qDate qso) (Just $ qTime qso) (Just $ qCall qso) (Just $ qFreq qso)) qsos
    mapM_ (markOne filename) (concat ids)
 where
    markOne :: FilePath -> QsosId -> IO ()
    markOne fn qsoid = do
        today <- liftIO getCurrentTime

        runSqlite (pack fn) $ do
            update $ \r -> do
                set r [ ConfirmationsLotw_sdate =. (val $ Just $ undashifyDate $ show $ utctDay today) ]
                where_ (r ^. ConfirmationsQsoid ==. (val qsoid))

--
-- HELPER FUNCTIONS
--

sqlToQSO :: Qsos -> QSO
sqlToQSO q =
    QSO (qsosDate q)
        (qsosTime q)
        (qsosFreq q)
        (qsosRx_freq q)
        (read (qsosMode q) :: ADIF.Mode)
        (fmap toInteger $ qsosDxcc q)
        (qsosGrid q)
        (qsosState q)
        (qsosName q)
        (qsosNotes q)
        (qsosXc_in q)
        (qsosXc_out q)
        (qsosRst_rcvd q)
        (qsosRst_sent q)
        (fmap toInteger $ qsosItu q)
        (fmap toInteger $ qsosWaz q)
        (qsosCall q)
        (fmap (\p -> read p :: ADIF.Propagation) (qsosProp_mode q))
        (qsosSat_name q)
        (qsosAntenna q)
