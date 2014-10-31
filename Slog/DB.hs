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
module Slog.DB(DBResult,
               QsosId,
               confirmQSO,
               addQSO,
               getQSO,
               getAllQSOs,
               getLatestQSO,
               getQSOsByCall,
               getQSOsByDXCC,
               getQSOsByGrid,
               getUnconfirmedQSOs,
               getUnsentQSOs,
               markQSOsAsSent,
               first, second, third)
 where

import Control.Applicative((<$>))
import Control.Monad.Trans(liftIO)
import Data.List(isPrefixOf)
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

-- | The result for any query from the database.  Turns out different parts of the library and
-- tools want a different piece of this, and have therefore cobbled together their own methods for
-- getting and combining this data.  So they'll just all get it all now and can decide which parts
-- are useful and which are not.
type DBResult = (QsosId, QSO, Confirmation)

-- Extract elements out of the DBResult tuple.
first :: (a, b, c) -> a
first  (a, _, _) = a

second :: (a, b, c) -> b
second (_, b, _) = b

third :: (a, b, c) -> c
third  (_, _, c) = c

--
-- QUERYING FOR QSOs
--

-- | A generic way of finding the ID for a QSO.  This function can take at
-- most a date, time, call sign, and frequency.  It returns the 'DBResult'
-- of all QSOs meeting the given criteria.  It is assumed that only one QSO
-- can ever take place on a given frequency, with a given call sign, in a
-- given minute.  This seems to be a pretty strong assumption.
--
-- Note that the date and time must be given in YYYYMMDD and HHMM format -
-- see 'undashifyDate' and 'uncolonifyTime'.
getQSO :: FilePath -> Maybe ADIF.Date -> Maybe ADIF.Time -> Maybe String -> Maybe Double -> IO [DBResult]
getQSO _ Nothing Nothing Nothing Nothing = return []
getQSO filename date time call freq = runSqlite (pack filename) $ do
    rows <- select $ from $ \(q `InnerJoin` c) -> do on(q ^. QsosId ==. c ^. ConfirmationsQsoid)
                                                     where_ ((isNothing (val date)  ||. (just (q ^. QsosDate) ==. val date)) &&.
                                                             (isNothing (val time)  ||. (just (q ^. QsosTime) ==. val time)) &&.
                                                             (isNothing (val call') ||. (just (q ^. QsosCall) ==. val call')) &&.
                                                             (isNothing (val freq)  ||. (just (q ^. QsosFreq) ==. val freq)))
                                                     return (q ^. QsosId, q, c)
    return $ map fmtTuple rows
 where
    call' = fmap uppercase call

-- | Return a list of all 'DBResult' records in the database, sorted by date and time.
getAllQSOs :: FilePath -> IO [DBResult]
getAllQSOs filename = runSqlite (pack filename) $ do
    rows <- select $ from $ \(q `InnerJoin` c) -> do on (q ^. QsosId ==. c ^. ConfirmationsQsoid)
                                                     orderBy [desc (q ^. QsosDate), desc (q ^. QsosTime)]
                                                     return (q ^. QsosId, q, c)
    return $ map fmtTuple rows

-- | Return the most recently added 'DBResult' record.  This is basically for updating the UI
-- without having to tear down a store and rebuild it from scratch.
getLatestQSO :: FilePath -> IO DBResult
getLatestQSO filename = runSqlite (pack filename) $ do
    rows <- select $ from $ \(q `InnerJoin` c) -> do on (q ^. QsosId ==. c ^. ConfirmationsQsoid)
                                                     orderBy [desc (q ^. QsosDate), desc (q ^. QsosTime)]
                                                     limit 1
                                                     return (q ^. QsosId, q, c)
    return $ fmtTuple $ head rows

-- | Return a list of all 'DBResult' records in the database for a given call sign.
getQSOsByCall :: FilePath -> String -> IO [DBResult]
getQSOsByCall filename call = runSqlite (pack filename) $ do
    rows <- select $ from $ \(q `InnerJoin` c)-> do on (q ^. QsosId ==. c ^. ConfirmationsQsoid)
                                                    where_ (q ^. QsosCall ==. val (uppercase call))
                                                    orderBy [desc (q ^. QsosDate), desc (q ^. QsosTime)]
                                                    return (q ^. QsosId, q, c)
    return $ map fmtTuple rows

-- | Return a list of all 'DBResult' records in the database for a given DXCC entity.
getQSOsByDXCC :: FilePath -> Int -> IO [DBResult]
getQSOsByDXCC filename dxcc = runSqlite (pack filename) $ do
    rows <- select $ from $ \(q `InnerJoin` c)-> do on (q ^. QsosId ==. c ^. ConfirmationsQsoid)
                                                    where_ (q ^. QsosDxcc ==. just (val dxcc))
                                                    orderBy [desc (q ^. QsosDate), desc (q ^. QsosTime)]
                                                    return (q ^. QsosId, q, c)
    return $ map fmtTuple rows

-- | Return a list of all 'DBResult' records in the database for a given grid.
getQSOsByGrid :: FilePath -> String -> IO [DBResult]
getQSOsByGrid filename grid = do
    -- We want to compare short grids, but the full six-digit one could potentially be stored
    -- in the database.  Unfortunately I see no way of making esqueleto's "like" operator work
    -- with the potentially empty QsosGrid column.  So I have to do this the stupid way.
    results <- getAllQSOs filename
    return $ filter (\(_, q, _) ->  grid' `isPrefixOf` maybe "" (uppercase . take 4) (qGrid q))
                    results
 where
    grid' = uppercase $ take 4 grid

-- | Return a list of all 'DBResult' records that have not yet been confirmed with LOTW.  In
-- this case, the 'Confirmation' portion of the 'DBResult' is redundant information (given that
-- all results will be unconfirmed QSOs), but is returned for consistency.
getUnconfirmedQSOs :: FilePath -> IO [DBResult]
getUnconfirmedQSOs filename = runSqlite (pack filename) $ do
    rows <- select $ from $ \(q `InnerJoin` c) -> do on (q ^. QsosId ==. c ^. ConfirmationsQsoid)
                                                     where_ (isNothing $ c ^. ConfirmationsLotw_rdate)
                                                     orderBy [asc (c ^. ConfirmationsLotw_sdate)]
                                                     return (q ^. QsosId, q, c)
    return $ map fmtTuple rows

-- | Return a list of all 'DBResult' records that have not been uploaded to LOTW.
getUnsentQSOs :: FilePath -> IO [DBResult]
getUnsentQSOs filename = runSqlite (pack filename) $ do
    rows <- select $ from $ \(q `InnerJoin` c) -> do on (q ^. QsosId ==. c ^. ConfirmationsQsoid)
                                                     where_ (isNothing $ c ^. ConfirmationsLotw_sdate)
                                                     return (q ^. QsosId, q, c)
    return $ map fmtTuple rows

--
-- MODIFYING THE DATABASE
--

-- | Insert the new 'QSO' record into the database and return the new row's unique ID.
-- If a row already exists with the record's date and time, an exception is raised.
addQSO :: FilePath -> QSO -> IO QsosId
addQSO filename qso = runSqlite (pack filename) $ do
    -- First, add the new QSO to the qsos table.
    qsoid <- insert $ Qsos (qDate qso) (qTime qso)
                           (qFreq qso) (qRxFreq qso)
                           (show $ qMode qso)
                           (fmap fromInteger (qDXCC qso))
                           (fmap uppercase (qGrid qso))
                           (qState qso)
                           (qName qso)
                           (qNotes qso)
                           (fmap uppercase (qXcIn qso)) (fmap uppercase (qXcOut qso))
                           (qRST_Rcvd qso) (qRST_Sent qso)
                           (fmap fromInteger (qITU qso))
                           (fmap fromInteger (qWAZ qso))
                           (uppercase $ qCall qso)
                           (fmap show (qPropMode qso))
                           (qSatName qso)
                           (qAntenna qso)

    -- And then add a reference in the confirmations table.
    insert $ Confirmations qsoid Nothing Nothing Nothing Nothing Nothing Nothing
    return qsoid

--
-- DEALING WITH CONFIRMATIONS
--

-- | Given a unique 'QsosId' for a 'QSO' (which should have first been obtained by calling
-- one of the various findQSO functions and grabbing the first value out of the 'DBResult'
-- tuple, update the database to reflect that the 'QSO' record has been confirmed.  A confirmed
-- record is one where the local station has recorded an entry in the log, and the remote
-- station has recorded and uploaded a matching entry in their log.
confirmQSO :: FilePath -> QsosId -> ADIF.Date -> IO ()
confirmQSO filename qsoid qsl_date = runSqlite (pack filename) $
    update $ \r -> do
        set r [ ConfirmationsLotw_rdate =. val (Just qsl_date) ]
        where_ (r ^. ConfirmationsQsoid ==. val qsoid)

-- | Given a list of 'QsosId' values corresponding to previously unsent 'QSO' records, mark them
-- as uploaded to LOTW in the database.  It is expected this function will be called after
-- 'LOTW.upload', as it makes sense t ohave the uploading succeed before marking as sent.
markQSOsAsSent :: FilePath -> [QsosId] -> IO ()
markQSOsAsSent filename =
    mapM_ (markOne filename)
 where
    markOne :: FilePath -> QsosId -> IO ()
    markOne fn qsoid = do
        today <- liftIO getCurrentTime

        runSqlite (pack fn) $
            update $ \r -> do
                set r [ ConfirmationsLotw_sdate =. val (Just $ undashifyDate $ show $ utctDay today) ]
                where_ (r ^. ConfirmationsQsoid ==. val qsoid)

--
-- HELPER FUNCTIONS
--

fmtTuple :: (Value t, Entity Qsos, Entity Confirmations) -> (t, QSO, Confirmation)
fmtTuple (i, q, c) =
    (unValue i, sqlToQSO $ entityVal q, sqlToConf $ entityVal c)

sqlToQSO :: Qsos -> QSO
sqlToQSO q =
    QSO (qsosDate q)
        (qsosTime q)
        (qsosFreq q)
        (qsosRx_freq q)
        (read (qsosMode q) :: ADIF.Mode)
        (toInteger <$> qsosDxcc q)
        (qsosGrid q)
        (qsosState q)
        (qsosName q)
        (qsosNotes q)
        (qsosXc_in q)
        (qsosXc_out q)
        (qsosRst_rcvd q)
        (qsosRst_sent q)
        (toInteger <$> qsosItu q)
        (toInteger <$> qsosWaz q)
        (qsosCall q)
        (fmap (\p -> read p :: ADIF.Propagation) (qsosProp_mode q))
        (qsosSat_name q)
        (qsosAntenna q)

sqlToConf :: Confirmations -> Confirmation
sqlToConf c =
    Confirmation (confirmationsQsl_rdate c)
                 (confirmationsQsl_sdate c)
                 (fmap (\r -> read r :: ADIF.SentVia) (confirmationsQsl_rcvd_via c))
                 (fmap (\r -> read r :: ADIF.SentVia) (confirmationsQsl_sent_via c))
                 (confirmationsLotw_rdate c)
                 (confirmationsLotw_sdate c)
