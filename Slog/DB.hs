{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
-- | This module interfaces with the sqlite3 database that makes up the data store of
-- the library.  This database stores a serialized version of the 'QSO' record, plus
-- some information about whether individual records have been uploaded to LOTW or
-- eQSL, whether a QSL card was received or sent, and whether records have been
-- confirmed by the remote station.
--
-- This module provides a monadic interface to the database.  Public functions should
-- be run via the 'runTransaction' function, which takes a path to the database and
-- handles the details of connection management transparently.  One significant downside
-- to this module is that most of these functions will call 'fail' on error, which does
-- not result in very friendly behavior.  This needs to be fixed.
module Slog.DB(confirmQSO,
               findQSOByDateTime,
               addQSO,
               getQSO,
               getAllQSOs,
               getUnconfirmedQSOs,
               getUnsentQSOs,
               markQSOsAsSent,
               runTransaction,
               Transaction)
 where

import Control.Applicative((<$>))
import Control.Monad(sequence_, void)
import Control.Monad.Reader
import Data.Time.Clock
import qualified Database.HDBC as H
import Database.HDBC.Sqlite3(Connection, connectSqlite3)

import qualified Slog.Formats.ADIF.Types as ADIF
import Slog.QSO
import Slog.Utils(undashifyDate, uppercase)

-- | The 'Transaction' type hides the details of database connection management from
-- the user.  Almost all public functions in this module will return something in
-- this monad.
type Transaction a = ReaderT Connection IO a

-- | Given the file path to the database and a function in the 'Transaction' monad,
-- run the transaction and return the result.  This handles connection management
-- for the user.
runTransaction :: FilePath -> Transaction a -> IO a
runTransaction fp io = do
    dbh <- connectSqlite3 fp
    prepDB dbh
    runReaderT withCommit dbh
 where
    withCommit = do
        result <- io
        ask >>= liftIO . H.commit
        return result

-- These are all private functions that are used internally to duplicate functionality
-- from the HDBC module, but in such a way as to be callable from the monad.

executeMany :: H.Statement -> [[H.SqlValue]] -> Transaction ()
executeMany stmt v = liftIO $ H.executeMany stmt v

prepare :: String -> Transaction H.Statement
prepare q = ask >>= \c -> liftIO $ H.prepare c q

quickQuery' :: String -> [H.SqlValue] -> Transaction [[H.SqlValue]]
quickQuery' q v = ask >>= \c -> liftIO $ H.quickQuery' c q v

-- If the qsos table does not already exist, create it now.  This is implicitly called
-- each time some statement is executed from the transaction monad.
prepDB :: H.IConnection conn => conn -> IO ()
prepDB dbh = do
    tables <- H.getTables dbh
    when ("qsos" `notElem` tables) $ do
        void $ H.run dbh "CREATE TABLE qsos (\
                \qsoid INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,\
                \date TEXT NOT NULL, time TEXT NOT NULL,\
                \freq REAL NOT NULL, rx_freq REAL,\
                \mode TEXT NOT NULL,\
                \dxcc INTEGER,\
                \grid TEXT,\
                \state TEXT,\
                \name TEXT,\
                \notes TEXT,\
                \xc_in TEXT, xc_out TEXT,\
                \rst_rcvd TEXT NOT NULL, rst_sent TEXT NOT NULL,\
                \itu INTEGER,\
                \waz INTEGER,\
                \call TEXT NOT NULL,\
                \prop_mode TEXT,\
                \sat_name TEXT,\
                \antenna TEXT)" []
    when ("confirmations" `notElem` tables) $ do
        void $ H.run dbh "CREATE TABLE confirmations (\
                \confid INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,\
                \qsoid INTEGER NOT NULL UNIQUE,\
                \qsl_rdate TEXT, qsl_sdate TEXT,\
                \qsl_rcvd_via TEXT, qsl_sent_via TEXT,\
                \lotw_rdate TEXT, lotw_sdate TEXT)" []
    H.commit dbh

-- | Given a QSO date and time (in YYYYMMDD and HHMM format - see -- 'undashifyDate'
-- and 'uncolonifyTime'), look up and return the unique ID for the first QSO found.
-- It is assumed that only one QSO can ever happen in any given minute.
-- FIXME:  If no QSO is found, fail is called.
findQSOByDateTime :: ADIF.Date -> ADIF.Time -> Transaction Integer
findQSOByDateTime date time = do
    ndxs <- quickQuery' "SELECT qsoid FROM qsos WHERE date = ? and time = ?"
                        [H.toSql $ date, H.toSql $ time]

    -- There really better be only one result.
    case ndxs of
        [[ndx]] -> return $ H.fromSql ndx
        _       -> fail $ "No QSO found for " ++ (show date) ++ " " ++ (show time)

-- | Insert the new 'QSO' record into the database and return the new row's unique ID.
-- If a row already exists with the record's date and time, an exception is raised.
addQSO :: QSO -> Transaction Integer
addQSO qso = do
    -- First, add the new QSO to the qsos table.
    addToQSOTable qso

    -- Get the qsoid assigned by the database so we can create the other tables.
    qsoid <- findQSOByDateTime (qDate qso) (qTime qso)
    addToConfTable (H.toSql qsoid)
    return qsoid
 where
    addToQSOTable q = quickQuery' "INSERT INTO qsos (date, time, freq, rx_freq, mode, dxcc,\
                                                     \grid, state, name, notes, xc_in, xc_out,\
                                                     \rst_rcvd, rst_sent, itu, waz,\
                                                     \call, prop_mode, sat_name, antenna)\
                                   \VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
                                   (qsoToSql q)
    addToConfTable ndx = quickQuery' "INSERT INTO confirmations (qsoid) VALUES (?)" [ndx]

-- | Given a 'qsoid', return a 'QSO' record from the database.
getQSO :: Integer -> Transaction QSO
getQSO qsoid =
    head <$> map sqlToQSO <$> quickQuery' "SELECT * FROM qsos WHERE qsoid = ?" [H.toSql qsoid]

-- | Given a unique ID for a 'QSO' (which should have first been obtained by calling
-- 'findQSOByDateTime') and a confirmation date, update the database to reflect that the
-- 'QSO' record has been confirmed.  A confirmed record is one where the local station has
-- recorded an entry in the log, and the remote station has recorded and uploaded a
-- matching entry in their log.
confirmQSO :: Integer -> ADIF.Date -> Transaction ()
confirmQSO qsoid qsl_date =
    void $ quickQuery' "UPDATE confirmations SET lotw_rdate = ? WHERE qsoid = ?"
                       [H.toSql qsl_date, H.toSql qsoid]

-- | Return a list of all 'QSO' records in the database, sorted by date and time.
getAllQSOs :: Transaction [QSO]
getAllQSOs =
    map sqlToQSO <$> quickQuery' "SELECT * FROM qsos ORDER BY date,time" []

-- | Return a list of all 'QSO' records that have not yet been confirmed with LOTW.
getUnconfirmedQSOs :: Transaction [QSO]
getUnconfirmedQSOs =
    map sqlToQSO <$> quickQuery' "SELECT qsos.* FROM qsos,confirmations WHERE \
                                  \qsos.qsoid=confirmations.qsoid AND lotw_rdate IS NULL \
                                  \ORDER BY lotw_sdate ASC;" []

-- | Return a list of all 'QSO' records that have not been uploaded to LOTW.
getUnsentQSOs :: Transaction [QSO]
getUnsentQSOs =
    map sqlToQSO <$> quickQuery' "SELECT qsos.* FROM qsos,confirmations WHERE \
                                  \qsos.qsoid=confirmations.qsoid AND lotw_sdate IS NULL;" []

-- | Given a list of previously unsent 'QSO' records (perhaps as returned by 'getUnsentQSOs'),
-- mark them as uploaded to LOTW in the database.  It is expected this function will be
-- called after 'LOTW.upload', as it makes sense to have the uploading succeed before
-- marking as sent.
markQSOsAsSent :: [QSO] -> Transaction ()
markQSOsAsSent qsos = do
    today <- liftIO getCurrentTime
    ids <- mapM (\qso -> findQSOByDateTime (qDate qso) (qTime qso)) qsos

    confStmt <- prepare "UPDATE confirmations SET lotw_sdate = ? WHERE qsoid = ?"
    executeMany confStmt (map (\x -> [H.toSql $ undashifyDate $ show $ utctDay today, H.toSql x]) ids)

--
-- HELPER FUNCTIONS
--

-- Convert between QSO and SqlValue types.  Order is important on the lists.
qsoToSql :: QSO -> [H.SqlValue]
qsoToSql qso =
    [H.toSql $ qDate qso, H.toSql $ qTime qso, H.toSql $ qFreq qso, H.toSql $ qRxFreq qso, H.toSql $ show $ qMode qso,
     H.toSql $ qDXCC qso, H.toSql $ qGrid qso, H.toSql $ qState qso, H.toSql $ qName qso, H.toSql $ qNotes qso,
     H.toSql $ qXcIn qso, H.toSql $ qXcOut qso, H.toSql $ qRST_Rcvd qso, H.toSql $ qRST_Sent qso,
     H.toSql $ qITU qso, H.toSql $ qWAZ qso, H.toSql $ uppercase (qCall qso), H.toSql $ qPropMode qso, H.toSql $ qSatName qso,
     H.toSql $ qAntenna qso]

sqlToQSO :: [H.SqlValue] -> QSO
sqlToQSO [_, date, time, freq, rx_freq, mode, dxcc, grid, state, name, notes, xc_in,
          xc_out, rst_rcvd, rst_sent, itu, waz, call, prop_mode, sat_name, antenna] =
    QSO {qDate = H.fromSql date, qTime = H.fromSql time, qFreq = H.fromSql freq, qRxFreq = H.fromSql rx_freq,
         qMode = read (H.fromSql mode) :: ADIF.Mode, qDXCC = H.fromSql dxcc, qGrid = H.fromSql grid,
         qState = H.fromSql state, qName = H.fromSql name, qNotes = H.fromSql notes, qXcIn = H.fromSql xc_in,
         qXcOut = H.fromSql xc_out, qRST_Rcvd = H.fromSql rst_rcvd, qRST_Sent = H.fromSql rst_sent,
         qITU = H.fromSql itu, qWAZ = H.fromSql waz, qCall = H.fromSql call,
         qPropMode = H.fromSql prop_mode, qSatName = H.fromSql sat_name, qAntenna = H.fromSql antenna}
sqlToQSO _ = error $ "sqlToQSO got an unexpected length of list.  How did this happen?"
