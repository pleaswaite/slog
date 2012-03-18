{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module DB(connect,
          findQSOByDateTime,
          addQSO,
          updateQSO,
          removeQSO,
          getAllQSOs,
          getAllQSOs',
          getUnconfirmedQSOs,
          getUnsentQSOs,
          markQSOsAsSent)
 where

import Control.Monad(when)
import Data.Char(toUpper)
import Data.List(find)
import Data.Time.Clock
import Database.HDBC
import Database.HDBC.Sqlite3(Connection, connectSqlite3)

import qualified Formats.ADIF.Types as ADIF
import QSO
import Utils(undashifyDate)

-- Connect to the database, create the tables if necessary, and return the
-- database handle.
connect :: FilePath -> IO Connection
connect fp = do
    dbh <- connectSqlite3 fp
    prepDB dbh
    return dbh

-- If the qsos table does not already exist, create it now.
prepDB :: IConnection conn => conn -> IO ()
prepDB dbh = do
    tables <- getTables dbh
    when (not ("qsos" `elem` tables)) $ do
        run dbh "CREATE TABLE qsos (\
                \qsoid INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,\
                \date TEXT NOT NULL, time TEXT NOT NULL,\
                \freq REAL NOT NULL, rx_freq REAL,\
                \mode TEXT NOT NULL,\
                \dxcc TEXT,\
                \grid TEXT,\
                \state TEXT,\
                \name TEXT,\
                \notes TEXT,\
                \xc_in TEXT, xc_out TEXT,\
                \rst_rcvd TEXT NOT NULL, rst_sent TEXT NOT NULL,\
                \iota TEXT,\
                \itu INTEGER,\
                \waz_zone INTEGER,\
                \call TEXT NOT NULL,\
                \sat_name TEXT, sat_mode TEXT)" []
        return ()
    when (not ("confirmations" `elem` tables)) $ do
        run dbh "CREATE TABLE confirmations (\
                \confid INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,\
                \qsoid INTEGER NOT NULL UNIQUE,\
                \qsl_rdate TEXT, qsl_sdate TEXT,\
                \qsl_rcvd TEXT, qsl_rcvd_via TEXT,\
                \qsl_sent TEXT, qsl_sent_via TEXT,\
                \lotw_rdate TEXT, lotw_sdate TEXT,\
                \lotw_rcvd TEXT, lotw_sent TEXT)" []
        return ()
    commit dbh

-- Given a QSO date and time, return its unique ID from the qsos table.  Raises
-- an error if no such QSO exists, so be prepared to handle that.
findQSOByDateTime :: IConnection conn => conn -> ADIF.Date -> ADIF.Time -> IO Integer
findQSOByDateTime dbh date time = do
    ndxs <- quickQuery' dbh "SELECT qsoid FROM qsos WHERE date = ? and time = ?"
                        [toSql $ date, toSql $ time]

    -- There really better be only one result.
    case ndxs of
        [[ndx]] -> return $ fromSql ndx
        _       -> fail $ "No QSO found for " ++ (show date) ++ " " ++ (show time)

-- Add a QSO object into the database.  Returns the new row's qsoid.
addQSO :: IConnection conn => conn -> QSO -> IO Integer
addQSO dbh qso = handleSql errorHandler $ do
    -- First, add the new QSO to the qsos table.
    addToQSOTable dbh qso

    -- Get the qsoid assigned by the database so we can create the other tables.
    qsoid <- findQSOByDateTime dbh (qDate qso) (qTime qso)
    addToConfTable dbh (toSql qsoid)
    commit dbh
    return qsoid
 where
    addToQSOTable db q = run db "INSERT INTO qsos (date, time, freq, rx_freq, mode, dxcc,\
                                                  \grid, state, name, notes, xc_in, xc_out,\
                                                  \rst_rcvd, rst_sent, iota, itu, waz_zone,\
                                                  \call, sat_name, sat_mode)\
                                \VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
                            (qsoToSql q)
    addToConfTable db ndx = run db "INSERT INTO confirmations (qsoid) VALUES (?)" [ndx]

    errorHandler e = do fail $ "Error adding QSO:  A QSO with this date and time already exists.\n" ++ show e

confirmQSO :: IConnection conn => conn -> [ADIF.Field] -> IO ()
confirmQSO conn fields = do
    return ()

-- Given a qsoid (which should have first been obtained by calling findQSOByDateTime),
-- update it with the values out of the provided new QSO.
updateQSO :: IConnection conn => conn -> Integer -> QSO -> IO ()
updateQSO dbh qsoid qso = do
    run dbh "UPDATE qsos SET date = ?, time = ?, freq = ?, rx_freq = ?, mode = ?\
            \dxcc = ?, grid = ?, state = ?, name = ?, notes = ?,\
            \xc_in = ?, xc_out = ?, rst_rcvd = ?, rst_sent = ?, iota = ?, itu = ?\
            \waz_zone = ?, call = ?, sat_name = ?, sat_mode = ?\
            \WHERE qsoid = ?"
            (qsoToSql qso ++ [toSql qsoid])
    -- Then, we need to zero out this QSO's row in the confirmations table so we'll know
    -- to upload the new information later.
    run dbh "UPDATE confirmations SET lotw_rdate = \"\", lotw_sdate = \"\", lotw_rcvd = \"\", lotw_sent = \"\" WHERE qsoid = ?"
            [toSql qsoid]
    commit dbh
    return ()

-- Look up the QSO given by the supplied qsoid (which should have first been obtained
-- by calling findQSOByDateTime), and remove it from the database.
removeQSO :: IConnection conn => conn -> Integer -> IO ()
removeQSO dbh qsoid = do
    run dbh "DELETE FROM qsos WHERE qsoid = ?" [toSql qsoid]
    run dbh "DELETE FROM confirmations WHERE qsoid = ?" [toSql qsoid]
    commit dbh
    return ()

-- Return a list of all QSOs in the database.
getAllQSOs :: IConnection conn => conn -> IO [QSO]
getAllQSOs dbh = do
    results <- quickQuery' dbh "SELECT * FROM qsos ORDER BY date,time" []
    return $ map sqlToQSO results

-- Return a list of all QSOs in the database, sorted in reverse.
getAllQSOs' :: IConnection conn => conn -> IO [QSO]
getAllQSOs' dbh = do
    results <- quickQuery' dbh "SELECT * FROM qsos ORDER BY date DESC, time DESC" []
    return $ map sqlToQSO results

-- Return a list of all QSOs that have not been confirmed with LOTW.
getUnconfirmedQSOs dbh = do
    results <- quickQuery' dbh "SELECT qsos.* FROM qsos,confirmations WHERE \
                                \qsos.qsoid=confirmations.qsoid AND lotw_rcvd IS NULL \
                                \ORDER BY lotw_sdate ASC;" []
    return $ map sqlToQSO results

-- Return a list of all QSOs that have not been uploaded to LOTW.
getUnsentQSOs :: IConnection conn => conn -> IO [QSO]
getUnsentQSOs dbh = do
    results <- quickQuery' dbh "SELECT qsos.* FROM qsos,confirmations WHERE \
                               \qsos.qsoid=confirmations.qsoid AND lotw_sent IS NULL;" []
    return $ map sqlToQSO results

-- Given a list of previously un-uploaded QSOs, mark them as sent.  It is expected
-- that the input list will be the output of getUnsentQSOs, with this function called
-- after that one and LOTW.upload.
markQSOsAsSent :: IConnection conn => conn -> [QSO] -> IO ()
markQSOsAsSent dbh qsos = do
    today <- getCurrentTime
    ids <- mapM (\qso -> findQSOByDateTime dbh (qDate qso) (qTime qso)) qsos

    confStmt <- prepare dbh "UPDATE confirmations SET lotw_sdate = ?, \
                            \lotw_sent = \"Y\" WHERE qsoid = ?"

    executeMany confStmt (map (\x -> [toSql $ undashifyDate $ show $ utctDay today, toSql x]) ids)
    commit dbh
    return ()

--
-- HELPER FUNCTIONS
--

-- Convert between QSO and SqlValue types.  Order is important on the lists.
qsoToSql :: QSO -> [SqlValue]
qsoToSql qso =
    [toSql $ qDate qso, toSql $ qTime qso, toSql $ qFreq qso, toSql $ qRxFreq qso, toSql $ show $ qMode qso,
     toSql $ qDXCC qso, toSql $ qGrid qso, toSql $ qState qso, toSql $ qName qso, toSql $ qNotes qso,
     toSql $ qXcIn qso, toSql $ qXcOut qso, toSql $ qRST_Rcvd qso, toSql $ qRST_Sent qso, toSql $ qIOTA qso,
     toSql $ qITU qso, toSql $ qWAZ qso, toSql $ upcase (qCall qso), toSql $ qSatName qso, toSql $ qSatMode qso]
 where
    upcase = map toUpper

sqlToQSO :: [SqlValue] -> QSO
sqlToQSO [qsoid, date, time, freq, rx_freq, mode, dxcc, grid, state, name, notes, xc_in,
          xc_out, rst_rcvd, rst_sent, iota, itu, waz_zone, call, sat_name, sat_mode] =
    QSO {qDate = fromSql date, qTime = fromSql time, qFreq = fromSql freq, qRxFreq = fromSql rx_freq,
         qMode = read (fromSql mode) :: ADIF.Mode, qDXCC = fromSql dxcc, qGrid = fromSql grid, qState = fromSql state,
         qName = fromSql name, qNotes = fromSql notes,
         qXcIn = fromSql xc_in, qXcOut = fromSql xc_out, qRST_Rcvd = fromSql rst_rcvd, qRST_Sent = fromSql rst_sent,
         qIOTA = fromSql iota, qITU = fromSql itu, qWAZ = fromSql waz_zone,
         qCall = fromSql call, qSatName = fromSql sat_name, qSatMode = fromSql sat_mode}
sqlToQSO _ = error $ "sqlToQSO got an unexpected length of list.  How did this happen?"
