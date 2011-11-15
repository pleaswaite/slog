module DB where

import Database.HDBC
import Database.HDBC.Sqlite3(Connection, connectSqlite3)
import Control.Monad(when)

import qualified Formats.ADIF.Types as ADIF
import QSO

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
                \freq REAL NOT NULL,\
                \rx_freq REAL,\
                \mode TEXT,\
                \dxcc TEXT,\
                \grid TEXT,\
                \state TEXT,\
                \name TEXT,\
                \qsl_rdate TEXT, qsl_sdate TEXT,\
                \qsl_rcvd TEXT, qsl_rcvd_via TEXT,\
                \qsl_sent TEXT, qsl_sent_via TEXT,\
                \notes TEXT,\
                \lotw_rdate TEXT, lotw_sdate TEXT,\
                \lotw_rcvd TEXT, lotw_sent TEXT,\
                \xc_in TEXT, xc_out TEXT,\
                \rst_rcvd TEXT, rst_sent TEXT,\
                \iota TEXT,\
                \itu INTEGER,\
                \waz_zone INTEGER,\
                \call TEXT NOT NULL,\
                \sat_name TEXT, sat_mode TEXT)" []
        return ()
    commit dbh

-- Add a QSO object into the database, raising an IO exception with error
-- message if a QSO with the same date and time already exists.
addQSO :: IConnection conn => conn -> QSO -> IO ()
addQSO dbh qso = handleSql errorHandler $ do
    run dbh "INSERT INTO qsos VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?,\
                                     \?, ?, ?, ?, ?, ?, ?, ?, ?, ?,\
                                     \?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
            (qsoToSql qso)
    return ()
 where
    errorHandler e = do fail $ "Error adding QSO:  A QSO with this date and time already exists.\n" ++ show e

-- Look up the QSO given by the supplied date and time, and update it with the
-- values out of the provided new QSO.
updateQSO :: IConnection conn => conn -> ADIF.Date -> ADIF.Time -> QSO -> IO ()
updateQSO dbh date time qso =
    run dbh "UPDATE qsos SET date = ?, time = ?, freq = ?, rx_freq = ?, mode = ?\
            \dxcc = ?, grid = ?, state = ?, name = ?, qsl_rdate = ?, qsl_sdate = ?\
            \qsl_rcvd = ?, qsl_rcvd_via = ?, qsl_sent = ?, qsl_sent_via = ?\
            \notes = ?, lotw_rdate = ?, lotw_sdate = ?, lotw_rcvd = ?, lotw_sent = ?\
            \xc_in = ?, xc_out = ?, rst_rcvd = ?, rst_sent = ?, iota = ?, itu = ?\
            \waz_zone = ?, call = ?, sat_name = ?, sat_mode = ?\
            \WHERE date = ?, time = ?"
            (qsoToSql qso ++ [toSql date, toSql time]) >>
    return ()

-- Look up the QSO given by the supplied date and time, and remove it from the database.
removeQSO :: IConnection conn => conn -> ADIF.Date -> ADIF.Time -> IO ()
removeQSO dbh date time = do
    run dbh "DELETE FROM qsos WHERE date = ?, time = ?"
            [toSql date, toSql time]
    return ()

-- Return a list of all QSOs in the database.
getAllQSOs :: IConnection conn => conn -> IO ([QSO])
getAllQSOs dbh = do
    results <- quickQuery' dbh "SELECT * FROM qsos ORDER BY date,time" []
    return $ map sqlToQSO results

--
-- HELPER FUNCTIONS
--

-- Convert between QSO and SqlValue types.  Order is important on the lists.
qsoToSql :: QSO -> [SqlValue]
qsoToSql qso =
    [toSql $ qDate qso, toSql $ qTime qso, toSql $ qFreq qso, toSql $ qRxFreq qso, toSql $ show $ qMode qso,
     toSql $ qDXCC qso, toSql $ qGrid qso, toSql $ qState qso, toSql $ qName qso, toSql $ qQSL_RDate qso,
     toSql $ qQSL_SDate qso, toSql $ show $ qQSL_Rcvd qso, toSql $ show $ qQSL_Sent qso, toSql $ show $ qQSL_SentVia qso,
     toSql $ qNotes qso, toSql $ qLOTW_RDate qso, toSql $ qLOTW_SDate qso, toSql $ show $ qLOTW_Rcvd qso, toSql $ show $ qLOTW_Sent qso,
     toSql $ qXcIn qso, toSql $ qXcOut qso, toSql $ qRST_Rcvd qso, toSql $ qRST_Sent qso, toSql $ qIOTA qso,
     toSql $ qITU qso, toSql $ qWAZ qso, toSql $ qCall qso, toSql $ qSatName qso, toSql $ qSatMode qso]

sqlToQSO :: [SqlValue] -> QSO
sqlToQSO [date, time, freq, rx_freq, mode, dxcc, grid, state, name, qsl_rdate, qsl_sdate,
          qsl_rcvd, qsl_rcvd_via, qsl_sent, qsl_sent_via, notes, lotw_rdate, lotw_sdate,
          lotw_rcvd, lotw_sent, xc_in, xc_out, rst_rcvd, rst_sent, iota, itu, waz_zone,
          call, sat_name, sat_mode] =
    QSO {qDate = fromSql date, qTime = fromSql time, qFreq = fromSql freq, qRxFreq = fromSql rx_freq,
         qMode = read (fromSql mode) :: ADIF.Mode, qDXCC = fromSql dxcc, qGrid = fromSql grid, qState = fromSql state,
         qName = fromSql name, qQSL_RDate = fromSql qsl_rdate, qQSL_SDate = fromSql qsl_sdate,
         qQSL_Rcvd = read (fromSql qsl_rcvd) :: ADIF.ReceivedStatus,
         qQSL_RcvdVia = read (fromSql qsl_rcvd_via) :: ADIF.SentVia,
         qQSL_Sent = read (fromSql qsl_sent) :: ADIF.SentStatus,
         qQSL_SentVia = read (fromSql qsl_sent_via ) :: ADIF.SentVia, qNotes = fromSql notes,
         qLOTW_RDate = fromSql lotw_rdate, qLOTW_SDate = fromSql lotw_sdate,
         qLOTW_Rcvd = read (fromSql lotw_rcvd) :: ADIF.ReceivedStatus, qLOTW_Sent = read (fromSql lotw_sent) :: ADIF.SentStatus,
         qXcIn = fromSql xc_in, qXcOut = fromSql xc_out, qRST_Rcvd = fromSql rst_rcvd, qRST_Sent = fromSql rst_sent,
         qIOTA = fromSql iota, qITU = fromSql itu, qWAZ = fromSql waz_zone,
         qCall = fromSql call, qSatName = fromSql sat_name, qSatMode = fromSql sat_mode}
