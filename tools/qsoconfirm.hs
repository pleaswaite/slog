import Data.ConfigFile
import Data.List(find)
import Data.Maybe(fromJust, isJust)
import Database.HDBC
import System.Directory(getHomeDirectory)
import System.IO

import Slog.DB(confirmQSO, connect, findQSOByDateTime, getUnconfirmedQSOs)
import Slog.Formats.ADIF.Parser(parseString)
import qualified Slog.Formats.ADIF.Types as ADIF
import Slog.LOTW(download)
import Slog.QSO(QSO(qDate))
import Slog.Utils(dashifyDate, withoutSeconds)

type QSLInfo = (ADIF.Date, ADIF.Time, Maybe ADIF.Date)

--
-- CONFIG FILE PROCESSING CODE
--

data Config = Config {
    confDB :: String,
    confUsername :: String,
    confPassword :: String }

readConfigFile :: FilePath -> IO Config
readConfigFile f = do
    contents <- readFile f
    let config = do
        c <- readstring emptyCP contents
        database <- get c "DEFAULT" "database"
        username <- get c "LOTW" "username"
        password <- get c "LOTW" "password"
        return Config { confDB = database,
                        confUsername = username,
                        confPassword = password }

    case config of
        Left cperr   -> fail $ show cperr
        Right c      -> return c

--
-- THE MAIN PROGRAM
--

-- Extract the date and time of a QSO along with the QSL received date from the ADIF
-- data.  Converting ADIF to a full QSO structure is just way too difficult to
-- do, as the find* functions below illustrate.
adifDateTime :: [ADIF.Field] -> QSLInfo
adifDateTime fields = let
    findDate fields = find isDate fields >>= \(ADIF.QSO_Date v) -> Just v
                      where isDate (ADIF.QSO_Date _) = True
                            isDate _                 = False

    findQSLDate fields = find isDate fields >>= \(ADIF.QSL_RDate v) -> v
                         where isDate (ADIF.QSL_RDate _) = True
                               isDate _                  = False
    findTime fields = find isTime fields >>= \(ADIF.TimeOn v) -> Just v
                      where isTime (ADIF.TimeOn _) = True
                            isTime _               = False
 in
    (fromJust $ findDate fields, fromJust $ findTime fields, findQSLDate fields)

doConfirm :: IConnection conn => conn -> (ADIF.Date, ADIF.Time, ADIF.Date) -> IO ()
doConfirm dbh (date, time, qsl_date) = handleSql errorHandler $ do
    qsoid <- findQSOByDateTime dbh date (withoutSeconds time)
    confirmQSO dbh qsoid qsl_date
 where
    errorHandler e = do fail $ "Error confirming QSO:  No QSO with this date and time exists.\n" ++ show e

confirmQSOs :: IConnection conn => conn -> [QSLInfo] -> IO ()
confirmQSOs dbh qsos = do
    -- Filter out all QSOs without a QSL received date.
    let qsos' = map (\(date, time, qsl) -> (date, time, fromJust qsl)) $
                    filter (\(_, _, qsl) -> isJust qsl) qsos

    -- And finally, mark each as confirmed.
    mapM_ (doConfirm dbh) qsos'

main :: IO ()
main = do
    -- Read in the config file.
    homeDir <- getHomeDirectory
    conf <- readConfigFile (homeDir ++ "/.slog")

    -- Open the database.  We do not have to close the database since that happens
    -- automatically.
    dbh <- connect $ confDB conf

    -- Determine the earliest unconfirmed QSO.  We don't want to download everything, as
    -- that might be a whole lot of ADIF data.  However, there's really not a better way
    -- to figure out what needs to be confirmed except for iterating over every one and
    -- spamming the LOTW server with requests.  They probably wouldn't appreciate that.
    unconfirmeds <- getUnconfirmedQSOs dbh
    let earliestUnconfirmed = dashifyDate $ qDate $ unconfirmeds !! 0

    -- Grab the confirmed QSOs from LOTW.
    str <- download earliestUnconfirmed (confUsername conf) (confPassword conf)
    let adifs = parseString str

    -- Now iterate over all the new ADIF data and extract date/times for each.  Mark each
    -- as confirmed.
    case parseString str of
        Left err -> putStrLn $ show err
        Right (ADIF.ADIFFile {ADIF.fileBody=adifs}) -> confirmQSOs dbh (map adifDateTime adifs)

    return ()
