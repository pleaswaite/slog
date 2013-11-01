import Control.Monad.Trans(liftIO)
import Data.List(find)
import Data.Maybe(catMaybes, fromJust)
import Text.Printf(printf)

import Slog.DB(confirmQSO, findQSOByDateTime, getUnconfirmedQSOs, getQSO, runTransaction, Transaction)
import Slog.DXCC(DXCC(dxccEntity), entityFromID)
import Slog.Formats.ADIF.Parser(parseString)
import Slog.Formats.ADIF.Utils(freqToBand)
import qualified Slog.Formats.ADIF.Types as ADIF
import Slog.LOTW(download)
import Slog.QSO(QSO(qCall, qDate, qDXCC, qFreq, qTime))
import Slog.Utils(colonifyTime, dashifyDate, withoutSeconds)

import ToolLib.Config

type QSLInfo = (ADIF.Date, ADIF.Time, ADIF.Date)

-- Extract the date and time of a QSO along with the QSL received date from the ADIF
-- data.  Converting ADIF to a full QSO structure is just way too difficult to
-- do, as the find* functions below illustrate.
adifDateTime :: [ADIF.Field] -> Maybe QSLInfo
adifDateTime fields = let
    findDate flds = find isDate flds >>= \(ADIF.QSO_Date v) -> Just v
                    where isDate (ADIF.QSO_Date _) = True
                          isDate _                 = False

    findQSLDate flds = find isDate flds >>= \(ADIF.QSL_RDate v) -> Just v
                       where isDate (ADIF.QSL_RDate _) = True
                             isDate _                  = False

    findTime flds = find isTime flds >>= \(ADIF.TimeOn v) -> Just v
                    where isTime (ADIF.TimeOn _) = True
                          isTime _               = False
 in
    findQSLDate fields >>= \date -> Just (fromJust $ findDate fields, fromJust $ findTime fields, fromJust date)

logMessage :: QSO -> String
logMessage qso =
    printf "Confirmed QSO: %s (%s) on %s at %s %s" call entity band date time
 where
    call = qCall qso
    entity = maybe "unknown entity" dxccEntity (qDXCC qso >>= entityFromID)
    band = maybe "unknown band" show $ freqToBand (qFreq qso)
    date = dashifyDate $ qDate qso
    time = colonifyTime $ qTime qso

doConfirm :: QSLInfo -> Transaction ()
doConfirm (date, time, qsl_date) = do
    qsoid <- findQSOByDateTime date (withoutSeconds time)
    qso <- getQSO qsoid

    confirmQSO qsoid qsl_date
    liftIO $ (putStrLn . logMessage) qso
    return ()

filterPreviousConfirmations :: [QSO] -> [Maybe QSLInfo] -> [QSLInfo]
filterPreviousConfirmations qsos infos = let
    -- qsos is a list of unconfirmed QSO objects as understood by our local database.
    -- Convert it into a list of QSLInfo tuples (where the third field doesn't really
    -- matter).
    unconfirmedDateTimes = map (\qso -> (qDate qso, qTime qso, qDate qso)) qsos
 in
    -- Then, remove all the info tuples that are not already confirmed.  This leaves
    -- us with just a list of unconfirmed tuples which is suitable for feeding into
    -- the database.
    filter (\info -> info `memberOf` unconfirmedDateTimes)
           (catMaybes infos)
 where
    memberOf _ [] = False
    memberOf left@(lDate, lTime, _) ((rDate, rTime, _):lst) = (lDate == rDate && (withoutSeconds lTime) == (withoutSeconds rTime)) || left `memberOf` lst

confirmQSOs :: [QSLInfo] -> Transaction ()
confirmQSOs qsos = do
    mapM_ doConfirm qsos

main :: IO ()
main = do
    -- Read in the config file.
    conf <- readConfig

    -- Get the on-disk location of the database.
    let fp = confDB conf

    -- Determine the earliest unconfirmed QSO.  We don't want to download everything, as
    -- that might be a whole lot of ADIF data.  However, there's really not a better way
    -- to figure out what needs to be confirmed except for iterating over every one and
    -- spamming the LOTW server with requests.  They probably wouldn't appreciate that.
    unconfirmeds <- runTransaction fp getUnconfirmedQSOs
    let earliestUnconfirmed = dashifyDate $ qDate $ unconfirmeds !! 0

    -- Grab the confirmed QSOs from LOTW.
    str <- download earliestUnconfirmed (confUsername conf) (confPassword conf)

    -- Now iterate over all the new ADIF data and extract date/times for each.  Mark each
    -- as confirmed.
    case parseString str of
        Left err -> putStrLn $ (show err) ++ "\n\nin input\n\n" ++ str
        Right (ADIF.ADIFFile {ADIF.fileBody=adifs}) -> let
            infos = map adifDateTime adifs
            unconfirmedInfos = filterPreviousConfirmations unconfirmeds infos
         in
            runTransaction fp $ confirmQSOs unconfirmedInfos

    return ()
