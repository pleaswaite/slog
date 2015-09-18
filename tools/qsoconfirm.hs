{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Monad(when)
import Control.Monad.Trans(liftIO)
import Data.List(find)
import Data.Maybe(catMaybes, fromJust)
import Data.Tuple.Utils(snd3)
import Text.Printf(printf)

import Slog.DB(DBResult, confirmQSO, getQSO, getUnconfirmedQSOs, initDB)
import Slog.DXCC(DXCC(dxccEntity), entityFromID)
import Slog.Formats.ADIF.Parser(parseString)
import Slog.Formats.ADIF.Utils(freqToBand)
import qualified Slog.Formats.ADIF.Types as ADIF
import Slog.LOTW(download)
import Slog.QSO(QSO(..))
import Slog.Utils(colonifyTime, dashifyDate, withoutSeconds)

import ToolLib.Config

data QSLInfo = QSLInfo { qiBand :: ADIF.Band,
                         qiCall :: String,
                         qiDate :: ADIF.Date,
                         qiTime :: ADIF.Time,
                         qiQSLDate :: ADIF.Date }

instance Eq QSLInfo where
    a == b = (qiBand a == qiBand b) &&
             (qiCall a == qiCall b) &&
             (qiDate a == qiDate b) &&
             (withoutSeconds (qiTime a) == withoutSeconds (qiTime b))

-- Extract the date and time of a QSO along with the QSL received date from the ADIF
-- data.  Converting ADIF to a full QSO structure is just way too difficult to
-- do, as the find* functions below illustrate.
mkQSLInfo :: [ADIF.Field] -> Maybe QSLInfo
mkQSLInfo fields = let
    findBand flds = find isBand flds >>= \(ADIF.Band b) -> Just b
                    where isBand (ADIF.Band _) = True
                          isBand _             = False

    findCall flds = find isCall flds >>= \(ADIF.Call c) -> Just c
                    where isCall (ADIF.Call _) = True
                          isCall _             = False

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
    findQSLDate fields >>= \date -> Just QSLInfo { qiBand = fromJust $ findBand fields,
                                                   qiCall = fromJust $ findCall fields,
                                                   qiDate = fromJust $ findDate fields,
                                                   qiTime = fromJust $ findTime fields,
                                                   qiQSLDate = fromJust date }

logMessage :: QSO -> String
logMessage QSO{..} =
    printf "Confirmed QSO: %s (%s) on %s at %s %s" qCall entity band date time
 where
    entity = maybe "unknown entity" dxccEntity (qDXCC >>= entityFromID)
    band = maybe "unknown band" show $ freqToBand qFreq
    date = dashifyDate qDate
    time = colonifyTime qTime

doConfirm :: FilePath -> QSLInfo -> IO ()
doConfirm fp QSLInfo{..} = do
    -- LOTW gives us band information, but our database stores everything in terms of
    -- frequency.  Thus we query the database for all QSOs matching time and call.  This
    -- could possibly return multiple results, across multiple bands.  We'll find the
    -- right one later.
    results <- getQSO fp (Just qiDate)
                         (Just $ withoutSeconds qiTime)
                         (Just qiCall)
                         Nothing
    mapM_ confirmOne results
 where
    confirmOne (i, q, _) =
        -- If this QSO is on the same band as the QSLInfo object, it must be the one
        -- we want to confirm.  This guard basically does the filtering that we wish
        -- the database could have given us above, if we didn't have to work in terms
        -- of frequency.
        when (maybe False (qiBand ==) (freqToBand $ qFreq q))
             (do confirmQSO fp i qiQSLDate
                 liftIO $ (putStrLn . logMessage) q)

filterPreviousConfirmations :: [DBResult] -> [Maybe QSLInfo] -> [QSLInfo]
filterPreviousConfirmations results infos = let
    -- results is a list of unconfirmed QSO objects as understood by our local database.
    -- Convert it into a list of QSLInfo objects (where the qiQSLDate field doesn't
    -- really matter).
    unconfirmed = map (\(_, QSO{..}, _) -> QSLInfo { qiBand = fromJust $ freqToBand qFreq,
                                                     qiCall = qCall,
                                                     qiDate = qDate,
                                                     qiTime = qTime,
                                                     qiQSLDate = qDate })
                      results
 in
    -- Then, remove all the QSLInfo objects that are not already confirmed.  This leaves
    -- us with just a list of unconfirmed objects which is suitable for feeding into
    -- the database.
    filter (`elem` unconfirmed) (catMaybes infos)

confirmQSOs :: FilePath -> [QSLInfo] -> IO ()
confirmQSOs fp =
    mapM_ (doConfirm fp)

main :: IO ()
main = do
    -- Read in the config file.
    Config{..} <- readConfig

    -- Get the on-disk location of the database.
    initDB confDB
    let fp = confDB

    -- Determine the earliest unconfirmed QSO.  We don't want to download everything, as
    -- that might be a whole lot of ADIF data.  However, there's really not a better way
    -- to figure out what needs to be confirmed except for iterating over every one and
    -- spamming the LOTW server with requests.  They probably wouldn't appreciate that.
    unconfirmedResults <- getUnconfirmedQSOs fp
    let earliestUnconfirmed = dashifyDate . qDate $ snd3 $ head unconfirmedResults

    -- Grab the confirmed QSOs from LOTW.
    str <- download earliestUnconfirmed confUsername confPassword

    -- Now iterate over all the new ADIF data and extract date/times for each.  Mark each
    -- as confirmed.
    case parseString str of
        Left err -> putStrLn $ printf "%s\n\nin input\n\n%s" (show err) str
        Right (ADIF.ADIFFile {ADIF.fileBody=adifs}) -> let
            infos = map mkQSLInfo adifs
            unconfirmedInfos = filterPreviousConfirmations unconfirmedResults infos
         in
            confirmQSOs fp unconfirmedInfos
