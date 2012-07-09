module Report(reportAll,
              reportDXCC,
              reportVUCC)
 where

import Data.List(nubBy)
import qualified Data.Map as Map
import System.IO
import Text.XHtml.Strict
import Text.XHtml.Table

import Slog.DXCC(DXCC(..), entityFromID)
import qualified Slog.Formats.ADIF.Types as ADIF
import Slog.QSO
import Slog.Utils(colonifyTime, dashifyDate, freqToBand)

import Types(ConfirmInfo, EntryTy, emptyBandRow)

-- This defines a row in a general query table, fitting the header given below.
qsoToRow :: (QSO, Bool) -> HtmlTable
qsoToRow (qso, isConfirmed) =
    besides [td $ toHtml (dashifyDate $ qDate qso),
             td $ toHtml (colonifyTime $ qTime qso),
             td $ toHtml (qCall qso),
             td $ toHtml (show $ qFreq qso),
             td $ toHtml (show $ qMode qso),
             td $ toHtml (qDXCC qso >>= entityFromID >>= Just . dxccEntity),
             td $ toHtml (qGrid qso),
             td $ toHtml (maybe "" show $ qITU qso),
             td $ toHtml (maybe "" show $ qWAZ qso),
             td $ toHtml (if isConfirmed then "Y" else "")
     ]

-- This header is suitable for printing out general queries - dumping all logged QSOs,
-- dumping all QSOs for a specific band or specific call, etc.
tableHeader :: HtmlTable
tableHeader =
    besides [(th $ toHtml "Date"),
             (th $ toHtml "Time"),
             (th $ toHtml "Call"),
             (th $ toHtml "Frequency"),
             (th $ toHtml "Mode"),
             (th $ toHtml "DXCC"),
             (th $ toHtml "Grid"),
             (th $ toHtml "ITU"),
             (th $ toHtml "WAZ"),
             (th $ toHtml "Confirmed")
     ]

report :: String -> [ConfirmInfo] -> Html
report caption ci = concatHtml [
    table ! [border 1] << (toHtml tableBody),
    br,
    toHtml $ show nQSOs ++ " " ++ caption ++ ", " ++ show nConfirmed ++ " confirmed" ]
 where
    nQSOs = length results
    nConfirmed = length $ filter id (map snd ci)

    results = map qsoToRow ci

    tableBody = if length results == 0 then tableHeader
                else tableHeader `above` aboves results

-- Just dump all logged QSOs to HTML.
reportAll :: [ConfirmInfo] -> Html
reportAll = report "QSOs logged"

-- Dump all logged QSOs for various DXCC awards.  The results of this can be filtered down
-- further by band or mode to get the sub-awards.
reportDXCC :: [ConfirmInfo] -> Html
reportDXCC ci = report "DXCC QSOs logged" (zip uniq $ repeat True)
 where
    dxccEq qsoA qsoB = case (qDXCC qsoA, qDXCC qsoB) of
        (Just a, Just b) -> a == b
        _                -> False

    -- Remove the confirmation info, since everything's confirmed.
    ci' = fst $ unzip ci

    -- Reduce the list to only unique entities.
    uniq = nubBy dxccEq ci'

-- Dump all logged QSOs for the VUCC award.
reportVUCC :: [ConfirmInfo] -> Html
reportVUCC ci = report "VUCC QSOs logged" (zip uniq $ repeat True)
 where
    modifyGrid qso = maybe qso (\grid -> qso { qGrid = Just $ take 4 grid }) (qGrid qso)

    gridsEq qsoA qsoB = (qGrid qsoA) == (qGrid qsoB)

    -- Remove the confirmation info, since everything's confirmed.
    ci' = fst $ unzip ci

    -- Grab everything 50 MHz and up.
    sixAndUp = filter (\qso -> maybe False (>= ADIF.Band6M) (freqToBand $ qFreq qso)) ci'

    -- Reduce the grid element of each QSO to just four characters.
    trimmedGrids = map modifyGrid sixAndUp

    -- And then reduce the list to only unique grid squares.
    uniq = nubBy gridsEq trimmedGrids
