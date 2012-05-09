module Report(reportAll) where

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

-- For reporting on award status, we really only want a table of bands.  Also
-- I only care about a subset.
awardHeader :: HtmlTable
awardHeader = let
    bands = [ADIF.Band160M, ADIF.Band80M, ADIF.Band60M, ADIF.Band40M, ADIF.Band30M,
             ADIF.Band20M, ADIF.Band17M, ADIF.Band15M, ADIF.Band12M, ADIF.Band10M,
             ADIF.Band6M, ADIF.Band2M, ADIF.Band1Point25M, ADIF.Band70CM]
 in
    besides $ [(th $ toHtml "")] ++ map (th . toHtml . show) bands ++ [th $ toHtml "Total"]

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

reportAward :: (Ord a, Show a) => [a] -> (QSO -> Maybe EntryTy) -> [QSO] -> Html
reportAward keys fn qsos = let
    initial = Map.fromList $ map (\v -> (v, emptyBandRow)) keys
 in
    concatHtml [table ! [border 1] << (toHtml tableBody),
                br]
 where
    tableBody = awardHeader `above` aboves (map (td . toHtml . show) keys)

-- Just dump all logged QSOs to HTML.
reportAll :: [ConfirmInfo] -> Html
reportAll = report "QSOs logged"
