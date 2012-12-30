module Report(reportAll,
              reportChallenge,
              reportDXCC,
              reportVUCC)
 where

import Data.List(groupBy, nubBy, sortBy)
import Text.XHtml.Strict hiding(caption)
import Text.XHtml.Table

import Slog.DXCC(DXCC(..), entityFromID)
import qualified Slog.Formats.ADIF.Types as ADIF
import Slog.Formats.ADIF.Utils(freqToBand)
import Slog.QSO
import Slog.Utils(colonifyTime, dashifyDate)

import Types(ConfirmInfo)

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

-- The DXCC challenge award requires a lot of special code.
data ChallengeRec = ChallengeRec { c160M      :: [QSO],
                                   c80M       :: [QSO],
                                   c40M       :: [QSO],
                                   c30M       :: [QSO],
                                   c20M       :: [QSO],
                                   c17M       :: [QSO],
                                   c15M       :: [QSO],
                                   c12M       :: [QSO],
                                   c10M       :: [QSO],
                                   c6M        :: [QSO],
                                   cOther     :: [QSO] }

data ChallengeCount = ChallengeCount { n160M       :: Integer,
                                       n80M        :: Integer,
                                       n40M        :: Integer,
                                       n30M        :: Integer,
                                       n20M        :: Integer,
                                       n17M        :: Integer,
                                       n15M        :: Integer,
                                       n12M        :: Integer,
                                       n10M        :: Integer,
                                       n6M         :: Integer,
                                       nOther      :: Integer }

mkChallengeRec :: ChallengeRec
mkChallengeRec = ChallengeRec { c160M = [],
                                c80M = [],
                                c40M = [],
                                c30M = [],
                                c20M = [],
                                c17M = [],
                                c15M = [],
                                c12M = [],
                                c10M = [],
                                c6M = [],
                                cOther = [] }

mkCountRec :: ChallengeCount
mkCountRec = ChallengeCount { n160M = 0,
                              n80M = 0,
                              n40M = 0,
                              n30M = 0,
                              n20M = 0,
                              n17M = 0,
                              n15M = 0,
                              n12M = 0,
                              n10M = 0,
                              n6M = 0,
                              nOther = 0 }

recToRow :: (ChallengeRec, QSO) -> HtmlTable
recToRow (record, qso) =
    besides $ [td $ toHtml $ qDXCC qso >>= entityFromID >>= Just . dxccEntity] ++
              map (htmlCall record) [c160M, c80M, c40M, c30M, c20M, c17M, c15M, c12M, c10M, c6M]
 where
    firstQSO rec band =
        if length lst == 0 then Nothing else Just $ lst !! 0
     where
        lst = sortBy dateSorter (band rec)

    dateSorter qsoA qsoB = case compare (qDate qsoA) (qDate qsoB) of
        EQ -> compare (qTime qsoA) (qTime qsoB)
        c  -> c

    htmlCall rec band = td $ toHtml $ maybe "" qCall (firstQSO rec band)

challengeHeader :: HtmlTable
challengeHeader = besides $ [th $ toHtml "DXCC"] ++ map (th . toHtml . show) challengeBands
 where
    challengeBands = [ADIF.Band160M, ADIF.Band80M, ADIF.Band40M, ADIF.Band30M,
                      ADIF.Band20M, ADIF.Band17M, ADIF.Band15M, ADIF.Band12M, ADIF.Band10M,
                      ADIF.Band6M]

reportChallenge :: [ConfirmInfo] -> Html
reportChallenge ci = table ! [border 1] << (toHtml tableBody)
 where
    dxccSorter qsoA qsoB = case (qDXCC qsoA, qDXCC qsoB) of
        (Just a, Just b) -> compare nameA nameB
                             where
                                 nameA = entityFromID a >>= Just . dxccEntity
                                 nameB = entityFromID b >>= Just . dxccEntity
        _                -> EQ

    dxccGrouper qsoA qsoB = dxccSorter qsoA qsoB == EQ

    createRecords dxccList = foldl addToRec mkChallengeRec dxccList
     where
        addToRec rec entry = case freqToBand (qFreq entry) of
            Just ADIF.Band160M      -> rec { c160M = (c160M rec) ++ [entry] }
            Just ADIF.Band80M       -> rec { c80M = (c80M rec) ++ [entry] }
            Just ADIF.Band40M       -> rec { c40M = (c40M rec) ++ [entry] }
            Just ADIF.Band30M       -> rec { c30M = (c30M rec) ++ [entry] }
            Just ADIF.Band20M       -> rec { c20M = (c20M rec) ++ [entry] }
            Just ADIF.Band17M       -> rec { c17M = (c17M rec) ++ [entry] }
            Just ADIF.Band15M       -> rec { c15M = (c15M rec) ++ [entry] }
            Just ADIF.Band12M       -> rec { c12M = (c12M rec) ++ [entry] }
            Just ADIF.Band10M       -> rec { c10M = (c10M rec) ++ [entry] }
            Just ADIF.Band6M        -> rec { c6M = (c6M rec) ++ [entry] }
            _                       -> rec

    -- Remove the confirmation info, since everything's confirmed.
    ci' = fst $ unzip ci

    -- Create a new list where each element is a list of all QSOs for a specific
    -- DXCC entity.
    dxccBuckets = groupBy dxccGrouper (sortBy dxccSorter ci')

    -- Then convert that list into a list of tuples.  The first element is a ChallengeRec
    -- where each element holds a list of QSOs for a single band.  The second element
    -- is the first QSO, from which we can extract the DXCC entity later on.
    bandDxccBuckets = map (\bucket -> (createRecords bucket, bucket !! 0)) dxccBuckets

    results = map recToRow bandDxccBuckets
    totals = countUp mkCountRec bandDxccBuckets
     where
        a ++? b = a + (if length b > 0 then 1 else 0)

        addTo rec result = rec { n160M = (n160M rec) ++? (c160M result),
                                 n80M = (n80M rec) ++? (c80M result),
                                 n40M = (n40M rec) ++? (c40M result),
                                 n30M = (n30M rec) ++? (c30M result),
                                 n20M = (n20M rec) ++? (c20M result),
                                 n17M = (n17M rec) ++? (c17M result),
                                 n15M = (n15M rec) ++? (c15M result),
                                 n12M = (n12M rec) ++? (c12M result),
                                 n10M = (n10M rec) ++? (c10M result),
                                 n6M = (n6M rec) ++? (c6M result) }
        countUp rec ((r, _):lst) = countUp (addTo rec r) lst
        countUp rec [] = rec

    reportTotals rec = besides $ [th $ toHtml "Total"] ++
                       map (\band -> th $ toHtml $ show (band rec))
                           [n160M, n80M, n40M, n30M, n20M, n17M, n15M, n12M, n10M, n6M]

    tableBody = if length results == 0 then challengeHeader
                else challengeHeader `above` aboves results `above` (reportTotals totals)

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
    -- Due to the way the VUCC award works, we cannot rely on the grid square given by
    -- doing a lookup.  The other station could be in a different grid than their QTH.
    -- Thus, we need to use the grid from the exchange.  For ease of reporting, though,
    -- I'm just going to stick that value back into the grid field so the rest of the
    -- reporting code does not need to be changed.
    modifyGrid qso = maybe qso (\grid -> qso { qGrid = Just $ take 4 grid }) (qXcIn qso)

    gridsEq qsoA qsoB = (qGrid qsoA) == (qGrid qsoB)

    -- Remove the confirmation info, since everything's confirmed.
    ci' = fst $ unzip ci

    -- Grab everything 50 MHz and up.
    sixAndUp = filter (\qso -> maybe False (>= ADIF.Band6M) (freqToBand $ qFreq qso)) ci'

    -- Reduce the grid element of each QSO to just four characters.
    trimmedGrids = map modifyGrid sixAndUp

    -- And then reduce the list to only unique grid squares.
    uniq = nubBy gridsEq trimmedGrids
