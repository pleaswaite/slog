module Report(reportAll,
              reportChallenge,
              reportDXCC,
              reportVUCC)
 where

import Data.List(groupBy, nubBy, sortBy)
import qualified Data.Map as Map
import Data.Ord(comparing)
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

-- The DXCC challenge award requires a lot of special code.
data ChallengeRec = ChallengeRec { c160M      :: [QSO],
                                   c80M       :: [QSO],
                                   c60M       :: [QSO],
                                   c40M       :: [QSO],
                                   c30M       :: [QSO],
                                   c20M       :: [QSO],
                                   c17M       :: [QSO],
                                   c15M       :: [QSO],
                                   c12M       :: [QSO],
                                   c10M       :: [QSO],
                                   c6M        :: [QSO],
                                   c2M        :: [QSO],
                                   c1Point25M :: [QSO],
                                   c70CM      :: [QSO],
                                   c23CM      :: [QSO],
                                   other      :: [QSO] }

mkChallengeRec :: ChallengeRec
mkChallengeRec = ChallengeRec { c160M = [],
                                c80M = [],
                                c60M = [],
                                c40M = [],
                                c30M = [],
                                c20M = [],
                                c17M = [],
                                c15M = [],
                                c12M = [],
                                c10M = [],
                                c6M = [],
                                c2M = [],
                                c1Point25M = [],
                                c70CM = [],
                                c23CM = [],
                                other = [] }

recToRow :: (ChallengeRec, QSO) -> HtmlTable
recToRow (rec, qso) =
    besides $ [td $ toHtml $ qDXCC qso >>= entityFromID >>= Just . dxccEntity] ++
              map (htmlCall rec) [c160M, c80M, c60M, c40M, c30M, c20M, c17M, c15M,
                                  c12M, c10M, c6M, c2M, c1Point25M, c70CM, c23CM]
 where
    firstQSO rec band =
        if length lst == 0 then Nothing else Just $ lst !! 0
     where
        lst = sortBy dateSorter (band rec)

    dateSorter qsoA qsoB = case compare (qDate qsoA) (qDate qsoB) of
        EQ -> compare (qTime qsoA) (qTime qsoB)
        c  -> c

    htmlCall rec band = td $ toHtml $ maybe "" qCall (firstQSO rec band)

challengeHeader = besides $ [th $ toHtml "DXCC"] ++ map (th . toHtml . show) challengeBands
 where
    challengeBands = [ADIF.Band160M, ADIF.Band80M, ADIF.Band60M, ADIF.Band40M, ADIF.Band30M,
                      ADIF.Band20M, ADIF.Band17M, ADIF.Band15M, ADIF.Band12M, ADIF.Band10M,
                      ADIF.Band6M, ADIF.Band2M, ADIF.Band1Point25M, ADIF.Band70CM, ADIF.Band23CM]

reportChallenge :: [ConfirmInfo] -> Html
reportChallenge ci = table ! [border 1] << (toHtml tableBody)
 where
    dxccSorter qsoA qsoB = case (qDXCC qsoA, qDXCC qsoB) of
        (Just a, Just b) -> compare a b
        _                -> EQ

    dxccGrouper qsoA qsoB = dxccSorter qsoA qsoB == EQ

    createRecords dxccList = foldl addToRec mkChallengeRec dxccList
     where
        addToRec rec entry = case freqToBand (qFreq entry) of
            Just ADIF.Band160M      -> rec { c160M = (c160M rec) ++ [entry] }
            Just ADIF.Band80M       -> rec { c80M = (c80M rec) ++ [entry] }
            Just ADIF.Band60M       -> rec { c60M = (c60M rec) ++ [entry] }
            Just ADIF.Band40M       -> rec { c40M = (c40M rec) ++ [entry] }
            Just ADIF.Band30M       -> rec { c30M = (c30M rec) ++ [entry] }
            Just ADIF.Band20M       -> rec { c20M = (c20M rec) ++ [entry] }
            Just ADIF.Band17M       -> rec { c17M = (c17M rec) ++ [entry] }
            Just ADIF.Band15M       -> rec { c15M = (c15M rec) ++ [entry] }
            Just ADIF.Band12M       -> rec { c12M = (c12M rec) ++ [entry] }
            Just ADIF.Band10M       -> rec { c10M = (c10M rec) ++ [entry] }
            Just ADIF.Band6M        -> rec { c6M = (c6M rec) ++ [entry] }
            Just ADIF.Band2M        -> rec { c2M = (c2M rec) ++ [entry] }
            Just ADIF.Band1Point25M -> rec { c1Point25M = (c1Point25M rec) ++ [entry] }
            Just ADIF.Band70CM      -> rec { c70CM = (c70CM rec) ++ [entry] }
            Just ADIF.Band23CM      -> rec { c23CM = (c23CM rec) ++ [entry] }
            _                       -> rec { c160M = (other rec) ++ [entry] }

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

    tableBody = if length results == 0 then challengeHeader
                else challengeHeader `above` aboves results

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
