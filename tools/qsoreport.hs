import qualified Data.Map as Map
import Data.ConfigFile
import Data.Ix(range)
import Data.List(sortBy)
import Data.Maybe(fromMaybe)
import System.Directory(getHomeDirectory)
import System.IO
import Text.XHtml.Strict
import Text.XHtml.Table

import DB(connect, getAllQSOs', getUnconfirmedQSOs)
import DXCC(DXCC(..), entityFromID)
import qualified Formats.ADIF.Types as ADIF
import QSO
import Utils(colonifyTime, dashifyDate, freqToBand)

type EntryTy = Bool
type ConfirmInfo = [(QSO, Bool)]

data BandRow = BandRow {
    row160M :: EntryTy,
    row80M :: EntryTy,
    row60M :: EntryTy,
    row40M :: EntryTy,
    row30M :: EntryTy,
    row20M :: EntryTy,
    row17M :: EntryTy,
    row15M :: EntryTy,
    row12M :: EntryTy,
    row10M :: EntryTy,
    row6M :: EntryTy,
    row2M :: EntryTy,
    row1Point25M :: EntryTy,
    row70CM :: EntryTy
 } deriving (Show)

emptyBandRow :: BandRow
emptyBandRow = BandRow { row160M=False, row80M=False, row60M=False, row40M=False, row30M=False,
                         row20M=False, row17M=False, row15M=False, row12M=False, row10M=False,
                         row6M=False, row2M=False, row1Point25M=False, row70CM=False }

--
-- CONFIG FILE PROCESSING CODE
--

data Config = Config {
    confDB :: String,
    confQTH :: String }

readConfigFile :: FilePath -> IO Config
readConfigFile f = do
    contents <- readFile f
    let config = do
        c <- readstring emptyCP contents
        database <- get c "DEFAULT" "database"
        qth <- get c "DEFAULT" "qth"
        return Config { confDB = database,
                        confQTH = qth }

    case config of
        Left cperr  -> fail $ show cperr
        Right c     -> return c

--
-- THE MAIN PROGRAM
--

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

report :: String -> (QSO -> Bool) -> ConfirmInfo -> Html
report caption f ci = concatHtml [
    table ! [border 1] << (toHtml tableBody),
    br,
    toHtml $ show (length results) ++ " " ++ caption ++ ", " ++ show (length $ filter id confirms) ++ " confirmed" ]
 where
    confirms = map snd ci
    results = map qsoToRow (filter (f . fst) ci)
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
reportAllQSOs :: ConfirmInfo -> Html
reportAllQSOs ci = report "QSOs logged" (\_ -> True) ci

-- Dump all QSOs for a specific band to HTML.
reportBandQSOs :: ADIF.Band -> ConfirmInfo -> Html
reportBandQSOs band ci =
    report ("QSOs logged for " ++ show band)
           (\q -> (freqToBand $ qFreq q) == band)
           ci

main :: IO ()
main = do
    -- Read in the config file.
    homeDir <- getHomeDirectory
    conf <- readConfigFile (homeDir ++ "/.slog")

    -- Open the database.  We do not have to close the database since that happens
    -- automatically.
    dbh <- connect $ confDB conf

    -- Reporting is a multiple step process:
    -- (1) Get all QSOs and all unconfirmed QSOs.
    qsos <- getAllQSOs' dbh
    unconfirmed <- getUnconfirmedQSOs dbh

    -- (2) Construct a list of tuples:  a QSO, and a boolean saying whether it's
    -- been confirmed or not.
    let confirms = map (`notElem` unconfirmed) qsos
    let ci = zip qsos confirms

    -- (3) Filter the results based on band, call, or whatever else was requested
    -- on the command line.

    -- (4) Convert to HTML based upon whatever header and body formatting was requested
    -- on the command line.  This is what makes it a report.
    let html = showHtml $ reportAllQSOs ci

    -- (5) Display.
    putStrLn html
