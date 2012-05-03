import qualified Data.Map as Map
import Data.ConfigFile
import System.Directory(getHomeDirectory)
import System.IO
import Text.XHtml.Strict
import Text.XHtml.Table

import DB(connect, getAllQSOs', getUnconfirmedQSOs)
import DXCC(DXCC(..), entityFromID)
import qualified Formats.ADIF.Types as ADIF
import QSO
import Utils(colonifyTime, dashifyDate, freqToBand)

import Report(reportAll)

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
    let html = showHtml $ reportAll ci

    -- (5) Display.
    putStrLn html
