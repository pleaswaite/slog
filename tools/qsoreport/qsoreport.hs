import qualified Data.Map as Map
import Data.ConfigFile
import System.Console.GetOpt
import System.Directory(getHomeDirectory)
import System.Environment(getArgs)
import System.Exit(ExitCode(..), exitWith)
import System.IO
import Text.XHtml.Strict
import Text.XHtml.Table

import DB(connect, getAllQSOs', getUnconfirmedQSOs)
import DXCC(DXCC(..), entityFromID)
import qualified Formats.ADIF.Types as ADIF
import QSO
import Utils(colonifyTime, dashifyDate, freqToBand, uppercase)

import qualified Filter as F
import Report(reportAll)
import Types(ConfirmInfo)

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
-- OPTION PROCESSING CODE
--

type FilterFunc = (ConfirmInfo -> Bool)

data Options = Options {
    optFilter :: [FilterFunc] }

type OptAction = (Options -> IO Options)

defaultOptions :: Options
defaultOptions = Options {
    optFilter = [F.byNone] }

mkAction opt f =
    return opt { optFilter = optFilter opt ++ [f] }

opts :: [OptDescr OptAction]
opts = [
    Option [] ["filter-band"]           (ReqArg (\arg opt -> mkAction opt (F.byBand (read arg :: ADIF.Band))) "BAND")
           "filter by band",
    Option [] ["filter-call"]           (ReqArg (\arg opt -> mkAction opt (F.byCall $ uppercase arg)) "CALL")
           "filter QSOs by call sign",
    Option [] ["filter-confirmed"]      (NoArg (\opt -> mkAction opt (F.byConfirmed True)))
           "filter confirmed QSOs",
    Option [] ["filter-dxcc"]           (ReqArg (\arg opt -> mkAction opt (F.byDXCC (read arg :: Integer))) "DXCC")
           "filter by DXCC entity",
    Option [] ["filter-itu"]            (ReqArg (\arg opt -> mkAction opt (F.byITU (read arg :: Integer))) "ITU")
           "filter by ITU zone",
    Option [] ["filter-mode"]           (ReqArg (\arg opt -> mkAction opt (F.byMode (read arg :: ADIF.Mode))) "MODE")
           "filter by mode",
    Option [] ["filter-unconfirmed"]    (NoArg (\opt -> mkAction opt (F.byConfirmed False)))
           "filter unconfirmed QSOs",
    Option [] ["filter-waz"]            (ReqArg (\arg opt -> mkAction opt (F.byWAZ (read arg :: Integer))) "WAZ")
           "filter by WAZ zone",
    Option ['h'] ["help"]               (NoArg   (\_ -> do putStrLn (usageInfo "qsoreport" opts)
                                                           exitWith ExitSuccess))
           "print program usage"
 ]

handleOpts :: [String] -> IO ([OptAction], [String])
handleOpts argv =
    case getOpt RequireOrder opts argv of
        (o, n, [])      -> return (o, n)
        (_, _, errs)    -> ioError (userError (concat errs ++ usageInfo header opts))
                           where header = "Usage: qsoreport [OPTIONS]"

processArgs argsFunc = do
    (actions, _) <- argsFunc
    foldl (>>=) (return defaultOptions) actions

--
-- THE MAIN PROGRAM
--

main :: IO ()
main = do
    -- Process command line arguments.
    cmdline <- processArgs (getArgs >>= handleOpts)

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
    let ci' = foldl (flip filter) ci (optFilter cmdline)

    -- (4) Convert to HTML based upon whatever header and body formatting was requested
    -- on the command line.  This is what makes it a report.
    let html = showHtml $ reportAll ci'

    -- (5) Display.
    putStrLn html
