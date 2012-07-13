import qualified Data.Map as Map
import Data.ConfigFile
import System.Console.GetOpt
import System.Directory(getHomeDirectory)
import System.Environment(getArgs)
import System.Exit(ExitCode(..), exitWith)
import System.IO
import Text.XHtml.Strict(Html, showHtml)

import Slog.DB(connect, getAllQSOs', getUnconfirmedQSOs)
import Slog.DXCC(DXCC(..), entityFromID)
import qualified Slog.Formats.ADIF.Types as ADIF
import Slog.QSO
import Slog.Utils(colonifyTime, dashifyDate, uppercase)

import qualified Filter as F
import Report(reportAll, reportChallenge, reportDXCC, reportVUCC)
import Types(ConfirmInfo)

--
-- CONFIG FILE PROCESSING CODE
--

data Config = Config {
    confDB :: String }

readConfigFile :: FilePath -> IO Config
readConfigFile f = do
    contents <- readFile f
    let config = do
        c <- readstring emptyCP contents
        database <- get c "DEFAULT" "database"
        return Config { confDB = database }

    case config of
        Left cperr  -> fail $ show cperr
        Right c     -> return c

--
-- OPTION PROCESSING CODE
--

type FilterFunc = (ConfirmInfo -> Bool)
type ReportFunc = ([ConfirmInfo] -> Html)

data Options = Options {
    optFilter :: [FilterFunc],
    optReport :: ReportFunc }

type OptAction = (Options -> IO Options)

defaultOptions :: Options
defaultOptions = Options {
    optFilter = [F.byNone],
    optReport = reportAll }

mkFilterAction opt f =
    opt { optFilter = optFilter opt ++ [f] }

opts :: [OptDescr OptAction]
opts = [
    Option [] ["filter-band"]           (ReqArg (\arg opt -> return $ mkFilterAction opt (F.byBand (read arg :: ADIF.Band))) "BAND")
           "filter by band",
    Option [] ["filter-call"]           (ReqArg (\arg opt -> return $ mkFilterAction opt (F.byCall $ uppercase arg)) "CALL")
           "filter QSOs by call sign",
    Option [] ["filter-confirmed"]      (NoArg (\opt -> return $ mkFilterAction opt (F.byConfirmed True)))
           "filter confirmed QSOs",
    Option [] ["filter-digital"]        (NoArg (\opt -> return $ mkFilterAction opt F.byDigital))
           "show only digital QSOs",
    Option [] ["filter-dxcc"]           (ReqArg (\arg opt -> return $ mkFilterAction opt (F.byDXCC (read arg :: Integer))) "DXCC")
           "filter by DXCC entity",
    Option [] ["filter-image"]          (NoArg (\opt -> return $ mkFilterAction opt F.byImage))
           "show only image QSOs",
    Option [] ["filter-itu"]            (ReqArg (\arg opt -> return $ mkFilterAction opt (F.byITU (read arg :: Integer))) "ITU")
           "filter by ITU zone",
    Option [] ["filter-mode"]           (ReqArg (\arg opt -> return $ mkFilterAction opt (F.byMode (read arg :: ADIF.Mode))) "MODE")
           "filter by mode",
    Option [] ["filter-phone"]          (NoArg (\opt -> return $ mkFilterAction opt F.byPhone))
           "show only phone QSOs",
    Option [] ["filter-unconfirmed"]    (NoArg (\opt -> return $ mkFilterAction opt (F.byConfirmed False)))
           "filter unconfirmed QSOs",
    Option [] ["filter-waz"]            (ReqArg (\arg opt -> return $ mkFilterAction opt (F.byWAZ (read arg :: Integer))) "WAZ")
           "filter by WAZ zone",

    Option [] ["challenge"]             (NoArg (\opt -> return (mkFilterAction opt (F.byConfirmed True)) { optReport = reportChallenge }))
           "display DXCC challenge progress",
    Option [] ["dxcc"]                  (NoArg (\opt -> return (mkFilterAction opt (F.byConfirmed True)) { optReport = reportDXCC }))
           "display DXCC progress",
    Option [] ["vucc"]                  (NoArg (\opt -> return (mkFilterAction opt (F.byConfirmed True)) { optReport = reportVUCC }))
           "display VUCC progress",

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
    let html = showHtml $ (optReport cmdline) ci'

    -- (5) Display.
    putStrLn html
