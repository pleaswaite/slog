import Control.Monad(liftM)
import System.Console.GetOpt
import System.Environment(getArgs)
import System.Exit(ExitCode(..), exitWith)
import Text.XHtml.Strict(Html, showHtml)

import Slog.DB(DBResult, getAllQSOs)
import qualified Slog.Formats.ADIF.Types as ADIF
import Slog.Utils(uppercase)

import ToolLib.Config

import qualified Filter as F
import Report(reportAll, reportChallenge, reportDXCC, reportVUCC)

--
-- OPTION PROCESSING CODE
--

type FilterFunc = (DBResult -> Bool)
type ReportFunc = ([DBResult] -> Html)

data Options = Options {
    optFilter :: [FilterFunc],
    optReport :: ReportFunc }

type OptAction = (Options -> IO Options)

defaultOptions :: Options
defaultOptions = Options {
    optFilter = [F.byNone],
    optReport = reportAll }

mkFilterAction :: Options -> FilterFunc -> Options
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
    Option [] ["filter-satellite"]      (NoArg (\opt -> return $ mkFilterAction opt F.bySatellite))
           "show only satellite QSOs",
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

processArgs :: Monad m => m ([Options -> m Options], t) -> m Options
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
    conf <- readConfig

    -- Get the on-disk location of the database.
    let fp = confDB conf

    -- Reporting is a multiple step process:
    -- (1) Get all QSOs.
    results <- liftM reverse $ getAllQSOs fp

    -- (2) Filter the results based on band, call, or whatever else was requested
    -- on the command line.
    let results' = foldl (flip filter) results (optFilter cmdline)

    -- (3) Convert to HTML based upon whatever header and body formatting was requested
    -- on the command line.  This is what makes it a report.
    let html = showHtml $ (optReport cmdline) results'

    -- (5) Display.
    putStrLn html
