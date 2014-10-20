{-# OPTIONS_GHC -Wall #-}

import Control.Monad(liftM)
import Data.List((\\), nub, sort)
import Data.Maybe(mapMaybe)
import System.Console.GetOpt
import System.Environment(getArgs)

import qualified Slog.Formats.ADIF.Types as ADIF
import Slog.DB(getAllQSOs, second)
import Slog.DXCC(DXCC(dxccEntity), entityIDs, entityFromID)
import Slog.QSO(QSO(qDXCC), isConfirmed)

import ToolLib.Config

import qualified Filter as F

--
-- OPTION PROCESSING CODE
--

type FilterDXCCFunc = (DXCC -> Bool)
type FilterQSOFunc = (QSO -> Bool)

data Options = Options {
    optFilterDXCC    :: FilterDXCCFunc,
    optFilterQSO     :: [FilterQSOFunc],
    optConfirmedOnly :: Bool }

type OptAction = (Options -> IO Options)

defaultOptions :: Options
defaultOptions = Options {
    optFilterDXCC    = F.dxccByNone,
    optFilterQSO     = [F.qsoByNone],
    optConfirmedOnly = True }

mkFilterAction :: Options -> FilterQSOFunc -> Options
mkFilterAction opt f =
    opt { optFilterQSO = optFilterQSO opt ++ [f] }

opts :: [OptDescr OptAction]
opts = [
    Option ['a'] ["all"]         (NoArg (\opt -> return opt {optConfirmedOnly = False }))
           "filter out all worked entities, not just confirmed",
    Option [] ["filter-band"]    (ReqArg (\arg opt -> return $ mkFilterAction opt (F.qsoByBand (read arg :: ADIF.Band))) "BAND")
           "filter by band",
    Option [] ["filter-cont"]    (ReqArg (\arg opt -> return opt {optFilterDXCC = (F.dxccByContinent (read arg :: ADIF.Continent))}) "CONT")
           "show only results for a given continent"
 ]

handleOpts :: [String] -> IO ([OptAction], [String])
handleOpts argv =
    case getOpt RequireOrder opts argv of
        (o, n, [])   -> return (o, n)
        (_, _, errs) -> ioError (userError (concat errs ++ usageInfo header opts))
                        where header = "Usage: unworked [OPTIONS]"

processArgs :: Monad m => m ([Options -> m Options], t) -> m Options
processArgs argsFunc = do
    (actions, _) <- argsFunc
    foldl (>>=) (return defaultOptions) actions

--
-- THE MAIN PROGRAM
--

-- Given a list of QSOs, reduce it to a list of DXCC entity IDs that have not
-- been worked.
unworkedIDs :: [QSO] -> [Integer]
unworkedIDs qsos =
    entityIDs \\ (nub $ mapMaybe qDXCC qsos)

main :: IO ()
main = do
    -- Process command line arguments.
    cmdline <- processArgs (getArgs >>= handleOpts)

    -- Read in the config file.
    conf <- readConfig

    -- Get the on-disk location of the database.
    let fp = confDB conf

    -- Get all QSOs as (id, QSO, Confirmation) tuples.
    results <- liftM reverse $ getAllQSOs fp

    -- If we are reporting based on only confirmed QSOs, remove unconfirmed
    -- ones from the list.  Otherwise, use all the QSOs in the log.
    let qsos = map second $ if optConfirmedOnly cmdline then filter (\(_, _, c) -> isConfirmed c) results
                            else results

    -- Reduce the list of worked QSOs down to just those on the given band
    -- (or whatever) we want to consider.  Then, convert that into a list of
    -- DXCC entity IDs that have not been worked.
    let unworked = unworkedIDs $ foldl (flip filter) qsos (optFilterQSO cmdline)

    -- And then we need to convert the list of entity numbers into a list of
    -- entity objects.  This is roundabout because DXCC doesn't expose the
    -- real list.
    let unworkedEntities = mapMaybe entityFromID unworked

    -- And finally, reduce the list of unworked entities to just those
    -- on the given continent (if such a filter was requested).
    let unworkedEntities' = filter (optFilterDXCC cmdline) unworkedEntities

    mapM_ putStrLn $
          sort (map dxccEntity unworkedEntities')
