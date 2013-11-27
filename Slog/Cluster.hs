-- | A module for dealing with the DX Cluster.
module Slog.Cluster( Spot(..),
                     latestSpots,
                     parseString )
 where

import Control.Exception(IOException, try)
import Control.Monad.State
import Data.List(findIndex)
import Data.List.Split(splitOn)
import Data.Maybe(fromJust, mapMaybe)
import Network.HTTP
import Slog.Utils(stringToDouble)
import qualified Slog.Formats.ADIF.Types as ADIF

-- | This record contains all the information about a single DX cluster spot.
data Spot = Spot { spCall :: String,                  -- ^ the DX's call sign
                   spFreq :: Double,                  -- ^ the frequency of the QSO
                   spSpotter :: String,               -- ^ the call sign of the spotter
                   spComment :: Maybe String,         -- ^ any comment from the spotter
                   spDate :: ADIF.Date,               -- ^ the date of the QSO
                   spTime :: ADIF.Time,               -- ^ the time of the QSO
                   spLOTW :: Bool,                    -- ^ is the DX an LOTW user?
                   spEQSL :: Bool,                    -- ^ is the DX an eSQL user?
                   spContinent :: ADIF.Continent      -- ^ what continent is the DX on?
 }
 deriving (Eq, Show)

--
-- STRING PARSING CODE
--

makeSpot :: [String] -> Maybe Spot
makeSpot [call, freq, spotter, comment, datetime, lotw, eqsl, continent] =
    Just $ Spot { spCall = call,
                  spFreq = fromJust $ stringToDouble freq,
                  spSpotter = spotter,
                  spComment = asMaybe comment,
                  spDate = date,
                  spTime = time,
                  spLOTW = lotw == "L",
                  spEQSL = eqsl == "E",
                  spContinent = read continent :: ADIF.Continent }
 where
    asMaybe "" = Nothing
    asMaybe x  = Just x

    time = head $ words datetime
    date = last $ words datetime
makeSpot _ = Nothing

parseOne :: String -> Maybe Spot
parseOne s = makeSpot $ splitOn "^" s

-- | Given a string of text from the DX cluster, convert it into a list of Spot
-- records.  Each line is an individual spot, and any errors in processing a line
-- will result in that line being throw out from the resulting list.
parseString :: String -> [Spot]
parseString s = mapMaybe parseOne (lines s)

--
-- GENERATING AN INFINITE LIST OF SPOTS
--

-- Download the last 25 DX cluster spots from the web.  On error, just return an
-- empty string.  This is not critical stuff.
grabSpots :: IO String
grabSpots = do
    let url = "http://hamqth.com/dxc_csv.php?limit=25"
    result <- try ((simpleHTTP $ getRequest url) >>= getResponseBody) :: IO (Either IOException String)
    return $ either (\_ -> "") (id) result

-- Given a list of new spots and the latest spot we previously grabbed, return all the spots that
-- are newer.
removeOldSpots :: [Spot] -> Spot -> [Spot]
removeOldSpots newSpots latestOld =
    maybe newSpots
          (flip take newSpots)
          (findIndex (== latestOld) newSpots)

-- | Return a list of the DX cluster spots that have appeared since the last time
-- this function was called.
latestSpots :: StateT (Maybe Spot) IO [Spot]
latestSpots = do
    spots <- lift grabSpots
    let spots' = parseString spots

    -- Grab the previous latest spot out of the global state, then figure out what
    -- spots are new since then.  Nothing means we didn't have a previous latest
    -- spot, in which case everything is new.
    oldSpot <- get
    let newSpots = maybe spots'
                         (removeOldSpots spots')
                         oldSpot

    -- And then the first spot is now the newest, so stash that in the state for
    -- next time.
    put $ Just $ head newSpots
    return newSpots
