{-# LANGUAGE MultiWayIf #-}

module ToolLib.Config ( BandMap,
                        QTHMap,
                        Config(..),
                        QTH(..),
                        readConfig )
 where

import Control.Applicative((<$>))
import Data.ConfigFile(emptyCP, get, has_option, items, readstring, sections)
import Data.List(isPrefixOf)
import Data.List.Split(splitOn)
import Data.Maybe(mapMaybe)
import System.Directory(getHomeDirectory)
import Text.Read(readMaybe)

import qualified Slog.Formats.ADIF.Types as ADIF

-- | A mapping from a 'Band' to the antenna that should be used for it.  This is
-- used in the 'Config'  and 'QTH' records.
type BandMap = [(ADIF.Band, String)]

-- | A mapping from 'QTH' name to 'QTH' record.  This is used in the 'Config' record.
type QTHMap = [(String, QTH)]

-- | Top-level configuration for slog.
data Config = Config {
    confDB        :: String,                -- ^ filename of the QSO database
    confPassword  :: String,                -- ^ LOTW password
    confUsername  :: String,                -- ^ LOTW username

    confQTHPass   :: String,                -- ^ hamqth password
    confQTHUser   :: String,                -- ^ hamqth username

    confQTHs      :: QTHMap,                -- ^ a list of configured 'QTH' records
    confDefaultQTH:: String,                -- ^ which 'QTH' is the default?

    confModeMap     :: BandMap,             -- ^ a mapping from 'Band' to the mode that should
                                            -- be used on that band; this does not have to be
                                            -- comprehensive
    confDefaultMode :: String,              -- ^ the mode to use by default

    confRadioModel  :: String,              -- ^ the radio model, as understood by rigctld
    confRadioDev    :: String               -- ^ the radio device file
 }
 deriving(Show)

-- | A 'QTH' describes one operating location.  Multiple may be configured, if you operate
-- from multiple instances - home, a vacation place, a mountain top, etc.  This allows for
-- keeping track of many different locations within one single log file.  There should
-- always be at least one 'QTH' defined in the config file.
data QTH = QTH {
    qthName :: String,                      -- ^ the name of this location - this should match
                                            -- up with what is configured in LOTW
    qthCall :: String,                      -- ^ the call sign used at this 'QTH'

    qthAntennas         :: [String],        -- ^ a list of antennas used at this 'QTH'
    qthAntennaMap       :: BandMap,         -- ^ a mapping from 'Band' to the antenna that should
                                            -- be used on that band; this does not have to be
                                            -- comprehensive
    qthDefaultAntenna   :: String           -- the antenna to use by default
 }
 deriving(Show)

-- | Read the config file and return a 'Config' record.
readConfig :: IO Config
readConfig = do
    homeDir <- getHomeDirectory
    contents <- readFile (homeDir ++ "/.slog")

    let config = do
        c <- readstring emptyCP contents

        database <- get c "DEFAULT" "database"
        password <- get c "LOTW" "password"
        username <- get c "LOTW" "username"

        qthPass  <- get c "Lookup" "password"
        qthUser  <- get c "Lookup" "username"

        qths     <- readQTHs c
        defQTH   <- defaultQTH c qths

        bandModes   <- getModeMap c
        defMode     <- defaultMode c

        radio    <- get c "Radio" "model"
        device   <- get c "Radio" "device"

        return Config { confDB         = database,
                        confPassword   = password,
                        confUsername   = username,

                        confQTHPass    = qthPass,
                        confQTHUser    = qthUser,

                        confQTHs        = qths,
                        confDefaultQTH  = defQTH,

                        confModeMap     = bandModes,
                        confDefaultMode = defMode,

                        confRadioModel = radio,
                        confRadioDev   = device }

    case config of
        Left cperr   -> fail $ show cperr
        Right c      -> return c
 where
    readQTHs conf = mapM (readQTH conf) $
                         filter ("QTH" `isPrefixOf`) (sections conf)

    readQTH conf section = do
        name <- get conf section "name"
        call <- get conf section "call"

        antennas <- getQTHAntennas conf section
        defaultAntenna <- getDefaultAntenna conf section antennas
        antennaMap <- getAntennaMap conf section

        return (name, QTH { qthName = name,
                            qthCall = call,

                            qthAntennas = antennas,
                            qthAntennaMap = antennaMap,
                            qthDefaultAntenna = defaultAntenna })
     where
        getDefaultAntenna c qth antennas = do
            let givesDefault = has_option c qth "defaultAntenna"

            if | givesDefault           -> get c qth "defaultAntenna"
               | not $ null antennas    -> return $ head antennas
               | otherwise              -> return "Unknown"

        getQTHAntennas c qth = do
            let givesAntennas = has_option c qth "antennas"
            if givesAntennas then splitOn "," <$> get c qth "antennas" else return []

        getAntennaMap c qth = do
            antennas <- items c qth
            return $ mapMaybe readMapPair antennas

    defaultQTH conf qths = do
        let givesDefault = has_option conf "DEFAULT" "defaultQTH"
        if givesDefault then get conf "DEFAULT" "defaultQTH" else return $ (fst . head) qths

    defaultMode conf = do
        let givesDefault = has_option conf "Modes" "default"

        if | givesDefault -> get conf "Modes" "default"
           | otherwise    -> return "SSB"

    getModeMap conf = do
        modes <- items conf "Modes"
        return $ mapMaybe readMapPair modes

    readMapPair (key, val) = if | "default_" `isPrefixOf` key -> let key' = drop 8 key
                                                                     band = readMaybe key' :: Maybe ADIF.Band
                                                                 in maybe Nothing (\x -> Just (x, val)) band
                                | otherwise                   -> Nothing
