{-# LANGUAGE MultiWayIf #-}

module ToolLib.Config ( BandMap,
                        Config(..),
                        readConfig )
 where

import Control.Applicative((<$>))
import Data.ConfigFile(emptyCP, get, has_option, items, readstring)
import Data.List(isPrefixOf)
import Data.List.Split(splitOn)
import Data.Maybe(mapMaybe)
import System.Directory(getHomeDirectory)
import Text.Read(readMaybe)

import qualified Slog.Formats.ADIF.Types as ADIF

type BandMap = [(ADIF.Band, String)]

data Config = Config {
    confDB        :: String,
    confPassword  :: String,
    confUsername  :: String,

    confQTHPass   :: String,
    confQTHUser   :: String,

    confQTHs      :: [String],
    confDefaultQTH:: String,

    confAntennas        :: [String],
    confAntennaMap      :: BandMap,
    confDefaultAntenna  :: String,

    confModeMap     :: BandMap,
    confDefaultMode :: String,

    confRadioModel  :: String,
    confRadioDev    :: String }

readConfig :: IO Config
readConfig = do
    homeDir <- getHomeDirectory
    contents <- readFile (homeDir ++ "/.slog")

    let config = do
        -- Load some default values into the record, then read the real config file and have its
        -- values take precedence.
        def <- readstring emptyCP "[Antennas]\nantennas = Unknown\n"
        c <- readstring def contents

        database <- get c "DEFAULT" "database"
        password <- get c "LOTW" "password"
        username <- get c "LOTW" "username"

        qthPass  <- get c "Lookup" "password"
        qthUser  <- get c "Lookup" "username"

        qths   <- getQTHs c
        defQTH <- defaultQTH c qths

        antennas     <- getAntennas c
        bandAntennas <- getAntennaMap c
        defAntenna   <- defaultAntenna c antennas

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

                        confAntennas        = antennas,
                        confAntennaMap      = bandAntennas,
                        confDefaultAntenna  = defAntenna,

                        confModeMap     = bandModes,
                        confDefaultMode = defMode,

                        confRadioModel = radio,
                        confRadioDev   = device }

    case config of
        Left cperr   -> fail $ show cperr
        Right c      -> return c
 where
    getQTHs conf = splitOn "," <$> get conf "QTHs" "qths"

    defaultQTH conf qths = do
        let givesDefault = has_option conf "QTHs" "default"
        if givesDefault then get conf "QTHs" "default" else return $ head qths

    getAntennas conf = do
        let givesAntennas = has_option conf "Antennas" "antennas"
        if givesAntennas then splitOn "," <$> get conf "Antennas" "antennas" else return []

    defaultAntenna conf antennas = do
        let givesDefault = has_option conf "Antennas" "default"

        if | givesDefault           -> get conf "Antennas" "default"
           | not $ null antennas    -> return $ head antennas
           | otherwise              -> return "Unknown"

    getAntennaMap conf = do
        antennas <- items conf "Antennas"
        return $ mapMaybe (\(name, ant) -> if | "default_" `isPrefixOf` name -> let name' = drop 8 name
                                                                                    band  = readMaybe name' :: Maybe ADIF.Band
                                                                                in maybe Nothing (\x -> Just (x, ant)) band
                                              | otherwise                    -> Nothing)
                          antennas

    defaultMode conf = do
        let givesDefault = has_option conf "Modes" "default"

        if | givesDefault -> get conf "Modes" "default"
           | otherwise    -> return "SSB"

    getModeMap conf = do
        modes <- items conf "Modes"
        return $ mapMaybe (\(name, mode) -> if | "default_" `isPrefixOf` name -> let name' = drop 8 name
                                                                                     band  = readMaybe name' :: Maybe ADIF.Band
                                                                                 in maybe Nothing (\x -> Just (x, mode)) band
                                               | otherwise                    -> Nothing)
                          modes
