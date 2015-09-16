{-# LANGUAGE MultiWayIf #-}

module ToolLib.Config ( Config(..),
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

data Config = Config {
    confDB        :: String,
    confPassword  :: String,
    confQTH       :: String,
    confUsername  :: String,

    confQTHPass   :: String,
    confQTHUser   :: String,

    confAntennas        :: [String],
    confAntennaMap      :: [(ADIF.Band, String)],
    confDefaultAntenna  :: String,

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
        qth      <- get c "DEFAULT" "qth"
        username <- get c "LOTW" "username"

        qthPass  <- get c "Lookup" "password"
        qthUser  <- get c "Lookup" "username"

        antennas     <- getAntennas c
        bandAntennas <- getAntennaMap c
        defAntenna   <- defaultAntenna c antennas

        radio    <- get c "Radio" "model"
        device   <- get c "Radio" "device"

        return Config { confDB         = database,
                        confPassword   = password,
                        confQTH        = qth,
                        confUsername   = username,

                        confQTHPass    = qthPass,
                        confQTHUser    = qthUser,

                        confAntennas        = antennas,
                        confAntennaMap      = bandAntennas,
                        confDefaultAntenna  = defAntenna,

                        confRadioModel = radio,
                        confRadioDev   = device }

    case config of
        Left cperr   -> fail $ show cperr
        Right c      -> return c
 where
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
