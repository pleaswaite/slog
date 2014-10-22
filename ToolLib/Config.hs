module ToolLib.Config ( Config(..),
                        readConfig )
 where

import Control.Applicative((<$>))
import Data.ConfigFile(emptyCP, get, items, readstring)
import Data.List.Split(splitOn)
import System.Directory(getHomeDirectory)

data Config = Config {
    confDB        :: String,
    confPassword  :: String,
    confQTH       :: String,
    confUsername  :: String,

    confQTHPass   :: String,
    confQTHUser   :: String,

    confAntennas  :: [String] }

readConfig :: IO Config
readConfig = do
    homeDir <- getHomeDirectory
    contents <- readFile (homeDir ++ "/.slog")

    let config = do
        c <- readstring emptyCP contents

        database <- get c "DEFAULT" "database"
        password <- get c "LOTW" "password"
        qth      <- get c "DEFAULT" "qth"
        username <- get c "LOTW" "username"

        qthPass  <- get c "Lookup" "password"
        qthUser  <- get c "Lookup" "username"

        antennas <- splitOn "," <$> get c "Antennas" "antennas"

        return Config { confDB         = database,
                        confPassword   = password,
                        confQTH        = qth,
                        confUsername   = username,

                        confQTHPass    = qthPass,
                        confQTHUser    = qthUser,

                        confAntennas   = antennas }

    case config of
        Left cperr   -> fail $ show cperr
        Right c      -> return c
