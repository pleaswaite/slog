module ToolLib.Config ( Config(..),
                        readConfig )
 where

import Data.ConfigFile(emptyCP, get, readstring)
import System.Directory(getHomeDirectory)

data Config = Config {
    confDB        :: String,
    confPassword  :: String,
    confQTH       :: String,
    confUsername  :: String,

    confQTHPass   :: String,
    confQTHUser   :: String }

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

        return Config { confDB         = database,
                        confPassword   = password,
                        confQTH        = qth,
                        confUsername   = username,

                        confQTHPass    = qthPass,
                        confQTHUser    = qthUser }

    case config of
        Left cperr   -> fail $ show cperr
        Right c      -> return c
