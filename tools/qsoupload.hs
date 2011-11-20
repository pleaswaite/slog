import Control.Exception(finally)
import Data.ConfigFile
import List(intersperse)
import System.Console.GetOpt
import System.Directory(getTemporaryDirectory, removeFile)
import System.Environment(getArgs)
import System.IO
import System.IO.Error(catch)

import DB(connect, getUnsentQSOs, markQSOsAsSent)
import Formats.ADIF.Writer(renderRecord)
import LOTW(sign, upload)
import QSO(qsoToADIF)

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

data Options = Options {
    optConfigFile :: String }
type OptAction = (Options -> IO Options)

defaultOptions :: Options
defaultOptions = Options {
    optConfigFile = "slog.conf" }

opts :: [OptDescr OptAction]
opts = [
    Option [] ["config"]        (ReqArg (\arg opt -> return opt { optConfigFile = arg }) "CONFIG")
           "the location of the config file"
 ]

handleOpts :: [String] -> IO ([OptAction], [String])
handleOpts argv =
    case getOpt RequireOrder opts argv of
        (o, n, [])      -> return (o, n)
        (_, _, errs)    -> ioError (userError (concat errs ++ usageInfo header opts))
                           where header = "Usage: qsoupload [OPTIONS] file"

--
-- THE MAIN PROGRAM
--

withTempFile :: String -> (FilePath -> Handle -> IO a) -> IO a
withTempFile pattern func = do
    tempdir <- catch (getTemporaryDirectory) (\_ -> return ".")
    (tempfile, temph) <- openTempFile tempdir pattern
    finally (func tempfile temph)
            (do removeFile tempfile)

writeAndUpload :: String -> (FilePath -> IO FilePath) -> (FilePath -> IO ()) -> FilePath -> Handle -> IO ()
writeAndUpload adifs signFunc uploadFunc tempname temph = do
    hPutStrLn temph adifs
    hClose temph
    signedFile <- signFunc tempname
    finally (uploadFunc signedFile)
            (do removeFile signedFile)

main :: IO ()
main = do
    -- Process command line arguments.
    (actions, _) <- getArgs >>= handleOpts
    cmdline <- foldl (>>=) (return defaultOptions) actions

    -- Read in the config file.
    conf <- readConfigFile (optConfigFile cmdline)

    -- Open the database.  We do not have to close the database since that happens
    -- automatically.
    dbh <- connect $ confDB conf

    -- Get all the un-uploaded QSOs and convert them to a string of ADIF data.
    qsos <- getUnsentQSOs dbh
    let adifs = concat $ intersperse "\r\n" $ map (renderRecord . qsoToADIF) qsos

    -- Then write out the temporary file, sign it, and upload it.
    withTempFile "new.adi" (writeAndUpload adifs (sign $ confQTH conf) upload)

    -- Finally, update the database to reflect everything that's been uploaded.
    markQSOsAsSent dbh qsos
