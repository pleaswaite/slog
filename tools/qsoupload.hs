{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Applicative((<$>))
import Control.Exception(IOException, catch, finally)
import Data.List(intercalate)
import System.Console.GetOpt
import System.Directory(getTemporaryDirectory, removeFile)
import System.Environment(getArgs)
import System.IO

import Slog.DB(getUnsentQSOsQ, initDB, markQSOsAsSent)
import Slog.Formats.ADIF.Writer(renderRecord)
import Slog.LOTW(sign)
import Slog.QSO(qsoToADIF)

import ToolLib.Config

type SignFuncTy = FilePath -> IO FilePath

--
-- OPTION PROCESSING CODE
--
data Options = Options {
    optQTH :: String
 }

type OptAction = (Options -> IO Options)

defaultOptions :: Config -> Options
defaultOptions Config{..} = Options {
    optQTH = confDefaultQTH
 }

opts :: [OptDescr OptAction]
opts = [
    Option "q" ["qth"]      (ReqArg (\arg opt -> return opt { optQTH = arg }) "QTH")
           "the QTH this contact was made from (defaults to ~/.slog default)"
 ]

handleOpts :: [String] -> IO ([OptAction], [String])
handleOpts argv =
    case getOpt RequireOrder opts argv of
        (o, n, [])      -> return (o, n)
        (_, _, errs)    -> ioError (userError (concat errs ++ usageInfo header opts))
                           where header = "Usage: qsoupload [opts]"

processArgs :: Monad m => Config -> m ([Options -> m Options], t) -> m Options
processArgs conf argsFunc = do
    (actions, _) <- argsFunc
    foldl (>>=) (return $ defaultOptions conf) actions

--
-- THE MAIN PROGRAM
--

withTempFile :: String -> (FilePath -> Handle -> IO a) -> IO a
withTempFile pattern func = do
    tempdir <- catch getTemporaryDirectory (\(_ :: IOException) -> return ".")
    (tempfile, temph) <- openTempFile tempdir pattern
    finally (func tempfile temph)
            (removeFile tempfile)

writeAndUpload :: String -> SignFuncTy -> FilePath -> Handle -> IO ()
writeAndUpload adifs signFunc tempname temph = do
    hPutStrLn temph adifs
    hClose temph
    signedFile <- signFunc tempname
    removeFile signedFile

main :: IO ()
main = do
    -- Read in the config file.
    conf@Config{..} <- readConfig

    -- Process command line arguments.  We do this after reading the config file here so we can
    -- use the QTH out of the config file as the default, and then use whatever may have been
    -- given as an optional command line argument as an override.
    cmdline <- processArgs conf (getArgs >>= handleOpts)

    -- Get the on-disk location of the database.
    let fp = confDB
    initDB confDB

    -- Get all the un-uploaded QSOs and their matching ID numbers, and convert them to
    -- a string of ADIF data.  We'll save the IDs for marking in the database later.
    (ids, qsos) <- unzip <$> map (\(a, b, _) -> (a, b)) <$> getUnsentQSOsQ fp (optQTH cmdline)
    let adifs = intercalate "\r\n" $ map (renderRecord . qsoToADIF) qsos

    -- Then write out the temporary file, sign it, and upload it.
    withTempFile "new.adi" (writeAndUpload adifs (sign $ optQTH cmdline))

    -- Finally, update the database to reflect everything that's been uploaded.
    markQSOsAsSent fp ids
