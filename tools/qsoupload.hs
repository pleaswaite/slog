{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Applicative((<$>))
import Control.Exception(IOException, catch, finally)
import Data.List(intercalate)
import System.Directory(getTemporaryDirectory, removeFile)
import System.IO

import Slog.DB(getUnsentQSOs, markQSOsAsSent)
import Slog.Formats.ADIF.Writer(renderRecord)
import Slog.LOTW(sign)
import Slog.QSO(qsoToADIF)

import ToolLib.Config

type SignFuncTy = FilePath -> IO FilePath

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
    Config{..} <- readConfig

    -- Get the on-disk location of the database.
    let fp = confDB

    -- Get all the un-uploaded QSOs and their matching ID numbers, and convert them to
    -- a string of ADIF data.  We'll save the IDs for marking in the database later.
    (ids, qsos) <- unzip <$> map (\(a, b, _) -> (a, b)) <$> getUnsentQSOs fp
    let adifs = intercalate "\r\n" $ map (renderRecord . qsoToADIF) qsos

    -- Then write out the temporary file, sign it, and upload it.
    withTempFile "new.adi" (writeAndUpload adifs (sign confQTH))

    -- Finally, update the database to reflect everything that's been uploaded.
    markQSOsAsSent fp ids
