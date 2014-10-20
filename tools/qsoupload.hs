{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Applicative((<$>))
import Control.Exception(IOException, catch, finally)
import Data.List(intersperse)
import System.Directory(getTemporaryDirectory, removeFile)
import System.IO

import Slog.DB(getUnsentQSOs, markQSOsAsSent)
import Slog.Formats.ADIF.Writer(renderRecord)
import Slog.LOTW(sign, upload)
import Slog.QSO(qsoToADIF)

import ToolLib.Config

type SignFuncTy = FilePath -> IO FilePath
type UploadFuncTy = FilePath -> IO ()

withTempFile :: String -> (FilePath -> Handle -> IO a) -> IO a
withTempFile pattern func = do
    tempdir <- catch getTemporaryDirectory (\(_ :: IOException) -> return ".")
    (tempfile, temph) <- openTempFile tempdir pattern
    finally (func tempfile temph)
            (removeFile tempfile)

writeAndUpload :: String -> SignFuncTy -> UploadFuncTy -> FilePath -> Handle -> IO ()
writeAndUpload adifs signFunc uploadFunc tempname temph = do
    hPutStrLn temph adifs
    hClose temph
    signedFile <- signFunc tempname
    finally (uploadFunc signedFile)
            (removeFile signedFile)

main :: IO ()
main = do
    -- Read in the config file.
    conf <- readConfig

    -- Get the on-disk location of the database.
    let fp = confDB conf

    -- Get all the un-uploaded QSOs and their matching ID numbers, and convert them to
    -- a string of ADIF data.  We'll save the IDs for marking in the database later.
    (ids, qsos) <- unzip <$> map (\(a, b, _) -> (a, b)) <$> getUnsentQSOs fp
    let adifs = concat $ intersperse "\r\n" $ map (renderRecord . qsoToADIF) qsos

    -- Then write out the temporary file, sign it, and upload it.
    withTempFile "new.adi" (writeAndUpload adifs (sign $ confQTH conf) upload)

    -- Finally, update the database to reflect everything that's been uploaded.
    markQSOsAsSent fp ids
