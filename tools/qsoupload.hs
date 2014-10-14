{-# LANGUAGE ScopedTypeVariables #-}
import Control.Exception(IOException, catch, finally)
import Data.List(intersperse)
import Prelude hiding(catch)
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
    tempdir <- catch (getTemporaryDirectory) (\(_ :: IOException) -> return ".")
    (tempfile, temph) <- openTempFile tempdir pattern
    finally (func tempfile temph)
            (do removeFile tempfile)

writeAndUpload :: String -> SignFuncTy -> UploadFuncTy -> FilePath -> Handle -> IO ()
writeAndUpload adifs signFunc uploadFunc tempname temph = do
    hPutStrLn temph adifs
    hClose temph
    signedFile <- signFunc tempname
    finally (uploadFunc signedFile)
            (do removeFile signedFile)

main :: IO ()
main = do
    -- Read in the config file.
    conf <- readConfig

    -- Get the on-disk location of the database.
    let fp = confDB conf

    -- Get all the un-uploaded QSOs and convert them to a string of ADIF data.
    qsos <- getUnsentQSOs fp
    let adifs = concat $ intersperse "\r\n" $ map (renderRecord . qsoToADIF) qsos

    -- Then write out the temporary file, sign it, and upload it.
    withTempFile "new.adi" (writeAndUpload adifs (sign $ confQTH conf) upload)

    -- Finally, update the database to reflect everything that's been uploaded.
    markQSOsAsSent fp qsos
