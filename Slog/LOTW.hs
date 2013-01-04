{-# LANGUAGE DoAndIfThenElse #-}
-- | The LOTW module interfaces with the ARRL's LOTW website and provides a way
-- to sign ADIF files, upload signed files to the website, and download verified
-- QSLs from the website.
--
-- Before using this module, you must first have a login on the LOTW website which is
-- a pretty complicated procedure that is beyond the scope of this documentation.  You
-- must also have curl and the tsql programs installed, as there are not satisfactory
-- Haskell interfaces to do everything in this module.
module Slog.LOTW(sign,
                 download,
                 upload)
 where

import Data.List(isInfixOf)
import System.Cmd(system)
import System.Directory(doesFileExist)
import System.Exit(ExitCode(..))
import System.FilePath(replaceExtension)
import System.Process(readProcessWithExitCode)

-- | 'download' takes a starting date (as a string in YYYY-MM-DD format - see
-- 'dashifyDate'), a LOTW login name, and an LOTW password.  Fetch a list of
-- verified, received QSLs from LOTW and return them as an ADIF-formatted string,
download :: String -> String -> String -> IO String
download date call password = do
    -- Argh, Network.HTTP does not support HTTPS so we have to be all convoluted
    -- here to get the results.  At least curl will spit the data out to stdout.
    (exitcode, stdout, stderr) <- readProcessWithExitCode "curl" ["https://p1k.arrl.org/lotwuser/lotwreport.adi?qso_query=1&qso_withown=yes&qso_qslsince=" ++ date ++ "&qso_owncall=" ++ call ++ "&login=" ++ call ++ "&password=" ++ password ++ "&qso_qsldetail=yes"] ""
    case exitcode of
        -- LOTW can send errors as an HTML chunk (not a full page), so that needs
        -- to be intercepted here.
        ExitSuccess     -> if "LoTW is Offline" `isInfixOf` stdout
                           then fail $ "Fetching from LOTW failed: LOTW is offline"
                           else return stdout
        ExitFailure _   -> fail $ "Fetching from LOTW failed: " ++ stderr

-- | Given a QTH (which must match the QTH provided when generating a trustedqsl
-- cert and LOTW login) and a file path, sign the file.  Note that you must have set
-- everything up with tqsl first.  Return the path of the signed file.
sign :: String -> FilePath -> IO FilePath
sign qth file = do
    -- NOTE:  tqsl is annoying and will start up graphically, especially if there
    -- are any errors.  It's recommended that the QTH be verified by running this
    -- command.  We can at least verify the file path here, though.
    exists <- doesFileExist file

    if exists then do
        -- I'd add a -x here to tell tqsl to quit after signing, but that
        -- apparently makes it exit with 255 instead of 0.  Oh well.
        rc <- system $ "tqsl -d -l " ++ qth ++ " " ++ file
        case rc of
            ExitSuccess     -> return $ replaceExtension file ".tq8"
            ExitFailure _   -> fail "Signing failed."
    else
        fail "File does not exist."

-- | Upload a signed ADIF file to LOTW.
upload :: FilePath -> IO ()
upload file = do
    (exitcode, _, stderr) <- readProcessWithExitCode "curl" ["-F", "upfile=@" ++ file, "https://p1k.arrl.org/lotw/upload"] ""
    case exitcode of
        ExitSuccess     -> return ()
        ExitFailure _   -> fail $ "Uploading to LOTW failed: " ++ stderr
