module LOTW(sign,
            download,
            upload)
 where

import System.Cmd(system)
import System.Directory(doesFileExist)
import System.Exit(ExitCode(..))
import System.FilePath(replaceExtension)
import System.Process(readProcessWithExitCode)

-- Download a list of verified, received QSLs from LOTW, starting with a
-- provided date.  This can then be used to mark them as received in the
-- database.  The resulting string is in ADIF format.
download :: String -> String -> String -> IO String
download date call password = do
    -- Argh, Network.HTTP does not support HTTPS so we have to be all convoluted
    -- here to get the results.  At least curl will spit the data out to stdout.
    (exitcode, stdout, stderr) <- readProcessWithExitCode "curl" ["https://p1k.arrl.org/lotwuser/lotwreport.adi?qso_query=1&qso_withown=yes&qso_qslsince=" ++ date ++ "&qso_owncall=" ++ call ++ "&login=" ++ call ++ "&password=" ++ password] ""
    case exitcode of
        ExitSuccess     -> return stdout
        ExitFailure _   -> fail $ "Fetching from LOTW failed: " ++ stderr

-- Given a file path and a QTH, sign the file.  Note that you must have set
-- everything up with tqsl first.
--
-- NOTE:  tqsl is annoying and will start up graphically, especially if there
-- are any errors.  It's recommended that the QTH be verified by running this
-- command.  We can at least verify the file path here, though.
sign :: String -> FilePath -> IO FilePath
sign qth file = do
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

-- Upload a signed ADIF file to LOTW.
upload :: FilePath -> IO ()
upload file = do
    (exitcode, stdout, stderr) <- readProcessWithExitCode "curl" ["-F", "upfile=@" ++ file, "https://p1k.arrl.org/lotw/upload"] ""
    case exitcode of
        ExitSuccess     -> return ()
        ExitFailure _   -> fail $ "Uploading to LOTW failed: " ++ stderr
