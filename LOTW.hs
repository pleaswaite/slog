module LOTW(sign) where

import System.Cmd(system)
import System.Directory(doesFileExist)
import System.Exit(ExitCode(..))

-- Given a file path and a QTH, sign the file.  Note that you must have set
-- everything up with tqsl first.
--
-- NOTE:  tqsl is annoying and will start up graphically, especially if there
-- are any errors.  It's recommended that the QTH be verified by running this
-- command.  We can at least verify the file path here, though.
sign :: String -> FilePath -> IO ()
sign qth file = do
    exists <- doesFileExist file

    if exists then do
        -- I'd add a -x here to tell tqsl to quit after signing, but that
        -- apparently makes it exit with 255 instead of 0.  Oh well.
        rc <- system $ "tqsl -d -l " ++ qth ++ " " ++ file
        case rc of
            ExitSuccess     -> return ()
            ExitFailure _   -> fail "Signing failed."
    else
        fail "File does not exist."
