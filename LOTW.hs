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
sign :: FilePath -> String -> IO ()
sign file qth = do
    exists <- doesFileExist file

    if exists then do
        rc <- system $ "tqsl -d -l " ++ qth ++ " " ++ file ++ " -x"
        case rc of
            ExitSuccess     -> return ()
            ExitFailure _   -> fail "Signing failed."
    else
        fail "File does not exist."
