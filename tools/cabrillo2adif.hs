import Control.Monad(unless)
import System.Environment(getArgs)
import System.Exit(exitFailure)
import System.IO(hPrint, stderr)

import Slog.Formats.ADIF.Writer(renderFile)
import Slog.Formats.Cabrillo.ADIF(cabrilloToADIF)
import Slog.Formats.Cabrillo.Parser(parseString)

main :: IO ()
main = do
    args <- getArgs
    unless (length args == 1) $ do
        hPrint stderr "usage: cabrillo2adif contestname < cabfile"
        exitFailure

    input <- getContents
    case parseString (head args) input of
        Left err    -> hPrint stderr err >> exitFailure
        Right str   -> case cabrilloToADIF str of
                           Just adif -> putStrLn $ renderFile adif
                           Nothing   -> hPrint stderr "Error converting Cabrillo to ADIF" >> exitFailure
