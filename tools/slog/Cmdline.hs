{-# OPTIONS_GHC -Wall #-}

module Cmdline(Options(..),
               processArgs)
 where

import System.Console.GetOpt
import System.Environment(getArgs)

data Options = Options {
    optRigctl :: Bool
 }

type OptAction = (Options -> IO Options)

defaultOptions :: Options
defaultOptions = Options {
    optRigctl = True
 }

opts :: [OptDescr OptAction]
opts = [
    Option [] ["no-rigctl"] (NoArg (\opt -> return opt { optRigctl = False }))
           "disable rigctl support - may be needed to run digi programs at the same time"
 ]

handleOpts :: [String] -> IO ([OptAction], [String])
handleOpts argv =
    case getOpt RequireOrder opts argv of
        (o, n, [])      -> return (o, n)
        (_, _, errs)    -> ioError (userError (concat errs ++ usageInfo header opts))
                           where header = "Usage: qsoupload [opts]"

processArgs :: IO Options
processArgs = do
    (actions, _) <- getArgs >>= handleOpts
    foldl (>>=) (return defaultOptions) actions
