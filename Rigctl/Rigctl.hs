module Rigctl where

import Control.Monad.Reader
import Data.List(isPrefixOf)
import Data.Maybe(fromJust, isNothing)
import Network
import System.Cmd(system)
import System.IO
import System.IO.Utils(hGetLines)
import Text.Printf(hPrintf)

import Commands.Class(ser)
import qualified Commands.Ask as A
import qualified Commands.Tell as T
import Utils(stringToInteger)

type RigConn = ReaderT Rig IO
data Rig = Rig { socket :: Handle }

data RigctldError = NoError | RigError Integer

connect :: String -> Integer -> IO Rig
connect server port = do
    h <- connectTo server (PortNumber $ fromIntegral port)
    hSetBuffering h NoBuffering
    return (Rig h)

disconnect = hClose . socket

errorCode :: String -> RigctldError
errorCode s =
    case stringToInteger s of
        Just i    -> if i == 0 then NoError else RigError i
        Nothing   -> NoError

ask :: A.Command -> RigConn (Either RigctldError T.Command)
ask inCmd =
    if isNothing serialized then return $ Left (RigError 1)
    else do
        wr $ fromJust serialized
        response <- rd

        if length response == 1 && "RPRT " `isPrefixOf` (response !! 0)
        then return $ Left $ errorCode (drop 5 $ response !! 0)
        else return $ Left NoError
 where
    serialized = ser inCmd

tell :: T.Command -> RigConn RigctldError
tell inCmd =
    if isNothing serialized then return $ RigError 1
    else do
        wr $ fromJust serialized
        response <- rd

        let firstLine = response !! 0

        if "RPRT " `isPrefixOf` firstLine
        then return $ errorCode (drop 5 firstLine)
        else return $ RigError 1
 where
    serialized = ser inCmd

rd :: RigConn [String]
rd = do
    h <- asks socket
    liftIO $ hGetLines h

wr :: String -> RigConn ()
wr s = do
    h <- asks socket
    liftIO $ hPrintf h "%s\n" s

runRigctld :: String -> String -> IO ()
runRigctld model device = do
    rc <- system $ "rigctld -m " ++ model ++ " -r " ++ device
    return ()
