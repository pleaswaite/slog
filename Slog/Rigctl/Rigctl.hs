module Slog.Rigctl.Rigctl(RigConn,
                          RigctldError(..),
                          connect,
                          disconnect,
                          ask,
                          tell,
                          runRigctld)
 where

import Control.Monad.Reader hiding(ask)
import Data.List(isPrefixOf)
import Data.Maybe(fromJust, isNothing)
import Network
import System.Cmd(system)
import System.IO
import System.IO.Utils(hGetLines)
import Text.Printf(hPrintf)

import Slog.Rigctl.Commands.Class(ser)
import qualified Slog.Rigctl.Commands.Ask as A
import qualified Slog.Rigctl.Commands.Tell as T
import Slog.Utils(stringToInteger)

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

        case response of
            []      -> return $ Left $ RigError 1
            [l1]    -> if "RPRT " `isPrefixOf` l1 then return $ Left $ errorCode (drop 5 l1)
                       else return $ doAsk inCmd response
            _       -> return $ doAsk inCmd response
 where
    serialized = ser inCmd
    doAsk cmd s = maybe (Left $ RigError 1) Right (T.toTell cmd s)

tell :: T.Command -> RigConn (Maybe RigctldError)
tell inCmd =
    if isNothing serialized then return $ Just $ RigError 1
    else do
        wr $ fromJust serialized
        response <- rd

        case response of
            [l1]    -> if "RPRT " `isPrefixOf` l1 then return $ Just $ errorCode (drop 5 l1)
                       else return $ Nothing
            _       -> return $ Just $ RigError 1
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
