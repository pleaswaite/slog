{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module contains the top-level interface for communicating with a radio via
-- the rigctld program.  While this module contains a function for starting rigctld, it
-- is assumed in the Slog utilities that the user will have started it up before ever
-- running any of those utilities.
module Slog.Rigctl.Rigctl(RigConn,
                          RigctldError(..),
                          connect,
                          disconnect,
                          ask,
                          tell,
                          isRigctldRunning,
                          runRigctld)
 where

import Control.Exception(IOException, catch)
import Control.Monad(void)
import Control.Monad.Reader hiding(ask)
import Data.List(isPrefixOf)
import Data.Maybe(fromJust, isNothing)
import Network
import Prelude hiding(catch)
import System.Cmd(system)
import System.IO
import System.IO.Utils(hGetLines)
import Text.Printf(hPrintf)

import Slog.Rigctl.Commands.Class(ser)
import qualified Slog.Rigctl.Commands.Ask as A
import qualified Slog.Rigctl.Commands.Tell as T
import Slog.Utils(stringToInteger)

-- | A 'RigConn' is a monad transformer that takes place in the 'IO' monad.  Running
-- 'connect' will give you an initial state, which can then be passed to 'runReaderT'
-- to run the monad and alter this state with either a call to 'ask' or 'tell'.
--
-- This is really not very clear.  See buildArgList in qsoadd.hs for an example.
type RigConn = ReaderT Rig IO
data Rig = Rig { socket :: Handle }

-- | The result of talking with a radio via rigctld is either 'NoError', or some error code
-- wrapped in a 'RigError'.  The exact meaning of these error codes is not yet exposed.
data RigctldError = NoError | RigError Integer

-- | Attempt to connect to the rigctld process listening on the hostname 'String'
-- and port number 'Integer'.
connect :: String -> Integer -> IO Rig
connect server port = do
    h <- connectTo server (PortNumber $ fromIntegral port)
    hSetBuffering h NoBuffering
    return (Rig h)

-- | Disconnect from the rigctld process previously connected to with the 'connect' function.
disconnect :: Rig -> IO ()
disconnect = hClose . socket

errorCode :: String -> RigctldError
errorCode s | Just i <- stringToInteger s = if i == 0 then NoError else RigError i
            | otherwise                   = NoError

-- | Given a 'Slog.Rigctl.Commands.Ask.Command', ask the radio for a piece of information.  The result is either an
-- error code or the matching 'Slog.Rigctl.Commands.Tell.Command' with information filled in.  Note that not every
-- 'Slog.Rigctl.Commands.Ask.Command' has an equivalent 'Slog.Rigctl.Commands.Tell.Command'.
--
-- This method must be run in the 'RigConn' monad, which requires use of the 'runReaderT'
-- function.
ask :: A.Command -> RigConn (Either RigctldError T.Command)
ask inCmd =
    if isNothing serialized then return $ Left (RigError 1)
    else do
        wr $ fromJust serialized
        response <- rd

        case response of
            []      -> return $ Left $ RigError 1
            [li]    -> return $ if "RPRT " `isPrefixOf` l1 then Left $ errorCode (drop 5 l1)
                                else doAsk inCmd response
            _       -> return $ doAsk inCmd response
 where
    serialized = ser inCmd
    doAsk cmd s = maybe (Left $ RigError 1) Right (T.toTell cmd s)

-- | Given a 'Slog.Rigctl.Commands.Tell.Command', tell the radio to either apply a particular setting or run some function.
-- The result is potentially an error code.
--
-- This method must be run in the 'RigConn' monad, which requires use of the 'runReaderT'
-- function.
tell :: T.Command -> RigConn RigctldError
tell inCmd =
    if isNothing serialized then return $ RigError 1
    else do
        wr $ fromJust serialized
        response <- rd

        case response of
            [l1]    -> return $ if "RPRT " `isPrefixOf` l1 then errorCode (drop 5 l1)
                                else NoError
            _       -> return $ RigError 1
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

-- | Determine if the rigctld process is running by attempting to connect with it.  This
-- function assumes localhost and the default port number.
isRigctldRunning :: IO Bool
isRigctldRunning =
    catch tryConnect (\(_ :: IOException) -> return False)
 where
    tryConnect = connect "localhost" 4532 >>= disconnect >> return True

-- | Attempt to start rigctld.  The first 'String' argument is the model number of the
-- radio to connect to.  The second 'String' argument is the device node to use for the
-- connection.  You must have permissions to read and write from this device.
runRigctld :: String -> String -> IO ()
runRigctld model device =
    void $ system $ "rigctld -m " ++ model ++ " -r " ++ device
