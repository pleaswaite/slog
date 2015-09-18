{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module contains the top-level interface for communicating with a radio via
-- the rigctld program.  While this module contains a function for starting rigctld, it
-- is assumed in the Slog utilities that the user will have started it up before ever
-- running any of those utilities.
module Slog.Rigctl.Rigctl(RigctldError(..),
                          ask,
                          connect,
                          disconnect,
                          tell,
                          isRigctldRunning,
                          runRigctld)
 where

import Control.Exception(IOException, catch)
import Control.Monad.Reader hiding(ask)
import Data.List(isInfixOf)
import Data.Maybe(fromJust, isNothing)
import Network
import System.Process(readProcess, system)
import System.IO

import           Slog.Rigctl.Commands.Class(ser)
import qualified Slog.Rigctl.Commands.Ask as A
import qualified Slog.Rigctl.Commands.Tell as T

-- | The result of talking with a radio via rigctld is either 'NoError', or some error code
-- wrapped in a 'RigError'.  The exact meaning of these error codes is not yet exposed.
data RigctldError = NoError | RigError Integer

-- | Attempt to connect to the rigctld process listening on the hostname 'String'
-- and port number 'Integer'.
connect :: String -> Integer -> IO Handle
connect server port = do
    h <- connectTo server (PortNumber $ fromIntegral port)
    hSetBuffering h LineBuffering
    return h

-- | Disconnect from the rigctld process previously connected to with the 'connect' function.
disconnect :: Handle -> IO ()
disconnect = hClose

-- | Given a 'Slog.Rigctl.Commands.Ask.Command', ask the radio for a piece of information.  The result is either an
-- error code or the matching 'Slog.Rigctl.Commands.Tell.Command' with information filled in.  Note that not every
-- 'Slog.Rigctl.Commands.Ask.Command' has an equivalent 'Slog.Rigctl.Commands.Tell.Command'.
--
-- This method must be run in the 'RigConn' monad, which requires use of the 'runReaderT'
-- function.
ask :: A.Command -> IO (Either RigctldError T.Command)
ask inCmd =
    if isNothing serialized then return $ Left (RigError 1)
    else do
        result <- readProcess "rigctl"
                              ["-m", "2", fromJust serialized]
                              ""
        if | result == ""                    -> return $ Left $ RigError 1
           | "not found!" `isInfixOf` result -> return $ Left $ RigError 1
           | otherwise                       -> return $ doAsk inCmd (lines result)
 where
    serialized = ser inCmd
    doAsk cmd s = maybe (Left $ RigError 1) Right (T.toTell cmd s)

-- | Given a 'Slog.Rigctl.Commands.Tell.Command', tell the radio to either apply a particular setting or run some function.
-- The result is potentially an error code.
--
-- This method must be run in the 'RigConn' monad, which requires use of the 'runReaderT'
-- function.
tell :: T.Command -> IO RigctldError
tell inCmd =
    if isNothing serialized then return $ RigError 1
    else do
        result <- readProcess "rigctl"
                              ["-m", "2", fromJust serialized]
                              ""
        if | result == ""   -> return NoError
           | otherwise      -> return $ RigError 1
 where
    serialized = ser inCmd

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
