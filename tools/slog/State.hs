{-# OPTIONS_GHC -Wall #-}

-- | The 'State' module provides data types and functions for manipulating global
-- program state.  Unfortunately we need to have one of these, since this is a
-- graphical program and thus inherently has state.

module State where

import Control.Applicative((<$>))
import Control.Monad(void)
import Data.IORef(IORef, atomicWriteIORef, newIORef, readIORef)
import Graphics.UI.Gtk

import ToolLib.Config(Config)

import Contest
import Types

-- | A record containing all the important global program state.  Its members are useful
-- in lots of different places, so having them all wrapped up in one single record is
-- much more convenient.
data PState = PState {
    psConf :: Config,                        -- ^ the 'Config' data read from disk

    psWidgets :: Widgets,                    -- ^ 'Widgets' that appear on the main screen
    psCWidgets :: CWidgets,                  -- ^ 'CWidgets' that appear on the contest config dialog

    psPrevStore :: ListStore DisplayRow,     -- ^ the 'ListStore' of previous contacts with a looked up call
    psAllStore :: ListStore DisplayRow,      -- ^ the 'ListStore' of all contacts in the database

    psContestMode :: Bool,                   -- ^ are we in contest mode?
    psContestVal :: Contest                  -- ^ the 'Contest' record for generating exchange data
 }

-- | Apply a modification function to an existing 'PState' record and store that result
-- as the new state.
modifyState :: IORef PState -> (PState -> PState) -> IO ()
modifyState state fn = do
    oldVal <- readState state id
    atomicWriteIORef state (fn oldVal)

-- | Create a new global state by wrapping up a given 'PState' record.
newState :: PState -> IO (IORef PState)
newState = newIORef

-- | Extract a single item out of an existing 'PState' record via an accessor function.
readState :: IORef PState -> (PState -> a) -> IO a
readState state fn = fn <$> readIORef state

-- | Given an existing 'PState' and an accessor function for pulling out a single GTK
-- widget, perform some function on that widget and return the result.
withStateWidget :: IORef PState -> (Widgets -> a) -> (a -> IO b) -> IO b
withStateWidget state getWidgetFn doSomethingFn = do
    widget <- readState state (getWidgetFn . psWidgets)
    doSomethingFn widget

-- | Same as 'withStateWidget', but ignore the result.
withStateWidget_ :: IORef PState -> (Widgets -> a) -> (a -> IO b) -> IO ()
withStateWidget_ state getWidgetFn doSomethingFn = void $ withStateWidget state getWidgetFn doSomethingFn
