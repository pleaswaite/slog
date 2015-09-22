{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind -fno-warn-wrong-do-bind #-}
{-# LANGUAGE RecordWildCards #-}

module Dialogs.QTH(loadQTHWidgets,
                   runQTHDialog)
 where

import           Control.Applicative((<$>))
import           Control.Monad(void)
import           Data.IORef(IORef)
import qualified Data.Text as T
import           Graphics.UI.Gtk

import ToolLib.Config(Config(..), QTH(..))

import State
import Types
import UI(comboBoxSetActiveText)

loadQTHWidgets :: Builder -> IO QTHWidgets
loadQTHWidgets builder = do
    [qthDlg]    <- mapM (builderGetObject builder castToDialog)     ["configQTHDialog"]
    [qthCombo]  <- mapM (builderGetObject builder castToComboBox)   ["qthCombo"]
    void $ comboBoxSetModelText qthCombo

    [callSignLabel] <- mapM (builderGetObject builder castToLabel)  ["qthCallSignLabel"]

    return $ QTHWidgets qthDlg
                        qthCombo
                        callSignLabel

runQTHDialog :: IORef PState -> IO ()
runQTHDialog state = do
    currentQTH <- readState state psQTH
    conf <- readState state psConf
    QTHWidgets{..} <- readState state psQTHWidgets

    -- Set the dialog to display the current QTH and its call sign.
    comboBoxSetActiveText qthCombo (T.pack currentQTH)

    let call = maybe "Unknown" qthCall (lookup currentQTH $ confQTHs conf)
    set qthCallLabel [ labelText := call]

    dialogRun qthDialog
    widgetHide qthDialog

    -- Change the current QTH in the program state so that newly added QSOs will be correct
    -- in the database.  If something went wrong and there's no active text in the combo
    -- (no idea how that could happen), just use the old one.
    newQTH <- maybe currentQTH T.unpack <$> comboBoxGetActiveText qthCombo
    modifyState state (\v -> v { psQTH = newQTH })
