{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind -fno-warn-wrong-do-bind #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Dialogs.Contest(initContestDialog,
                       loadContestWidgets,
                       runContestDialog)
 where

import           Control.Applicative((<$>))
import           Control.Monad(when)
import           Data.IORef(IORef)
import           Data.Maybe(fromMaybe)
import qualified Data.Text as T
import           Graphics.UI.Gtk

import Slog.Utils(stringToInteger)

import Contest
import State(PState(..), modifyState, readState, withStateWidget_)
import Types(CWidgets(..), Widgets(..))

contestActive :: CWidgets -> IO Bool
contestActive CWidgets{..} = get cwEnable toggleButtonActive

initContestDialog :: CWidgets -> IO ()
initContestDialog CWidgets{..} = do
    -- Pack a combo box with a list of contest possibilities into the table.
    combo <- comboBoxNewText
    mapM_ (comboBoxAppendText combo . T.pack) ["ARRL Sweepstakes",
                                               "ARRL Ten Meter Contest",
                                               "ARRL VHF/UHF Contest",
                                               "CQ WW DX",
                                               "Generic Grid-Based Contest",
                                               "Generic Serial-Based Contest"]
    set combo       [ comboBoxActive := 5 ]
    set cwNotebook  [ notebookPage := 4 ]

    boxPackStart cwBox combo PackNatural 0
    boxReorderChild cwBox combo 3

    -- Then hook up a signal handler to only make it appear if contest mode is enabled.
    on cwEnable toggled $ do active <- get cwEnable toggleButtonActive
                             set combo      [ widgetVisible := active ]
                             set cwNotebook [ widgetVisible := active ]

    -- And this signal handler tells us which set of entries to display based on which
    -- item in the combo is chosen.
    on combo changed $ get combo comboBoxActive >>= \case
                           0 -> set cwNotebook [ notebookPage := 0 ]         -- arrl sweepstakes
                           1 -> set cwNotebook [ notebookPage := 1 ]         -- arrl ten meter contest
                           2 -> set cwNotebook [ notebookPage := 2 ]         -- arrl vhf/uhf contest
                           3 -> set cwNotebook [ notebookPage := 3 ]         -- cq ww dx
                           4 -> set cwNotebook [ notebookPage := 2 ]         -- generic grid-based contest
                           5 -> set cwNotebook [ notebookPage := 4 ]         -- generic serial-based contest
                           _ -> return ()                                    -- do nothing just in case

    return ()

loadContestWidgets :: Builder -> IO CWidgets
loadContestWidgets builder = do
    [contestDlg] <- mapM (builderGetObject builder castToDialog) ["contestDialog"]
    [box]        <- mapM (builderGetObject builder castToBox) ["contestDialogBox"]

    [gridGrid, serialSerial, sweepsSerial, sweepsPrec, sweepsCall,
     sweepsSection, tenState] <- mapM (builderGetObject builder castToEntry)
                                        ["gridGridEntry", "serialSerialEntry", "sweepsSerialEntry",
                                         "sweepsPrecEntry", "sweepsCallEntry", "sweepsSectionEntry",
                                         "tenStateEntry"]

    [enable]    <- mapM (builderGetObject builder castToRadioButton) ["enableContestButton"]
    [notebook]  <- mapM (builderGetObject builder castToNotebook) ["contestNotebook"]
    [sweepsCheck, zoneZone] <- mapM (builderGetObject builder castToSpinButton) ["sweepsCheck", "zoneZone"]

    return $ CWidgets contestDlg
                      box
                      enable
                      notebook
                      gridGrid
                      serialSerial
                      sweepsSerial sweepsPrec sweepsCall sweepsCheck sweepsSection
                      zoneZone
                      tenState

runContestDialog :: IORef PState -> IO ()
runContestDialog state = do
    dlg <- readState state (cwDialog . psCWidgets)
    rc <- dialogRun dlg

    -- If the user clicked OK, we need to grab the values out of the fields and
    -- return the data needed to run the contest state.
    when (rc == ResponseOk) $ do
        -- Are we even operating in contest mode?
        cw <- readState state psCWidgets
        contestMode <- contestActive cw

        -- If yes, we need to store new values for the exchange-generating function into
        -- program state.  If no, don't worry about it.  Checking psContestMode first will
        -- mean whatever else contest-related is stored won't matter.  We figure out which
        -- values to use by looking at what page the notebook is on.
        contestVal <- if contestMode then do
            notebook <- readState state (cwNotebook . psCWidgets)
            get notebook notebookPage >>= \case
                0 -> do serial  <- stringToInteger <$> get (cwSweepsSerial cw) entryText
                        prec    <- head <$> get (cwSweepsPrec cw) entryText
                        call    <- get (cwSweepsCall cw) entryText
                        check   <- truncate <$> get (cwSweepsCheck cw) spinButtonValue
                        section <- get (cwSweepsSection cw) entryText
                        return $ mkSweepsContest (Sweeps (fromMaybe 0 serial) prec call check section)

                1 -> mkTenMeterContest <$> get (cwTenState cw) entryText

                2 -> mkGridContest <$> get (cwGridGrid cw) entryText

                3 -> mkZoneContest <$> truncate <$> get (cwZoneZone cw) spinButtonValue

                4 -> do v <- stringToInteger <$> get (cwSerialSerial cw) entryText
                        return $ mkSerialContest (fromMaybe 0 v)

                _ -> return $ mkNoneContest ""
         else
             return $ mkNoneContest ""

        -- And then after all this, we should update the sent XC field immediately.
        withStateWidget_ state wXCSent (\w -> set w [ entryText := (T.pack . contestStr) contestVal ])

        -- Oh, RST in a contest is almost always 59/59, so just pre-fill that too so the user
        -- doesn't have to fill it in manually.  It's all about speed.
        withStateWidget_ state wRSTRcvd (\w -> set w [ entryText := if contestMode then "59" else "" ])
        withStateWidget_ state wRSTSent (\w -> set w [ entryText := if contestMode then "59" else "" ])

        -- Write the new state value with what we just found out.
        modifyState state (\v -> v { psContestMode = contestMode,
                                     psContestVal = contestVal })

    widgetHide dlg
