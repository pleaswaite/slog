{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE RecordWildCards #-}

module Progress(addEntityCheck,
                addGridCheck,
                addStateCheck,
                clearChecks,
                populateAllTables)
 where

import Control.Monad(when)
import Graphics.UI.Gtk

import Slog.Formats.ADIF.Types(Band(..))
import Slog.Mode(Mode, digitalMode)

import Types

populateTable :: Table -> [String] -> [String] -> IO ()
populateTable table modes bands = do
    mapM_ (\(band, x) -> do l <- createLabel band
                            tableAttach table l x (x+1) 0 1 [Fill] [] 0 0)
          (zip bands [1 .. nBands])

    hseparator <- hSeparatorNew
    tableAttach table hseparator 0 (nBands+1) 1 2 [Expand, Fill] [] 0 0

    mapM_ (\(mode, y) -> do l <- createLabel mode
                            tableAttach table l 0 1 (y+1) (y+2) [] [] 0 0)
          (zip modes [1 .. nModes])
 where
    nBands = length bands
    nModes = length modes

    createLabel s = do
        l <- labelNew (Nothing :: Maybe String)
        set l [ labelLabel := "<b>" ++ s ++ "</b>",
                labelUseMarkup := True,
                miscXalign := 0.5,
                miscYalign := 0.5 ]
        return l

rowForMode :: Mode -> Int
rowForMode s | digitalMode s = 3
             | otherwise     = 2

clearChecks :: Widgets -> IO ()
clearChecks Widgets{..} = do
    mapM_ (\widget -> set widget [ widgetSensitive := False ])
          [wDXCC, wGrid, wState]
    mapM_ (\cont -> containerForeach cont (removeImage cont))
          [wNewQSOGrid, wDXCCGrid, wGridGrid, wStateGrid]
 where
    removeImage container widget =
        when (isA widget gTypeImage) $ containerRemove container widget

populateAllTables :: Widgets -> IO ()
populateAllTables Widgets{..} = do
    populateTable wDXCCGrid  ["Phone", "Digital"] ["160M", "80M", "60M", "40M", "30M", "20M", "17M", "15M", "12M", "10M", "6M"]
    populateTable wGridGrid  ["Phone", "Digital"] ["6M", "2M", "1.25M", "70CM", "33CM", "23CM"]
    populateTable wStateGrid ["Phone", "Digital"] ["160M", "80M", "60M", "40M", "30M", "20M", "17M", "15M", "12M", "10M", "6M", "2M", "1.25M", "70CM"]

addEntityCheck :: Widgets -> (Maybe Band, Mode) -> IO ()
addEntityCheck Widgets{..} (band, mode)
    | band == Just Band160M = addCheckToTable wDXCCGrid 1  (rowForMode mode)
    | band == Just Band80M  = addCheckToTable wDXCCGrid 2  (rowForMode mode)
    | band == Just Band60M  = addCheckToTable wDXCCGrid 3  (rowForMode mode)
    | band == Just Band40M  = addCheckToTable wDXCCGrid 4  (rowForMode mode)
    | band == Just Band30M  = addCheckToTable wDXCCGrid 5  (rowForMode mode)
    | band == Just Band20M  = addCheckToTable wDXCCGrid 6  (rowForMode mode)
    | band == Just Band17M  = addCheckToTable wDXCCGrid 7  (rowForMode mode)
    | band == Just Band15M  = addCheckToTable wDXCCGrid 8  (rowForMode mode)
    | band == Just Band12M  = addCheckToTable wDXCCGrid 9  (rowForMode mode)
    | band == Just Band10M  = addCheckToTable wDXCCGrid 10 (rowForMode mode)
    | band == Just Band6M   = addCheckToTable wDXCCGrid 11 (rowForMode mode)
    | otherwise             = return ()

addGridCheck :: Widgets -> (Maybe Band, Mode) -> IO ()
addGridCheck Widgets{..} (band, mode)
    | band == Just Band6M        = addCheckToTable wGridGrid 1 (rowForMode mode)
    | band == Just Band2M        = addCheckToTable wGridGrid 2 (rowForMode mode)
    | band == Just Band1Point25M = addCheckToTable wGridGrid 3 (rowForMode mode)
    | band == Just Band70CM      = addCheckToTable wGridGrid 4 (rowForMode mode)
    | band == Just Band33CM      = addCheckToTable wGridGrid 5 (rowForMode mode)
    | band == Just Band23CM      = addCheckToTable wGridGrid 6 (rowForMode mode)
    | otherwise                  = return ()

addStateCheck :: Widgets -> (Maybe Band, Mode) -> IO ()
addStateCheck Widgets{..} (band, mode)
    | band == Just Band160M      = addCheckToTable wStateGrid 1  (rowForMode mode)
    | band == Just Band80M       = addCheckToTable wStateGrid 2  (rowForMode mode)
    | band == Just Band60M       = addCheckToTable wStateGrid 3  (rowForMode mode)
    | band == Just Band40M       = addCheckToTable wStateGrid 4  (rowForMode mode)
    | band == Just Band30M       = addCheckToTable wStateGrid 5  (rowForMode mode)
    | band == Just Band20M       = addCheckToTable wStateGrid 6  (rowForMode mode)
    | band == Just Band17M       = addCheckToTable wStateGrid 7  (rowForMode mode)
    | band == Just Band15M       = addCheckToTable wStateGrid 8  (rowForMode mode)
    | band == Just Band12M       = addCheckToTable wStateGrid 9  (rowForMode mode)
    | band == Just Band10M       = addCheckToTable wStateGrid 10 (rowForMode mode)
    | band == Just Band6M        = addCheckToTable wStateGrid 11 (rowForMode mode)
    | band == Just Band2M        = addCheckToTable wStateGrid 12 (rowForMode mode)
    | band == Just Band1Point25M = addCheckToTable wStateGrid 13 (rowForMode mode)
    | band == Just Band70CM      = addCheckToTable wStateGrid 14 (rowForMode mode)
    | otherwise                  = return ()

-- | Display a check mark in a table, at a given location.
addCheckToTable :: Table -> Int -> Int -> IO ()
addCheckToTable tbl col row = do
    img <- imageNewFromStock stockApply IconSizeSmallToolbar
    tableAttach tbl img
                col (col+1) row (row+1)
                [] []
                0 0
    widgetShowAll tbl
    return ()
