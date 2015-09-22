{-# OPTIONS_GHC -Wall #-}

-- | The Types module provides those types that don't really fit anywhere else.  For the
-- most part, this is GUI-related stuff.  This module should not import any other modules
-- in the 'Slog' collection.

module Types where

import Graphics.UI.Gtk

-- | A record to hold all the important widgets that can be displayed on the main screen.
data Widgets = Widgets {
    wCall :: Entry,                 -- ^ entry for remote station's callsign
    wFreqLabel :: Label,            -- ^ label next to frequency entry
    wFreq :: Entry,                 -- ^ entry for frequency
    wRxFreqLabel :: Label,          -- ^ label next to listening frequency label
    wRxFreq :: Entry,               -- ^ entry for frequency where remote station is listening (optional)
    wRSTRcvd :: Entry,              -- ^ entry for RST given by remote station
    wRSTSent :: Entry,              -- ^ entry for RST sent to remote station
    wXCRcvd :: Entry,               -- ^ entry for exchange given by remote station (optional)
    wXCSent :: Entry,               -- ^ entry for exchange sent to remote station (optional)

    wCurrent :: CheckButton,        -- ^ use current date and time?
    wRigctl :: CheckButton,         -- ^ get frequency info from rigctl?

    wDateLabel :: Label,            -- ^ label next to date entry
    wDate :: Entry,                 -- ^ entry for date of contact (used only if 'wCurrent' is Galse)
    wTimeLabel :: Label,            -- ^ label next to time entry
    wTime :: Entry,                 -- ^ entry for time of contact (used only if 'wCurrent' is False)

    wPrevious :: Frame,             -- ^ frame where previous contacts with remote station are displayed
    wDXCC :: Frame,                 -- ^ frame where status of DXCC with this entity is displayed
    wGrid :: Frame,                 -- ^ frame where status of VUCC with this grid is displayed
    wState :: Frame,                -- ^ frame where status of WAS with this state is displayed

    wLookup :: Button,              -- ^ button for looking up callsign in 'wCall'
    wClear :: Button,               -- ^ button for clearing out the UI
    wAdd :: Button,                 -- ^ button for adding a QSO

    wPreviousView :: TreeView,      -- ^ view for display of previous contacts with remote station
    wAllView :: TreeView,           -- ^ view for display of all QSOs on secondary tab

    wStatus :: Statusbar,           -- ^ area at the bottom of the screen for displaying status messages

    wNewQSOGrid :: Table,           -- ^ table containing all the above entries
    wDXCCGrid :: Table,             -- ^ table contained by 'wDXCC' where checkmarks will be displayed
    wGridGrid :: Table,             -- ^ table contained by 'wGrid' where checkmarks will be displayed
    wStateGrid :: Table,            -- ^ table contained by 'wState' where checkmarks will be displayed

    wAntenna :: ComboBox,           -- ^ antenna choices for this contact - will be populated in code
    wMode :: ComboBox,              -- ^ mode choices for this contact - will be populated in code

    wMainWindow :: Window,          -- ^ the main window

    wContestMenu :: Action,         -- ^ menu item for bringing up the contest mode dialog
    wQTHMenu :: Action              -- ^ menu item for bringing up the QTH configuration dialog
 }

-- | A record to hold all the widgets for the contest config dialog.
data CWidgets = CWidgets {
    cwDialog :: Dialog,             -- ^ the contest mode dialog
    cwBox :: Box,                   -- ^ box holding all the other widgets in the dialog

    cwEnable :: RadioButton,        -- ^ are we in contest mode or not?

    cwNotebook :: Notebook,         -- ^ notebook holding config info for each supported kind of contest

    cwGridGrid :: Entry,            -- ^ entry for our grid, on the grid page

    cwSerialSerial :: Entry,        -- ^ entry for starting serial number, on the serial page

    cwSweepsSerial :: Entry,        -- ^ entry for starting serial number, on the sweeps page
    cwSweepsPrec :: Entry,          -- ^ entry for precedence letter, on the sweeps page
    cwSweepsCall :: Entry,          -- ^ entry for our call sign, on the sweeps page
    cwSweepsCheck :: SpinButton,    -- ^ selector for check value, on the sweeps page
    cwSweepsSection :: Entry,       -- ^ entry for ARRL section, on the sweeps page

    cwZoneZone :: SpinButton,       -- ^ selector for CQ zone, on the zone page

    cwTenState :: Entry             -- ^ entry for state, on the sweeps page
 }

-- | A record to hold all the widgets for the QTH config dialog.
data QTHWidgets = QTHWidgets {
    qthDialog :: Dialog,            -- ^ the QTH configuration dialog
    qthCombo :: ComboBox,           -- ^ QTH choices - has lots of effects in the code
    qthCallLabel :: Label           -- ^ display the callsign associated with this QTH
 }

-- | The data type stored in a 'ListStore' and displayed in one of two places:  On the main
-- screen as part of the previous QSOs with a given call, and on the secondary screen as
-- part of the all QSOs list.
data DisplayRow = DisplayRow { dDate      :: String,           -- ^ date of contact (UTC)
                               dTime      :: String,           -- ^ time of contact (UTC)
                               dCall      :: String,           -- ^ remote station's call sign
                               dFreq      :: Double,           -- ^ frequency of contact
                               dRxFreq    :: Maybe Double,     -- ^ frequency remote station was listening on
                               dMode      :: String,           -- ^ mode used for contact
                               dDXCC      :: Maybe Integer,    -- ^ DXCC entity of remote station
                               dGrid      :: Maybe String,     -- ^ grid of remote station
                               dState     :: Maybe String,     -- ^ state of remote station
                               dXcIn      :: Maybe String,     -- ^ exchange given by remote station
                               dXcOut     :: Maybe String,     -- ^ exchange sent to remote station
                               dAntenna   :: Maybe String,     -- ^ antenna used for contact
                               dConfirmed :: Bool              -- ^ has contact been confirmed in LOTW yet?
 }
