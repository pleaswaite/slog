-- | This module exports the most basic data types used throughout the Slog library
-- and utilities.
module Slog.QSO(QSO(..),
                Confirmation(..),
                isConfirmed,
                qsoToADIF)
  where

import Data.Maybe(catMaybes, fromJust, isJust, isNothing)

-- Some of these types are pretty useful everywhere.  Perhaps they should move
-- up into a non-ADIF specific module.
import qualified Slog.Formats.ADIF.Types as ADIF
import Slog.Formats.ADIF.Utils(freqToBand)

{-# ANN module "HLint: ignore Use camelCase" #-}

-- | A 'QSO' is a record used to convey information about a single radio contact within
-- the Slog library and utilities.  It is the interchange format between the database
-- and the rest of the library.  In fact this record closely matches the layout of one
-- table in the database.  The qsoadd utility builds up this record from the command line
-- or graphical interface.
--
-- While this record can contain a lot of data, most of it is optional.  Much can be
-- determined automatically by looking up a call sign.  Other information is not
-- needed at all.
data QSO = QSO {
    qDate      :: ADIF.Date,
    qTime      :: ADIF.Time,
    qFreq      :: Double,
    qRxFreq    :: Maybe Double,
    qMode      :: ADIF.Mode,
    qDXCC      :: Maybe Integer,
    qGrid      :: Maybe String,
    qState     :: Maybe String,
    qName      :: Maybe String,
    qNotes     :: Maybe String,
    qXcIn      :: Maybe String,
    qXcOut     :: Maybe String,
    qRST_Rcvd  :: String,
    qRST_Sent  :: String,
    qITU       :: Maybe Integer,
    qWAZ       :: Maybe Integer,
    qCall      :: String,
    qPropMode  :: Maybe ADIF.Propagation,
    qSatName   :: Maybe String,
    qAntenna   :: Maybe String}
 deriving (Eq, Show, Read)

-- | A 'Confirmation' record is used to convey information about whether a 'QSO'
-- has been confirmed via a variety of methods.  This record closely matches the layout
-- of another table in the database, but is much less widely used than the 'QSO'
-- record.
data Confirmation = Confirmation {
    qQSL_RDate    :: Maybe ADIF.Date,
    qQSL_SDate    :: Maybe ADIF.Date,
    qQSL_RcvdVia  :: Maybe ADIF.SentVia,
    qQSL_SentVia  :: Maybe ADIF.SentVia,
    qLOTW_RDate   :: Maybe ADIF.Date,
    qLOTW_SDate   :: Maybe ADIF.Date }
 deriving (Eq, Show, Read)

-- | Given a 'Confirmation' record, determine if it has actually been confirmed or not.
-- For our purposes, this means confirmed in LOTW.
isConfirmed :: Confirmation -> Bool
isConfirmed c = isJust $ qLOTW_RDate c

-- | Given a 'QSO' record, attempt to convert it into a list of ADIF fields.  This is
-- used to grab QSOs out of the database and upload them to LOTW.
qsoToADIF :: QSO -> [ADIF.Field]
qsoToADIF qso = if isNothing band then [] else
    -- First, let's get the required fields we know will always exist.
    [ADIF.QSO_Date $ qDate qso,
     ADIF.TimeOn $ qTime qso,
     ADIF.Freq $ qFreq qso,
     ADIF.Mode $ qMode qso,
     ADIF.RST_Received $ qRST_Rcvd qso,
     ADIF.RST_Sent $ qRST_Sent qso,
     ADIF.Call $ qCall qso] ++

    -- LOTW wants the band, not the frequency, so just make sure that's
    -- included here.
    [ADIF.Band $ fromJust band] ++

    -- And now we add in everything that could potentially be set.
    catMaybes [qRxFreq qso >>= Just . ADIF.FreqRx,
               qDXCC qso >>= Just . ADIF.Their_DXCC,
               qGrid qso >>= Just . ADIF.Grid,
               qState qso >>= Just . ADIF.State,
               qName qso >>= Just . ADIF.Name,
               qITU qso >>= Just . ADIF.ITUZ,
               qWAZ qso >>= Just . ADIF.CQZ,
               qPropMode qso >>= Just . ADIF.Propagation,
               qSatName qso >>= Just . ADIF.SatelliteName]
 where
    band = freqToBand $ qFreq qso
