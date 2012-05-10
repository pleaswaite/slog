module Slog.QSO where

import Data.Maybe(catMaybes, fromJust, isNothing)

-- Some of these types are pretty useful everywhere.  Perhaps they should move
-- up into a non-ADIF specific module.
import qualified Slog.Formats.ADIF.Types as ADIF

import Slog.Utils(colonifyTime, dashifyDate, freqToBand, withoutSeconds)

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
    qIOTA      :: Maybe Integer,
    qITU       :: Maybe Integer,
    qWAZ       :: Maybe Integer,
    qCall      :: String,
    qSatName   :: Maybe String,
    qSatMode   :: Maybe String }
 deriving (Eq, Show, Read)

data Confirmation = Confirmation {
    qQSL_RDate    :: ADIF.Date,
    qQSL_SDate    :: ADIF.Date,
    qQSL_Rcvd     :: ADIF.ReceivedStatus,
    qQSL_RcvdVia  :: ADIF.SentVia,
    qQSL_Sent     :: ADIF.SentStatus,
    qQSL_SentVia  :: ADIF.SentVia,
    qLOTW_RDate   :: ADIF.Date,
    qLOTW_SDate   :: ADIF.Date,
    qLOTW_Rcvd    :: ADIF.ReceivedStatus,
    qLOTW_Sent    :: ADIF.SentStatus }
 deriving (Eq, Show, Read)

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
    (catMaybes [qRxFreq qso >>= Just . ADIF.FreqRx,
                qDXCC qso >>= Just . ADIF.Their_DXCC,
                qGrid qso >>= Just . ADIF.Grid,
                qState qso >>= Just . ADIF.State,
                qName qso >>= Just . ADIF.Name,
                qIOTA qso >>= Just . ADIF.Their_IOTA,
                qITU qso >>= Just . ADIF.ITUZ,
                qWAZ qso >>= Just . ADIF.CQZ,
                qSatName qso >>= Just . ADIF.SatelliteName,
                qSatMode qso >>= Just . ADIF.SatelliteMode])
 where
    band = freqToBand $ qFreq qso
