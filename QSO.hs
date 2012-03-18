module QSO where

import Data.List(find)
import Data.Maybe(catMaybes, fromJust)
import Utils(colonifyTime, dashifyDate, freqToBand, withoutSeconds)

-- Some of these types are pretty useful everywhere.  Perhaps they should move
-- up into a non-ADIF specific module.
import qualified Formats.ADIF.Types as ADIF

data QSO = QSO {
    qDate      :: ADIF.Date,
    qTime      :: ADIF.Time,
    qFreq      :: Double,
    qRxFreq    :: Maybe Double,
    qMode      :: ADIF.Mode,
    qDXCC      :: Maybe String,
    qGrid      :: Maybe String,
    qState     :: Maybe String,
    qName      :: Maybe String,
    qNotes     :: Maybe String,
    qXcIn      :: Maybe String,
    qXcOut     :: Maybe String,
    qRST_Rcvd  :: String,
    qRST_Sent  :: String,
    qIOTA      :: Maybe String,
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

-- This is just embarassing.
adifToQSO :: [ADIF.Field] -> QSO
adifToQSO fields = QSO {
    -- Things that are required and need conversion.
    qDate = dashifyDate                     $ fromJust $ find (\(ADIF.QSO_Date _) -> True) fields >>= \(ADIF.QSO_Date v) -> Just v,
    qTime = (colonifyTime . withoutSeconds) $ fromJust $ find (\(ADIF.TimeOn _) -> True) fields >>= \(ADIF.TimeOn v) -> Just v,

    -- Things that are required but we can take as-is.
    qFreq =       fromJust $ find (\(ADIF.Freq _) -> True) fields >>= \(ADIF.Freq v) -> Just v,
    qMode =       fromJust $ find (\(ADIF.Mode _) -> True) fields >>= \(ADIF.Mode v) -> Just v,
    qRST_Rcvd =   fromJust $ find (\(ADIF.RST_Received _) -> True) fields >>= \(ADIF.RST_Received v) -> Just v,
    qRST_Sent =   fromJust $ find (\(ADIF.RST_Sent _) -> True) fields >>= \(ADIF.RST_Sent v) -> Just v,
    qCall =       fromJust $ find (\(ADIF.Call _) -> True) fields >>= \(ADIF.Call v) -> Just v,

    -- Things that are optional.
    qRxFreq =  find (\(ADIF.FreqRx _) -> True) fields >>= \(ADIF.FreqRx v) -> Just v,
    qDXCC =    find (\(ADIF.Country _) -> True) fields >>= \(ADIF.Country v) -> Just v,
    qGrid =    find (\(ADIF.Grid _) -> True) fields >>= \(ADIF.Grid v) -> Just v,
    qState =   find (\(ADIF.State _) -> True) fields >>= \(ADIF.State v) -> Just v,
    qName =    find (\(ADIF.Name _) -> True) fields >>= \(ADIF.Name v) -> Just v,
    qIOTA =    find (\(ADIF.Their_IOTA _) -> True) fields >>= \(ADIF.Their_IOTA v) -> Just v,
    qITU =     find (\(ADIF.ITUZ _) -> True) fields >>= \(ADIF.ITUZ v) -> Just v,
    qWAZ =     find (\(ADIF.CQZ _) -> True) fields >>= \(ADIF.CQZ v) -> Just v,
    qSatName = find (\(ADIF.SatelliteName _) -> True) fields >>= \(ADIF.SatelliteName v) -> Just v,
    qSatMode = find (\(ADIF.SatelliteMode _) -> True) fields >>= \(ADIF.SatelliteMode v) -> Just v,

    -- Things we don't care about.
    qNotes = Nothing,
    qXcIn = Nothing,
    qXcOut = Nothing
 }

qsoToADIF :: QSO -> [ADIF.Field]
qsoToADIF qso =
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
    [ADIF.Band $ freqToBand $ qFreq qso] ++

    -- And now we add in everything that could potentially be set.
    (catMaybes [qRxFreq qso >>= Just . ADIF.FreqRx,
                qDXCC qso >>= Just . ADIF.Country,
                qGrid qso >>= Just . ADIF.Grid,
                qState qso >>= Just . ADIF.State,
                qName qso >>= Just . ADIF.Name,
                qIOTA qso >>= Just . ADIF.Their_IOTA,
                qITU qso >>= Just . ADIF.ITUZ,
                qWAZ qso >>= Just . ADIF.CQZ,
                qSatName qso >>= Just . ADIF.SatelliteName,
                qSatMode qso >>= Just . ADIF.SatelliteMode])
