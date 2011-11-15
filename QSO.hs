module QSO where

-- Some of these types are pretty useful everywhere.  Perhaps they should move
-- up into a non-ADIF specific module.
import qualified Formats.ADIF.Types as ADIF

data QSO = QSO {
    qDate :: ADIF.Date,
    qTime :: ADIF.Time,
    qFreq :: Double,
    qRxFreq :: Double,
    qMode :: ADIF.Mode,
    qDXCC :: String,
    qGrid :: String,
    qState :: String,
    qName :: String,
    qQSL_RDate :: ADIF.Date,
    qQSL_SDate :: ADIF.Date,
    qQSL_Rcvd :: ADIF.ReceivedStatus,
    qQSL_RcvdVia :: ADIF.SentVia,
    qQSL_Sent :: ADIF.SentStatus,
    qQSL_SentVia :: ADIF.SentVia,
    qNotes :: String,
    qLOTW_RDate :: ADIF.Date,
    qLOTW_SDate :: ADIF.Date,
    qLOTW_Rcvd :: ADIF.ReceivedStatus,
    qLOTW_Sent :: ADIF.SentStatus,
    qXcIn :: String,
    qXcOut :: String,
    qRST_Rcvd :: String,
    qRST_Sent :: String,
    qIOTA :: String,
    qITU :: Integer,
    qWAZ :: Integer,
    qCall :: String,
    qSatName :: String,
    qSatMode :: String }
 deriving (Eq, Show, Read)
