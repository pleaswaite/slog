module QSO where

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
