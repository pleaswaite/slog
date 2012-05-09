module Slog.Formats.ADIF.Types(Date, Time,
                               Location(..),
                               ADIFFile(..),
                               HeaderField(..),
                               Field(..),
                               AppDefined(..),
                               UserDefined(..),
                               ARRLSection(..),
                               AntennaPath(..),
                               Award(..),
                               Band(..),
                               Complete(..),
                               Continent(..),
                               Mode(..),
                               Propagation(..),
                               ReceivedStatus(..),
                               SentStatus(..),
                               SentVia(..))
 where

import Data.Maybe(fromMaybe)
import Text.Printf(printf)

--
-- BASIC FIELD DATA TYPES
--
type Date = String
type Time = String

data Location = Location { locDirection :: Char,
                           locDegrees :: Integer,
                           locMinutes :: Double
 } deriving (Eq, Read)

instance Show Location where
    show loc = printf "%c%03d %02.3f" (locDirection loc) (locDegrees loc) (locMinutes loc)

--
-- TOP-LEVEL DATA TYPE
--

-- An ADIF file consists of an optional header, followed by multiple records.  Each
-- record consists of a list of fields.
data ADIFFile = ADIFFile { fileHeader :: [HeaderField],
                           fileBody :: [[Field]]
 } deriving (Read, Show)

--
-- FIELDS AND HEADER FIELDS
--

-- The header contains only a couple special fields.
data HeaderField = ProgramID        String
                 | ProgramVersion   String
                 | Userdef          UserDefined
                 | Version          String
                 | HeaderAppdef     AppDefined
 deriving (Read, Show)

-- The records can contain a very large variety of fields.
data Field = Address          [String]
           | Age              Integer
           | AIndex           Integer
           | AntennaAz        Double
           | AntennaEl        Double
           | AntennaPath      AntennaPath
           | ARRLSection      ARRLSection
           | Band             Band
           | BandRx           Band
           | Call             String
           | Check            String
           | Class            String
           | County           String
           | Comment          String
           | Continent        Continent
           | ContactedOp      String
--           | ContestID        ContestID
           | Country          String
           | CQZ              Integer
           | CreditSubmitted  [Award]
           | CreditGranted    [Award]
           | Distance         Double
           | Their_DXCC       Integer
           | Email            String
           | EqCall           String
           | Eqsl_RDate       (Maybe Date)
           | Eqsl_SDate       (Maybe Date)
           | Eqsl_Received    ReceivedStatus
           | Eqsl_Sent        SentStatus
           | ForceInt         Bool
           | Freq             Double
           | FreqRx           Double
           | Grid             String
           | Their_IOTA       String
           | IOTA_ID          String
           | ITUZ             Integer
           | KIndex           Integer
           | Lat              Location
           | Lon              Location
           | LOTW_RDate       (Maybe Date)
           | LOTW_SDate       (Maybe Date)
           | LOTW_Received    ReceivedStatus
           | LOTW_Sent        SentStatus
           | MaxBursts        Integer
           | Mode             Mode
           | MS_Shower        String
           | MyCity           String
           | MyCounty         String
           | MyCountry        String
           | MyCQZone         Integer
           | MyGrid           String
           | My_IOTA          String
           | My_IOTA_ID       String
           | My_ITUZ          Integer
           | MyLat            Location
           | MyLon            Location
           | MyName           String
           | MyPostalCode     String
           | MyRig            String
           | MySIG            String
           | MySIG_Info       String
           | MyState          String
           | MyStreet         String
           | Name             String
           | Notes            [String]
           | NumBursts        Integer
           | NumPings         Integer
           | Operator         String
           | OwnerCall        String
           | PFX              String
           | Precedence       String
           | Propagation      Propagation
           | PublicKey        String
           | QSL_Message      [String]
           | QSL_RDate        (Maybe Date)
           | QSL_SDate        (Maybe Date)
           | QSL_Received     ReceivedStatus
           | QSL_RVia         SentVia
           | QSL_Sent         SentStatus
           | QSL_SVia         SentVia
           | QSL_Via          String
           | QSO_Complete     Complete
           | QSO_Date         Date
           | QSO_DateOff      Date
           | QSO_Random       Bool
           | QTH              String
           | Rig              [String]
           | RST_Received     String
           | RST_Sent         String
           | RxPower          Integer
           | SatelliteMode    String
           | SatelliteName    String
           | SFI              Double
           | SIG              String
           | SIG_Info         String
           | SRX              Integer
           | SRX_String       String
           | State            String
           | StationCall      String
           | Serial           Integer
           | SerialString     String
           | SWL              Bool
           | TenTen           Double
           | TimeOff          Time
           | TimeOn           Time
           | TxPower          Integer
           | Web              String
           | Appdef           AppDefined
 deriving (Read, Show)

--
-- ADDITIONAL DEFINED FIELDS
--

-- Applications may define their own fields, of course.  Here's a definition for them.
data AppDefined = AppDefined { appName :: String,
                               appLength :: Int,
                               appType :: Maybe Char,
                               appValue :: String
 } deriving (Read, Show)

-- Users may also define their own fields, but only in the header.  Here's a definition for
-- those, too.  Is this really necessary?
data UserDefined = UserPlain  String
                 | UserEnum   (String, [String])
                 | UserRange  (String, (Integer, Integer))
 deriving (Read, Show)

--
-- DATA TYPES FOR (MOST) ADIF ENUMERATIONS
-- Note that we do not have enumerations for Primary and Secondary Administrative Subdivisions,
-- nor for Country Codes.  There are about a billion of those, so I'm just going to use a String
-- to represent them and hope it all works out.
--

--
-- ARRL Section
-- FIXME:  Add PAC, MS (these overlap with other data types)
--
data ARRLSection = AL | AK | AB | AR | AZ | BC | CO | CT | DE | EB | EMA | ENY | EPA | EWA |
                   GA | ID | IL | IN | IA | KS | KY | LAX | LA | ME | MB | MAR | MDC | MI |
                   MN | MO | MT | NE | NV | NH | NM | NLI | NL | NC | ND | NTX | NFL |
                   NNJ | NNY | NT | NWT | OH | OK | ON | ORG | OR | PR | QX | RI | SV |
                   SDG | SF | SJV | SB | SCV | SK | SC | SD | STX | SFL | SNJ | TN | VI | UT |
                   VT | VA | WCF | WTX | WV | WMA | WNY | WPA | WWA | WI | WY
 deriving (Eq, Read, Show)

--
-- What path did the signal take?
--
data AntennaPath = GrayLine
                 | ShortPath
                 | LongPath
                 | Other
 deriving (Eq, Ord)

antennaPathMap :: [(AntennaPath, String)]
antennaPathMap = [(GrayLine, "G"), (ShortPath, "S"), (LongPath, "L"), (Other, "O")]

antennaPathMap' :: [(String, AntennaPath)]
antennaPathMap' = invert antennaPathMap

instance Show AntennaPath where
    show path = fromMaybe "" (lookup path antennaPathMap)

instance Read AntennaPath where
    readsPrec _ path = maybe [] (\b -> [(b, "")]) (lookup path antennaPathMap')

--
-- What awards has credit been submitted and granted for?
--
data Award = AJA | CQDX | CQDXFIELD | CQWAZ_MIXED | CQWAZ_CW | CQWAZ_PHONE |
             CQWAZ_RTTY | CQWAZ_160m | CQWPX | DARC_DOK | DXCC | DXCC_MIXED |
             DXCC_CW | DXCC_PHONE | DXCC_RTTY | IOTA | JCC | JCG | MARATHON |
             RDA | WAB | WAC | WAE | WAIP | WAJA | WAS | WAZ | USACA | VUCC
 deriving (Eq, Read, Show)

--
-- What band did the QSO take place on?
--
data Band = Band2190M | Band560M | Band160M | Band80M | Band60M | Band40M
          | Band30M | Band20M | Band17M | Band15M | Band12M | Band10M
          | Band6M | Band4M | Band2M | Band1Point25M | Band70CM | Band33CM
          | Band23CM | Band13CM | Band9CM | Band6CM | Band3CM | Band1Point25CM
          | Band6MM | Band4MM | Band2Point5MM | Band2MM | Band1MM
 deriving (Eq)

bandMap :: [(Band, String)]
bandMap = [(Band2190M, "2190M"), (Band560M, "560M"), (Band160M, "160M"), (Band80M, "80M"),
           (Band60M, "60M"), (Band40M, "40M"), (Band30M, "30M"), (Band20M, "20M"),
           (Band17M, "17M"), (Band15M, "15M"), (Band12M, "12M"), (Band10M, "10M"),
           (Band6M, "6M"), (Band4M, "4M"), (Band2M, "2M"), (Band1Point25M, "1.25M"),
           (Band70CM, "70CM"), (Band33CM, "33CM"), (Band23CM, "23CM"), (Band13CM, "13CM"),
           (Band9CM, "9CM"), (Band6CM, "6CM"), (Band3CM, "3CM"), (Band1Point25CM, "1.25CM"),
           (Band6MM, "6MM"), (Band4MM, "4MM"), (Band2Point5MM, "2.5MM"), (Band2MM, "2MM"),
           (Band1MM, "1MM")]

bandMap' :: [(String, Band)]
bandMap' = invert bandMap


instance Show Band where
    show band = fromMaybe "" (lookup band bandMap)

instance Read Band where
    readsPrec _ band = maybe [] (\b -> [(b, "")]) (lookup band bandMap')

--
-- Was the QSO completed?
--
data Complete = CYes | CNo | CNil | CUnknown
 deriving (Eq)

completeMap :: [(Complete, String)]
completeMap = [(CYes, "Y"), (CNo, "N"), (CNil, "NIL"), (CUnknown, "?")]

completeMap' :: [(String, Complete)]
completeMap' = invert completeMap

instance Show Complete where
    show comp = fromMaybe "" (lookup comp completeMap)

instance Read Complete where
    readsPrec _ comp = maybe [] (\c -> [(c, "")]) (lookup comp completeMap')

--
-- Which continent is the remote end on?
--
data Continent = NA | SA | EU | AF | OC | AS | AN
 deriving (Eq, Read, Show)

--
-- What mode was used?
--
data Mode = AM | AMTORFEC | ASCI | ATV | CHIP64 | CHIP128 | CLO | CONTESTI |
            CW | DSTAR | DOMINO | DOMINOF | FAX | FM | FMHELL | FSK31 | FSK441 |
            GTOR | HELL | HELL80 | HFSK | JT44 | JT4A | JT4B | JT4C | JT4D |
            JT4E | JT4F | JT4G | JT65 | JT65A | JT65B | JT65C | JT6M | MFSK8 |
            MFSK16 | MT63 | OLIVIA | PAC | PAC2 | PAC3 | PAX | PAX2 | PCW |
            PSK10 | PSK31 | PSK63 | PSK63F | PSK125 | PSKAM10 | PSKAM31 |
            PSKAM50 | PSKFEC31 | PSKHELL | Q15 | QPSK31 | QPSK63 | QPSK125 |
            ROS | RTTY | RTTYM | SSB | SSTV | THRB | THOR | THRBX | TOR | VOI |
            WINMOR | WSPR
 deriving (Eq, Read, Show)

--
-- What was the observed propagation?
--
data Propagation = AUR | AUE | BS | ECH | EME | ES | FAI | F2 | INTERNET |
                   ION | IRL | MS | RPT | RS | SAT | TEP | TR
 deriving (Eq, Read, Show)

--
-- QSL/EQSL/LOTW Received/Sent status
--
data ReceivedStatus = RYes | RNo | RRequested | RInvalid | RValidated
 deriving (Eq)

receivedStatusMap :: [(ReceivedStatus, String)]
receivedStatusMap = [(RYes, "Y"), (RNo, "N"), (RRequested, "R"),
                     (RInvalid, "I"), (RValidated, "V")]

receivedStatusMap' :: [(String, ReceivedStatus)]
receivedStatusMap' = invert receivedStatusMap

instance Show ReceivedStatus where
    show status = fromMaybe "" (lookup status receivedStatusMap)

instance Read ReceivedStatus where
    readsPrec _ status = maybe [] (\s -> [(s, "")]) (lookup status receivedStatusMap')

data SentStatus = SYes | SNo | SRequested | SQueued | SInvalid
 deriving (Eq)

sentStatusMap :: [(SentStatus, String)]
sentStatusMap = [(SYes, "Y"), (SNo, "N"), (SRequested, "R"), (SQueued, "Q"), (SInvalid, "I")]

sentStatusMap' :: [(String, SentStatus)]
sentStatusMap' = invert sentStatusMap

instance Show SentStatus where
    show status = fromMaybe "" (lookup status sentStatusMap)

instance Read SentStatus where
    readsPrec _ status = maybe [] (\s -> [(s, "")]) (lookup status sentStatusMap')

--
-- How was the QSL sent?
--
data SentVia = Bureau | Direct | Electronic | Manager
 deriving (Eq)

sentViaMap :: [(SentVia, String)]
sentViaMap = [(Bureau, "B"), (Direct, "D"), (Electronic, "E"), (Manager, "M")]

sentViaMap' :: [(String, SentVia)]
sentViaMap' = invert sentViaMap

instance Show SentVia where
    show sent = fromMaybe "" (lookup sent sentViaMap)

instance Read SentVia where
    readsPrec _ sent = maybe [] (\s -> [(s, "")]) (lookup sent sentViaMap')

-- Given an association list, turn it inside out
invert :: [(a, b)] -> [(b, a)]
invert = map (\(a, b) -> (b, a))

