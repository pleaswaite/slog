-- | This module exports a couple functions that convert an 'ADIFFile' or a portion
-- of one into a 'String'.  This is the only way to do such a conversion.  The 'Show'
-- methods in the 'Types' module should not be relied upon to do more than convert a
-- single piece.
module Slog.Formats.ADIF.Writer(renderFile,
                                renderHeader,
                                renderRecord)
 where

import Data.List(intersperse)
import Data.Maybe(isJust, fromJust)
import Text.Printf(printf)

import Slog.Formats.ADIF.Types

-- | Convert an entire 'ADIFFile' into a 'String', headers and all.
renderFile :: ADIFFile -> String
renderFile f = header ++ "\n\r" ++ concat body
 where header = renderHeader $ fileHeader f
       body   = map renderRecord (fileBody f)

-- | Convert a record (which is just a 'Field' list) into a 'String'.
renderRecord :: [Field] -> String
renderRecord rec = concat body
 where body = map renderField rec ++ ["<EOR>"]

-- | Convert a header (which is just a 'HeaderField' list) into a 'String'.
renderHeader :: [HeaderField] -> String
renderHeader rec = concat body
 where body = map renderHeaderField rec ++ ["<EOH>"]

-- Convert a single record field into a string.
renderField :: Field -> String
renderField f = case f of
    Address ls          -> "<ADDRESS" ++ (renderString $ condense ls)
    Age age             -> "<AGE" ++ (renderString $ show age)
    AIndex ndx          -> "<A_INDEX" ++ (renderString $ show ndx)
    AntennaAz az        -> "<ANT_AZ" ++ (renderString $ show az)
    AntennaEl el        -> "<ANT_EL" ++ (renderString $ show el)
    AntennaPath p       -> "<ANT_PATH" ++ (renderString $ show p)
    ARRLSection x       -> "<ARRL_SECT" ++ (renderString $ show x)
    Band b              -> "<BAND" ++ (renderString $ show b)
    BandRx b            -> "<BAND_RX" ++ (renderString $ show b)
    Call s              -> "<CALL" ++ renderString s
    Check s             -> "<CHECK" ++ renderString s
    Class s             -> "<CLASS" ++ renderString s
    County x            -> "<CNTY" ++ renderString x
    Comment s           -> "<COMMENT" ++ renderString s
    Continent c         -> "<CONT" ++ (renderString $ show c)
    ContactedOp op      -> "<CONTACTED_OP" ++ renderString op
--    ContestID x
    Country c           -> "<COUNTRY" ++ renderString c
    CQZ zone            -> "<CQZ" ++ (renderString $ show zone)
    CreditSubmitted lst -> "<CREDIT_SUBMITTED" ++ (renderString $ concat $ intersperse "," $ map show lst)
    CreditGranted lst   -> "<CREDIT_GRANTED" ++ (renderString $ concat $ intersperse "," $ map show lst)
    Distance d          -> "<DISTANCE" ++ (renderString $ show d)
    Their_DXCC dxcc     -> "<DXCC" ++ (renderString $ show dxcc)
    Email s             -> "<EMAIL" ++ renderString s
    EqCall s            -> "<EQ_CALL" ++ renderString s
    Eqsl_RDate x        -> maybe "" (\date -> "<EQSL_RDATE" ++ renderDate date) x
    Eqsl_SDate x        -> maybe "" (\date -> "<EQSL_SDATE" ++ renderDate date) x
    Eqsl_Received r     -> "<EQSL_QSL_RCVD" ++ (renderString $ show r)
    Eqsl_Sent s         -> "<EQSL_QSL_SENT" ++ (renderString $ show s)
    ForceInt b          -> "<FORCE_INT" ++ (renderString $ renderBoolean b)
    Freq s              -> "<FREQ" ++ (renderString $ show s)
    FreqRx s            -> "<FREQ_RX" ++ (renderString $ show s)
    Grid g              -> "<GRIDSQUARE" ++ renderString g
    Their_IOTA i        -> "<IOTA" ++ (renderString $ show i)
    IOTA_ID i           -> "<IOTA_ISLAND_ID" ++ renderString i
    ITUZ zone           -> "<ITUZ" ++ (renderString $ show zone)
    KIndex ndx          -> "<K_INDEX" ++ (renderString $ show ndx)
    Lat x               -> "<LAT" ++ (renderString $ show x)
    Lon x               -> "<LON" ++ (renderString $ show x)
    LOTW_RDate x        -> maybe "" (\date -> "<LOTW_RDATE" ++ renderDate date) x
    LOTW_SDate x        -> maybe "" (\date -> "<LOTW_SDATE" ++ renderDate date) x
    LOTW_Received r     -> "<LOTW_QSL_RCVD" ++ (renderString $ show r)
    LOTW_Sent s         -> "<LOTW_QSL_SENT" ++ (renderString $ show s)
    MaxBursts b         -> "<MAX_BURSTS" ++ (renderString $ show b)
    Mode m              -> "<MODE" ++ (renderString $ show m)
    MS_Shower s         -> "<MS_SHOWER" ++ renderString s
    MyCity c            -> "<MY_CITY" ++ renderString c
    MyCounty x          -> "<MY_CNTY" ++ renderString x
    MyCountry x         -> "<MY_COUNTRY" ++ renderString x
    MyCQZone x          -> "<MY_CQ_ZONE" ++ (renderString $ show x)
    MyGrid x            -> "<MY_GRIDSQUARE" ++ renderString x
    My_IOTA x           -> "<MY_IOTA" ++ renderString x
    My_IOTA_ID x        -> "<MY_IOTA_ISLAND_ID" ++ renderString x
    My_ITUZ x           -> "<MY_ITU_ZONE" ++ (renderString $ show x)
    MyLat x             -> "<MY_LAT" ++ (renderString $ show x)
    MyLon x             -> "<MY_LON" ++ (renderString $ show x)
    MyName x            -> "<MY_NAME" ++ renderString x
    MyPostalCode x      -> "<MY_POSTAL_CODE" ++ renderString x
    MyRig x             -> "<MY_RIG" ++ renderString x
    MySIG x             -> "<MY_SIG" ++ renderString x
    MySIG_Info x        -> "<MY_SIG_INFO" ++ renderString x
    MyState x           -> "<MY_STATE" ++ renderString x
    MyStreet x          -> "<MY_STREET" ++ renderString x
    Name x              -> "<NAME" ++ renderString x
    Notes x             -> "<NOTES" ++ (renderString $ condense x)
    NumBursts x         -> "<NR_BURSTS" ++ (renderString $ show x)
    NumPings x          -> "<NR_PINGS" ++ (renderString $ show x)
    Operator x          -> "<OPERATOR" ++ renderString x
    OwnerCall x         -> "<OWNER_CALLSIGN" ++ renderString x
    PFX x               -> "<PFX" ++ renderString x
    Precedence x        -> "<PRECEDENCE" ++ renderString x
    Propagation x       -> "<PROP_MODE" ++ (renderString $ show x)
    PublicKey x         -> "<PUBLIC_KEY" ++ renderString x
    QSL_Message x       -> "<QSL_MSG" ++ (renderString $ condense x)
    QSL_RDate x         -> maybe "" (\date -> "<QSLRDATE" ++ renderDate date) x
    QSL_SDate x         -> maybe "" (\date -> "<QSLSDATE" ++ renderDate date) x
    QSL_Received x      -> "<QSL_RCVD" ++ (renderString $ show x)
    QSL_RVia x          -> "<QSL_RCVD_VIA" ++ (renderString $ show x)
    QSL_Sent x          -> "<QSL_SENT" ++ (renderString $ show x)
    QSL_SVia x          -> "<QSL_SENT_VIA" ++ (renderString $ show x)
    QSL_Via x           -> "<QSL_VIA" ++ renderString x
    QSO_Complete x      -> "<QSO_COMPLETE" ++ (renderString $ show x)
    QSO_Date x          -> "<QSO_DATE" ++ renderDate x
    QSO_DateOff x       -> "<QSO_DATE_OFF" ++ renderDate x
    QSO_Random x        -> "<QSO_RANDOM" ++ (renderString $ renderBoolean x)
    QTH x               -> "<QTH" ++ renderString x
    Rig x               -> "<RIG" ++ (renderString $ condense x)
    RST_Received x      -> "<RST_RCVD" ++ renderString x
    RST_Sent x          -> "<RST_SENT" ++ renderString x
    RxPower x           -> "<RX_PWR" ++ (renderString $ show x)
    SatelliteMode x     -> "<SAT_MODE" ++ renderString x
    SatelliteName x     -> "<SAT_NAME" ++ renderString x
    SFI x               -> "<SFI" ++ (renderString $ show x)
    SIG x               -> "<SIG" ++ renderString x
    SIG_Info x          -> "<SIG_INFO" ++ renderString x
    SRX x               -> "<SRX" ++ (renderString $ show x)
    SRX_String x        -> "<SRX_STRING" ++ renderString x
    State x             -> "<STATE" ++ renderString x
    StationCall x       -> "<STATION_CALLSIGN" ++ renderString x
    Serial x            -> "<STX" ++ (renderString $ show x)
    SerialString x      -> "<STX_STRING" ++ renderString x
    SWL x               -> "<SWL" ++ (renderString $ renderBoolean x)
    TenTen x            -> "<TEN_TEN" ++ (renderString $ show x)
    TimeOff x           -> "<TIME_OFF" ++ renderTime x
    TimeOn x            -> "<TIME_ON" ++ renderTime x
    TxPower x           -> "<TX_PWR" ++ (renderString $ show x)
    Web x               -> "<WEB" ++ renderString x
    Appdef x            -> renderAppDefined x
 where
    condense ls = concat $ intersperse "\r\n" ls
    renderAppDefined x | isJust (appType x) = printf "<%s:%d:%c>%s" (appName x) (appLength x) (fromJust $ appType x) (appValue x)
                       | otherwise          = printf "<%s:%d>%s" (appName x) (appLength x) (appValue x)
    renderBoolean b | b == True = "Y" | otherwise = "N"
    renderDate = renderString
    renderString s = ":" ++ (show $ length s) ++ ">" ++ s
    renderTime = renderString

renderHeaderField :: HeaderField -> String
renderHeaderField f = case f of
    ProgramID x         -> "<PROGRAMID" ++ renderString x
    ProgramVersion x    -> "<PROGRAMVERSION" ++ renderString x
--    Userdef x           -> renderUserdef x
    Version x           -> "<ADIF_VER" ++ renderString x
 where
    renderString s = ":" ++ (show $ length s) ++ ">" ++ s
