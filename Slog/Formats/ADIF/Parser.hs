-- | This module exports a single function that converts a 'String' into an
-- 'ADIFFile'.  This is the only way to do such a conversion.  The 'Read' methods
-- in the 'Types' module should not be relied upon to do more than convert a single
-- token.
module Slog.Formats.ADIF.Parser(parseString) where

import Control.Applicative((<*), (*>))
import Data.Char(toUpper)
import Data.String.Utils(split)
import qualified Data.Text as T hiding(toUpper)
import Text.ParserCombinators.Parsec

import Slog.Formats.ADIF.Types
import Slog.Utils(uppercase)

stringToInt :: String -> Int
stringToInt s = fst $ (reads s :: [(Int, String)]) !! 0

stringToField :: (String, Int, Maybe Char) -> String -> Field
stringToField (name, len, ty) datum = case name of
    "ADDRESS"           -> Address $ split "\n\r" datum
    "AGE"               -> Age (read datum :: Integer)
    "A_INDEX"           -> AIndex (read datum :: Integer)
    "ANT_AZ"            -> AntennaAz (read datum :: Double)
    "ANT_EL"            -> AntennaEl (read datum :: Double)
    "ANT_PATH"          -> AntennaPath (read datum :: AntennaPath)
    "ARRL_SECT"         -> ARRLSection (read datum :: ARRLSection)
    "BAND"              -> Band (read datum :: Band)
    "BAND_RX"           -> BandRx (read datum :: Band)
    "CALL"              -> Call datum
    "CHECK"             -> Check datum
    "CLASS"             -> Class datum
    "CNTY"              -> County datum
    "COMMENT"           -> Comment datum
    "CONT"              -> Continent (read datum :: Continent)
    "CONTACTED_OP"      -> ContactedOp datum
--    "CONTEST_ID"
    "COUNTRY"           -> Country datum
    "CQZ"               -> CQZ (read datum :: Integer)
    "CREDIT_SUBMITTED"  -> CreditSubmitted $ map (\d -> read d :: Award) (split "," datum)
    "CREDIT_GRANTED"    -> CreditGranted $ map (\d -> read d :: Award) (split "," datum)
    "DISTANCE"          -> Distance (read datum :: Double)
    "DXCC"              -> Their_DXCC (read datum :: Integer)
    "EMAIL"             -> Email datum
    "EQ_CALL"           -> EqCall datum
    "EQSL_RDATE"        -> Eqsl_RDate $ Just datum
    "EQSL_SDATE"        -> Eqsl_SDate $ Just datum
    "EQSL_QSL_RCVD"     -> Eqsl_Received (read datum :: ReceivedStatus)
    "EQSL_QSL_SENT"     -> Eqsl_Sent (read datum :: SentStatus)
    "FORCE_INT"         -> ForceInt $ if datum == "Y" then True else False
    "FREQ"              -> Freq (read datum :: Double)
    "FREQ_RX"           -> Freq (read datum :: Double)
    "GRIDSQUARE"        -> Grid datum
    "GUEST_OP"          -> Operator datum
    "IOTA"              -> Their_IOTA (read datum :: Integer)
    "IOTA_ISLAND_ID"    -> IOTA_ID datum
    "ITUZ"              -> ITUZ (read datum :: Integer)
    "K_INDEX"           -> KIndex (read datum :: Integer)
    "LAT"               -> Lat (read datum :: Location)
    "LON"               -> Lon (read datum :: Location)
    "LOTW_QSLRDATE"     -> LOTW_RDate $ Just datum
    "LOTW_QSLSDATE"     -> LOTW_SDate $ Just datum
    "LOTW_QSL_RCVD"     -> LOTW_Received (read datum :: ReceivedStatus)
    "LOTW_QSL_SENT"     -> LOTW_Sent (read datum :: SentStatus)
    "MAX_BURSTS"        -> MaxBursts (read datum :: Integer)
    "MODE"              -> Mode (read datum :: Mode)
    "MS_SHOWER"         -> MS_Shower datum
    "MY_CITY"           -> MyCity datum
    "MY_CNTY"           -> MyCounty datum
    "MY_COUNTRY"        -> MyCountry datum
    "MY_CQ_ZONE"        -> MyCQZone (read datum :: Integer)
    "MY_GRIDSQUARE"     -> MyGrid datum
    "MY_IOTA"           -> My_IOTA datum
    "MY_IOTA_ISLAND_ID" -> My_IOTA_ID datum
    "MY_ITU_ZONE"       -> My_ITUZ (read datum :: Integer)
    "MY_LAT"            -> MyLat (read datum :: Location)
    "MY_LON"            -> MyLon (read datum :: Location)
    "MY_NAME"           -> MyName datum
    "MY_POSTAL_CODE"    -> MyPostalCode datum
    "MY_RIG"            -> MyRig datum
    "MY_SIG"            -> MySIG datum
    "MY_SIG_INFO"       -> MySIG_Info datum
    "MY_STATE"          -> MyState datum
    "MY_STREET"         -> MyStreet datum
    "NAME"              -> Name datum
    "NOTES"             -> Notes $ split "\n\r" datum
    "NR_BURSTS"         -> NumBursts (read datum :: Integer)
    "NR_PINGS"          -> NumPings (read datum :: Integer)
    "OPERATOR"          -> Operator datum
    "OWNER_CALLSIGN"    -> OwnerCall datum
    "PFX"               -> PFX datum
    "PRECEDENCE"        -> Precedence datum
    "PROP_MODE"         -> Propagation (read datum :: Propagation)
    "PUBLIC_KEY"        -> PublicKey datum
    "QSL_MSG"           -> QSL_Message $ split "\n\r" datum
    "QSLRDATE"          -> QSL_RDate $ Just datum
    "QSLSDATE"          -> QSL_SDate $ Just datum
    "QSL_RCVD"          -> QSL_Received (read datum :: ReceivedStatus)
    "QSL_RCVD_VIA"      -> QSL_RVia (read datum :: SentVia)
    "QSL_SENT"          -> QSL_Sent (read datum :: SentStatus)
    "QSL_SENT_VIA"      -> QSL_SVia (read datum :: SentVia)
    "QSL_VIA"           -> QSL_Via datum
    "QSO_COMPLETE"      -> QSO_Complete (read datum :: Complete)
    "QSO_DATE"          -> QSO_Date datum
    "QSO_DATE_OFF"      -> QSO_DateOff datum
    "QSO_RANDOM"        -> QSO_Random $ if datum == "Y" then True else False
    "QTH"               -> QTH datum
    "RIG"               -> Rig $ split "\n\r" datum
    "RST_RCVD"          -> RST_Received datum
    "RST_SENT"          -> RST_Sent datum
    "RX_PWR"            -> RxPower (read datum :: Integer)
    "SAT_MODE"          -> SatelliteMode datum
    "SAT_NAME"          -> SatelliteName datum
    "SFI"               -> SFI (read datum :: Double)
    "SIG"               -> SIG datum
    "SIG_INFO"          -> SIG_Info datum
    "SRX"               -> SRX (read datum :: Integer)
    "SRX_STRING"        -> SRX_String datum
    "STATE"             -> Slog.Formats.ADIF.Types.State datum
    "STATION_CALLSIGN"  -> StationCall datum
    "STX"               -> Serial (read datum :: Integer)
    "STX_STRING"        -> SerialString datum
    "SWL"               -> SWL $ if datum == "Y" then True else False
    "TEN_TEN"           -> TenTen (read datum :: Double)
    "TIME_OFF"          -> TimeOff datum
    "TIME_ON"           -> TimeOn datum
    "TX_PWR"            -> TxPower (read datum :: Integer)
    "VE_PROV"           -> Slog.Formats.ADIF.Types.State datum
    "WEB"               -> Web datum
    _                   -> Appdef $ AppDefined {appName=name,
                                                appLength=len,
                                                appType=ty,
                                                appValue=datum}

stringToHeaderField :: (String, Int, Maybe Char) -> String -> HeaderField
stringToHeaderField (name, len, ty) datum = case name of
    "ADIF_VER"          -> Version datum
    "PROGRAMID"         -> ProgramID datum
    "PROGRAMVERSION"    -> ProgramVersion datum
    _                   -> HeaderAppdef $ AppDefined {appName=name,
                                                      appLength=len,
                                                      appType=ty,
                                                      appValue=datum}

--
-- PARSER HELPER FUNCTIONS
--

-- Case insensitive version of the char parser.
char' :: Char -> CharParser st Char
char' c = satisfy (\x -> toUpper x == toUpper c)

-- Case insensitive version of the string parser.
string' :: String -> CharParser st ()
string' s = mapM_ char' s <?> s

--
-- THE ADIF PARSER
--

-- A file can have an optional header, then optionally some garbage characters, then a sequence
-- of records.
file        =  do hdr <- header
                  body <- skipMany garbage *> recordList
                  return $ ADIFFile {fileHeader=hdr, fileBody=body}
           <|> do body <- recordList
                  return $ ADIFFile {fileHeader=[], fileBody=body}

-- A header is an optional part of an ADIF file, but if it exists, it must start off with
-- some non-bracket character.  That's the crucial piece of information here.  Then, there
-- can be header-specific fields that follow the same rules as regular fields.  The whole
-- header is terminated with <EOH>.
header      = (skipMany1 garbage *> headerFList) <* eoh
headerFList = manyTill headerField (lookAhead $ try eoh)
headerField = do (name, len, ty) <- fieldID
                 datum <- count len anyChar <* skipMany garbage
                 return $ stringToHeaderField (name, len, ty) datum

-- A single record describes one QSO, and is terminated by <EOR>.  A list of records may
-- have arbitrary garbage characters between the <EOR> and the start of the next record.
recordList  = record `sepEndBy1` (eor *> skipMany garbage)
record      = manyTill field (lookAhead $ try eor)

-- A field describes one piece of data about a QSO and consists of an ID tag in brackets
-- followed by a specific number of characters, perhaps followed by more garbage before
-- the start of the next field.  So we only want to read as many characters as told.
field       = do (name, len, ty) <- fieldID
                 datum <- count len anyChar <* skipMany garbage
                 return $ stringToField (name, len, ty) datum
fieldID     = between (char '<') (char '>') fieldSpec
fieldSpec   = do name <- many1 $ noneOf ":<>"
                 len <- char ':' >> many1 digit
                 ty <- optionMaybe (char ':' >> anyChar)
                 return (uppercase name, stringToInt len, ty)

garbage     = noneOf "<"
eoh         = string' "<EOH>"
eor         = string' "<EOR>"

-- | Given a 'String' containing ADIF data, convert it into an 'ADIFFile'.  The complete and
-- exact type signature for this function is not given -- because the applicative parsing
-- functions make it into a giant mess.  However, the documented type signature here is close
-- enough for use.
parseString :: String -> Either ParseError ADIFFile
parseString s = let
    txt = T.strip $ T.pack s

    -- Data fetched from LOTW has "<APP_LoTW_EOF>" at the end of the text, which
    -- looks totally out of spec as far as I can tell.  We need to strip it out
    -- before doing the parsing.
    s'  = maybe s T.unpack (T.stripSuffix (T.pack "<APP_LoTW_EOF>") txt)
 in
    parse file "<stdin>" s'
