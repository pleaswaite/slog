module Formats.ADIF.Parser(parseString) where

import Control.Applicative((<*), (*>))
import Data.Char(toUpper)
import Data.String.Utils(split)
import Text.ParserCombinators.Parsec

import Formats.ADIF.Types

stringToInt :: String -> Int
stringToInt s = fst $ (reads s :: [(Int, String)]) !! 0

uppercase = map toUpper

stringToField :: (String, Int, Maybe Char) -> String -> Field
stringToField (name, length, ty) datum = case name of
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
    "IOTA"              -> Their_IOTA datum
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
    "SAT_MODE"          -> Satellite_Mode datum
    "SAT_NAME"          -> Satellite_Name datum
    "SFI"               -> SFI (read datum :: Double)
    "SIG"               -> SIG datum
    "SIG_INFO"          -> SIG_Info datum
    "SRX"               -> SRX (read datum :: Integer)
    "SRX_STRING"        -> SRX_String datum
    "STATE"             -> Types.State datum
    "STATION_CALLSIGN"  -> StationCall datum
    "STX"               -> Serial (read datum :: Integer)
    "STX_STRING"        -> SerialString datum
    "SWL"               -> SWL $ if datum == "Y" then True else False
    "TEN_TEN"           -> TenTen (read datum :: Double)
    "TIME_OFF"          -> TimeOff datum
    "TIME_ON"           -> TimeOn datum
    "TX_PWR"            -> TxPower (read datum :: Integer)
    "VE_PROV"           -> Types.State datum
    "WEB"               -> Web datum
    _                   -> Appdef $ AppDefined {appName=name,
                                                appLength=length,
                                                appType=ty,
                                                appValue=datum}

stringToHeaderField :: (String, Int, Maybe Char) -> String -> HeaderField
stringToHeaderField (name, _, _) datum = case name of
    "ADIF_VER"          -> Version datum
    "PROGRAMID"         -> ProgramID datum
    "PROGRAMVERSION"    -> ProgramVersion datum

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
file        =  do header <- header
                  body <- skipMany garbage *> recordList
                  return $ ADIFFile {fileHeader=header, fileBody=body}
           <|> do body <- recordList
                  return $ ADIFFile {fileHeader=[], fileBody=body}

-- A header is an optional part of an ADIF file, but if it exists, it must start off with
-- some non-bracket character.  That's the crucial piece of information here.  Then, there
-- can be header-specific fields that follow the same rules as regular fields.  The whole
-- header is terminated with <EOH>.
header      = (skipMany1 garbage *> headerFList) <* eoh
headerFList = manyTill headerField (lookAhead $ try eoh)
headerField = do (name, length, ty) <- fieldID
                 datum <- count length anyChar <* skipMany garbage
                 return $ stringToHeaderField (name, length, ty) datum

-- A single record describes one QSO, and is terminated by <EOR>.  A list of records may
-- have arbitrary garbage characters between the <EOR> and the start of the next record.
recordList  = record `sepEndBy1` (eor *> skipMany garbage)
record      = manyTill field (lookAhead $ try eor)

-- A field describes one piece of data about a QSO and consists of an ID tag in brackets
-- followed by a specific number of characters, perhaps followed by more garbage before
-- the start of the next field.  So we only want to read as many characters as told.
field       = do (name, length, ty) <- fieldID
                 datum <- count length anyChar <* skipMany garbage
                 return $ stringToField (name, length, ty) datum
fieldID     = between (char '<') (char '>') fieldSpec
fieldSpec   = do name <- many1 $ noneOf ":<>"
                 length <- char ':' >> many1 digit
                 ty <- optionMaybe (char ':' >> anyChar)
                 return (uppercase name, stringToInt length, ty)

garbage     = noneOf "<"
eoh         = string' "<EOH>"
eor         = string' "<EOR>"

parseString = parse file "<stdin>"
