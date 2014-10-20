{-# LANGUAGE LambdaCase #-}

-- | The Lookup module provides a mechanism to query the <http://www.hamqth.com> website
-- for information about a call sign.  Information is returned in a 'RadioAmateur'
-- record.
-- 
-- Before using this module, you must have a valid login on the website in order to
-- provide authentication credentials to the 'login' method.  Having done that, you
-- first call 'login' to obtain a 'SessionID'.  Then, simply repeatedly call
-- 'lookupCall' and inspect the results.
module Slog.Lookup.Lookup(RadioAmateur(..),
                          RAUses(..),
                          SessionID,
                          emptyRadioAmateur,
                          lookupCall, login)
 where

import Control.Exception(IOException, try)
import Control.Monad(mplus)
import Data.Maybe(catMaybes, isJust)
import Network.HTTP
import Text.Printf(printf)
import Text.XML.Light(unqual)
import Text.XML.Light.Input(parseXMLDoc)
import Text.XML.Light.Proc(filterElement, strContent)
import Text.XML.Light.Types(Element, QName, elName, qName)

import qualified Slog.Formats.ADIF.Types as A
import Slog.Utils(stringToInteger, uppercase)

-- | A RadioAmateur is a record used to return information following a query to
-- http://www.hamqth.com.  Not all fields will be provided for every call sign,
-- and not every field is even valid for every call sign.  For instance, the
-- 'raOblast' field will only mean something to a Russian call sign while the
-- 'raUSState' and 'raUSCounty' fields will only apply to an American call.
data RadioAmateur = RadioAmateur {
    raCall :: Maybe String,            -- ^ call sign searched for
    raNick :: Maybe String,            -- ^ real name
    raQTH :: Maybe String,             -- ^ home city/country/etc.
    raCountry :: Maybe String,         -- ^ country related to call, not address
    raITU :: Maybe Integer,            -- ^ ITU zone
    raWAZ :: Maybe Integer,            -- ^ CQ (WAZ) zone
    raGrid :: Maybe String,            -- ^ four or six digit grid identifier
    raAddrName :: Maybe String,        -- ^ address name
    raAddrStreet :: [String],          -- ^ street address
    raAddrCity :: Maybe String,        -- ^ city address
    raAddrZip :: Maybe String,         -- ^ ZIP code
    raAddrCountry :: Maybe String,     -- ^ country related to the address
    raDistrict :: Maybe String,        -- ^ station district
    raUSState :: Maybe String,         -- ^ US state (US calls only)
    raUSCounty :: Maybe String,        -- ^ US county (US calls only)
    raOblast :: Maybe String,          -- ^ Russian district (Russian calls only)
    raDOK :: Maybe Integer,            -- ^ DL stations
    raLOTW :: Maybe RAUses,            -- ^ Logbook of the World user?
    raIOTA :: Maybe Integer,           -- ^ Islands on the air reference number
    raQSLVia :: Maybe String,          -- ^ how to QSL
    raQSL :: Maybe RAUses,             -- ^ accepts QSL cards?
    raEQSL :: Maybe RAUses,            -- ^ eQSL user?
    raEMail :: Maybe String,           -- ^ email address
    raJabber :: Maybe String,          -- ^ jabber address
    raICQ :: Maybe Integer,            -- ^ ICQ number
    raMSN :: Maybe String,             -- ^ MSN address
    raSkype :: Maybe String,           -- ^ skype address
    raBirthYear :: Maybe Integer,      -- ^ year of birth
    raLicenseYear :: Maybe Integer,    -- ^ year of licensing
    raWeb :: Maybe String,             -- ^ URL to personal website
    raPicture :: Maybe String,         -- ^ URL to user's picture
    raLatitude :: Maybe String,        -- ^ station position (lat)
    raLongitude :: Maybe String,       -- ^ station position (long)
    raContinent :: Maybe A.Continent,  -- ^ continent
    raUTCOffset :: Maybe Integer       -- ^ station's offset to UTC time
 }
 deriving(Show)

-- | This data type is used by 'RadioAmateur' to determine whether or not the call sign
-- supports LOTW, eSQL, and paper QSL cards.  It's not a simple boolean due to the
-- potential unknown value.
data RAUses = Yes | No | Unknown
 deriving(Eq, Show)

-- | A SessionID is a 'String' used to communicate with <http://www.hamqth.com> without
-- the need to repeatedly authenticate.  It is acquired by calling 'login'.  A
-- SessionID will time out eventually, at which point 'login' will need to be called
-- again.
type SessionID = String

stringToRAUses :: String -> RAUses
stringToRAUses s | uppercase s == "Y" = Yes
                 | uppercase s == "N" = No
                 | otherwise          = Unknown

-- | Return an empty 'RadioAmateur' record - that is, one where all fields are
-- initialize to 'Nothing' or an empty list.  This is useful when constructing a
-- 'RadioAmateur' from some source other than 'lookupCall'.
emptyRadioAmateur :: RadioAmateur
emptyRadioAmateur = RadioAmateur {
    raCall           = Nothing,
    raNick           = Nothing,
    raQTH            = Nothing,
    raCountry        = Nothing,
    raITU            = Nothing,
    raWAZ            = Nothing,
    raGrid           = Nothing,
    raAddrName       = Nothing,
    raAddrStreet     = [],
    raAddrCity       = Nothing,
    raAddrZip        = Nothing,
    raAddrCountry    = Nothing,
    raDistrict       = Nothing,
    raUSState        = Nothing,
    raUSCounty       = Nothing,
    raOblast         = Nothing,
    raDOK            = Nothing,
    raLOTW           = Nothing,
    raIOTA           = Nothing,
    raQSLVia         = Nothing,
    raQSL            = Nothing,
    raEQSL           = Nothing,
    raEMail          = Nothing,
    raJabber         = Nothing,
    raICQ            = Nothing,
    raMSN            = Nothing,
    raSkype          = Nothing,
    raBirthYear      = Nothing,
    raLicenseYear    = Nothing,
    raWeb            = Nothing,
    raPicture        = Nothing,
    raLatitude       = Nothing,
    raLongitude      = Nothing,
    raContinent      = Nothing,
    raUTCOffset      = Nothing
 }

-- Given a parsed XML document, return a RadioAmateur record.
xmlToRadioAmateur :: Element -> Maybe RadioAmateur
xmlToRadioAmateur xml = Just
    RadioAmateur { raCall        = xml <?> "callsign",
                   raNick        = xml <?> "nick",
                   raQTH         = xml <?> "qth",
                   raCountry     = xml <?> "country",
                   raITU         = xml <?> "itu" >>= stringToInteger,
                   raWAZ         = xml <?> "cq" >>= stringToInteger,
                   raGrid        = xml <?> "grid",
                   raAddrName    = xml <?> "adr_name",
                   raAddrStreet  = catMaybes [xml <?> "adr_street1",
                                              xml <?> "adr_street2",
                                              xml <?> "adr_street3"],
                   raAddrCity    = xml <?> "adr_city",
                   raAddrZip     = xml <?> "adr_zip",
                   raAddrCountry = xml <?> "adr_country",
                   raDistrict    = xml <?> "district",
                   raUSState     = xml <?> "us_state",
                   raUSCounty    = xml <?> "us_county",
                   raOblast      = xml <?> "oblast",
                   raDOK         = xml <?> "dok" >>= stringToInteger,
                   raIOTA        = xml <?> "iota" >>= stringToInteger,
                   raQSLVia      = xml <?> "qsl_via",
                   raLOTW        = fmap stringToRAUses (xml <?> "lotw"),
                   raQSL         = fmap stringToRAUses (xml <?> "qsl"),
                   raEQSL        = fmap stringToRAUses (xml <?> "eqsl"),
                   raEMail       = xml <?> "email",
                   raJabber      = xml <?> "jabber",
                   raICQ         = xml <?> "icq" >>= stringToInteger,
                   raMSN         = xml <?> "msn",
                   raSkype       = xml <?> "skype",
                   raBirthYear   = xml <?> "birth_year" >>= stringToInteger,
                   raLicenseYear = xml <?> "lic_year" >>= stringToInteger,
                   raWeb         = xml <?> "web",
                   raPicture     = xml <?> "picture",
                   raLatitude    = xml <?> "latitude",
                   raLongitude   = xml <?> "longitude",
                   raContinent   = xml <?> "continent" >>= \c -> Just (read c :: A.Continent),
                   raUTCOffset   = xml <?> "utc_offset" >>= stringToInteger }
 where
    x <?> e = mplus (getElementValue e x) Nothing

-- We can't just use a straight-up == on QNames here because qURI is set and
-- the default comparison takes that into account, so anything made with unqual
-- above will fail.
elementHasName :: Text.XML.Light.Types.QName -> Element -> Bool
elementHasName qn ele = qName qn == (qName . elName) ele

-- Given a URL, fetch and return the body.  This will raise an IO exception on
-- error, so be ready to handle that.
getXML :: String -> IO String
getXML url = (simpleHTTP $ getRequest url) >>= getResponseBody

-- Given an XML element name and a processed document, lookup the element's value
-- and return that.
getElementValue :: String -> Element -> Maybe String
getElementValue ele xml =
    filterElement (elementHasName (unqual ele)) xml >>= (Just . strContent)

-- Did the XML document contain an error element?
gotError :: Element -> Bool
gotError xml = isJust $ filterElement (elementHasName (unqual "error")) xml

-- Wrap the above in code to make it act monadic.
responseIsValid :: Element -> Maybe Element
responseIsValid xml | gotError xml = Nothing
                    | otherwise    = Just xml

-- | Given a call sign and a 'SessionID', do a lookup at <http://www.hamqth.com>.  Return
-- a 'RadioAmateur' record if successful.  If you do not call 'login' first, this
-- function will not succeed.
lookupCall :: String -> SessionID -> IO (Maybe RadioAmateur)
lookupCall call sid =
    (try (getXML url) :: IO (Either IOException String)) >>= \case
        Right s   -> return $ parseXMLDoc s >>= responseIsValid >>= xmlToRadioAmateur
        _         -> return Nothing
 where
    url = printf "http://www.hamqth.com/xml.php?id=%s&callsign=%s&prg=slog" sid call

-- | Given a user name and password, login to <http://www.hamqth.com> and return the
-- returned session ID.  This operation may fail.  Be prepared.
login :: String -> String -> IO (Maybe SessionID)
login username pass =
    (try (getXML url) :: IO (Either IOException String)) >>= \case
        Right xml -> return $ parseXMLDoc xml >>= getElementValue "session_id"
        _         -> return Nothing
 where
    url = printf "http://www.hamqth.com/xml.php?u=%s&p=%s" username pass
