module Lookup.Lookup(RadioAmateur(..),
                     SessionID,
                     lookupCall, login)
 where

import Control.Monad.Trans(liftIO)
import Data.Char(toUpper)
import Data.Maybe(catMaybes, isJust)
import Network.HTTP
import System.IO.Error(try)
import Text.XML.Light(unqual)
import Text.XML.Light.Input(parseXMLDoc)
import Text.XML.Light.Proc(filterElement, strContent)
import Text.XML.Light.Types(Element, elName, qName)

data RadioAmateur = RadioAmateur {
    raCall :: Maybe String,
    raNick :: Maybe String,
    raQTH :: Maybe String,
    raCountry :: Maybe String,
    raITU :: Maybe Integer,
    raWAZ :: Maybe Integer,
    raGrid :: Maybe String,
    raAddrName :: Maybe String,
    raAddrStreet :: [String],
    raAddrCity :: Maybe String,
    raAddrZip :: Maybe String,
    raAddrCountry :: Maybe String,
    raDistrict :: Maybe String,
    raUSState :: Maybe String,
    raUSCounty :: Maybe String,
    raOblast :: Maybe String,
    raDOK :: Maybe Integer,
    raLOTW :: Maybe RAUses,
    raIOTA :: Maybe String,
    raQSLVia :: Maybe String,
    raQSL :: Maybe RAUses,
    raEQSL :: Maybe RAUses,
    raEMail :: Maybe String,
    raJabber :: Maybe String,
    raSkype :: Maybe String,
    raBirthYear :: Maybe Integer,
    raLicenseYear :: Maybe Integer,
    raWeb :: Maybe String,
    raPicture :: Maybe String }
 deriving(Show)

data RAUses = Yes | No | Unknown
 deriving(Eq, Show)

type SessionID = String

stringToInteger "" = 0
stringToInteger s  = fst $ (reads s :: [(Integer, String)]) !! 0

stringToRAUses s = case uppercase s of
    "Y" -> Yes
    "N" -> No
    _   -> Unknown
 where uppercase = map toUpper

-- Given a parsed XML document, return a RadioAmateur record.
xmlToRadioAmateur :: Element -> Maybe RadioAmateur
xmlToRadioAmateur xml = Just $
    RadioAmateur { raCall        = xml <?> "callsign",
                   raNick        = xml <?> "nick",
                   raQTH         = xml <?> "qth",
                   raCountry     = xml <?> "country",
                   raITU         = maybe Nothing (Just . stringToInteger) (xml <?> "itu"),
                   raWAZ         = maybe Nothing (Just . stringToInteger) (xml <?> "cq"),
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
                   raDOK         = maybe Nothing (Just . stringToInteger) (xml <?> "dok"),
                   raIOTA        = xml <?> "iota",
                   raQSLVia      = xml <?> "qsl_via",
                   raLOTW        = maybe Nothing (Just . stringToRAUses) (xml <?> "lotw"),
                   raQSL         = maybe Nothing (Just . stringToRAUses) (xml <?> "qsl"),
                   raEQSL        = maybe Nothing (Just . stringToRAUses) (xml <?> "eqsl"),
                   raEMail       = xml <?> "email",
                   raJabber      = xml <?> "jabber",
                   raSkype       = xml <?> "skype",
                   raBirthYear   = maybe Nothing (Just . stringToInteger) (xml <?> "birth_year"),
                   raLicenseYear = maybe Nothing (Just . stringToInteger) (xml <?> "lic_year"),
                   raWeb         = xml <?> "web",
                   raPicture     = xml <?> "picture" }
 where
    xml <?> ele = maybe Nothing Just (getElementValue ele xml)

-- We can't just use a straight-up == on QNames here because qURI is set and
-- the default comparison takes that into account, so anything made with unqual
-- above will fail.
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

-- Given a session ID and a call sign, do a lookup at http://www.hamqth.com.  Return
-- a RadioAmateur record if successful.  Note:  you need to call login first or this
-- will not succeed.
lookupCall :: String -> SessionID -> IO (Maybe RadioAmateur)
lookupCall call sid = do
    let url = "http://www.hamqth.com/xml.php?id=" ++ sid ++ "&callsign=" ++ call ++ "&prg=slog"
    result <- try $ getXML url

    case result of
        Left err  -> return Nothing
        Right s   -> return $ parseXMLDoc s >>= responseIsValid >>= xmlToRadioAmateur

-- Given a user name and password, login to http://www.hamqth.com and return the
-- returned session ID.  This operation may fail.  Be prepared.
login :: String -> String -> IO (Maybe SessionID)
login username password = do
    let url = "http://www.hamqth.com/xml.php?u=" ++ username ++ "&p=" ++ password
    result <- try $ getXML url

    case result of
        Left err  -> return Nothing
        Right xml -> return $ parseXMLDoc xml >>= getElementValue "session_id"
