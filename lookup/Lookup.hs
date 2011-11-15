module Lookup(RadioAmateur(..),
              lookupCall, login)  where

import Control.Monad.Trans(liftIO)
import Data.Char(toUpper)
import Data.Maybe(isJust)
import Network.HTTP
import System.IO.Error(try)
import Text.XML.Light(unqual)
import Text.XML.Light.Input(parseXMLDoc)
import Text.XML.Light.Proc(filterElement, strContent)
import Text.XML.Light.Types(Element, elName, qName)

data RadioAmateur = RadioAmateur {
    raCall :: String,
    raNick :: String,
    raQTH :: String,
    raCountry :: String,
    raITU :: Integer,
    raWAZ :: Integer,
    raGrid :: String,
    raAddrName :: String,
    raAddrStreet :: String,
    raAddrCity :: String,
    raAddrZip :: String,
    raAddrCountry :: String,
    raDistrict :: String,
    raLOTW :: RAUses,
    raQSL :: RAUses,
    raEQSL :: RAUses,
    raEMail :: String,
    raJabber :: String,
    raSkype :: String,
    raBirthYear :: Integer,
    raLicenseYear :: Integer,
    raWeb :: String,
    raPicture :: String }
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
                   raITU         = stringToInteger $ xml <?> "itu",
                   raWAZ         = stringToInteger $ xml <?> "cq",
                   raGrid        = xml <?> "grid",
                   raAddrName    = xml <?> "adr_name",
                   raAddrStreet  = xml <?> "adr_street",
                   raAddrCity    = xml <?> "adr_city",
                   raAddrZip     = xml <?> "adr_zip",
                   raAddrCountry = xml <?> "adr_country",
                   raDistrict    = xml <?> "district",
                   raLOTW        = stringToRAUses $ xml <?> "lotw",
                   raQSL         = stringToRAUses $ xml <?> "qsl",
                   raEQSL        = stringToRAUses $ "eqsl",
                   raEMail       = xml <?> "email",
                   raJabber      = xml <?> "jabber",
                   raSkype       = xml <?> "skype",
                   raBirthYear   = stringToInteger $ xml <?> "birth_year",
                   raLicenseYear = stringToInteger $ xml <?> "lic_year",
                   raWeb         = xml <?> "web",
                   raPicture     = xml <?> "picture" }
 where
    xml <?> ele = maybe "" id (getElementValue ele xml)

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
lookupCall :: SessionID -> String -> IO (Maybe RadioAmateur)
lookupCall sid call = do
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
