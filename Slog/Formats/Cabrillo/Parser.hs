{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE DoAndIfThenElse #-}

-- | This module exports a single function that converts a 'String' into a
-- 'CabrilloFile'.  This is the only way to do such a conversion.  The 'Read' methods
-- in the 'Types' module should not be relied upon to do more than convert a single
-- token.
module Slog.Formats.Cabrillo.Parser(parseString) where

import Control.Applicative((<$>))
import Data.List(partition)
import Data.Maybe(mapMaybe)
import Data.String.Utils(split)
import qualified Data.Text as T hiding(toUpper)
import Text.ParserCombinators.Parsec

import Slog.Formats.Cabrillo.Contest.Convert(toQSO)
import Slog.Formats.Cabrillo.Types
import Slog.Utils(uppercase)

stringToTag :: String -> String -> String -> Either String Tag
stringToTag contestName name datum = case name of
    "START-OF-LOG"              -> Right $ StartOfLog datum
    "END-OF-LOG"                -> Right EndOfLog
    "CALLSIGN"                  -> Right $ Callsign datum
    "CONTEST"                   -> Right $ Contest datum
    "CATEGORY-ASSISTED"         -> Right $ CategoryAssisted (read datum :: Assistance)
    "CATEGORY-BAND"             -> Right $ CategoryBand (read datum :: Band)
    "CATEGORY-MODE"             -> Right $ CategoryMode (read datum :: Mode)
    "CATEGORY-OPERATOR"         -> Right $ CategoryOperator (read datum :: Operator)
    "CATEGORY-POWER"            -> Right $ CategoryPower (read datum :: Power)
    "CATEGORY-STATION"          -> Right $ CategoryStation (read datum :: Station)
    "CATEGORY-TIME"             -> Right $ CategoryTime (read datum :: TimeOperated)
    "CATEGORY-TRANSMITTER"      -> Right $ CategoryTransmitter (read datum :: Transmitter)
    "CATEGORY-OVERLAY"          -> Right $ CategoryOverlay (read datum :: Overlay)
    "CERTIFICATE"               -> Right $ Certificate (datum == "YES")
    "CLAIMED-SCORE"             -> Right $ ClaimedScore (read datum :: Integer)
    "CLUB"                      -> Right $ Club datum
    "CREATED-BY"                -> Right $ CreatedBy datum
    "EMAIL"                     -> Right $ Email datum
    "LOCATION"                  -> Right $ Location datum
    "NAME"                      -> Right $ Name datum
    "ADDRESS"                   -> Right $ Address $ split "\n" datum
    "ADDRESS-CITY"              -> Right $ AddressCity datum
    "ADDRESS-STATE-PROVINCE"    -> Right $ AddressStateProvince datum
    "ADDRESS-POSTALCODE"        -> Right $ AddressPostalCode datum
    "ADDRESS-COUNTRY"           -> Right $ AddressCountry datum
    "OPERATORS"                 -> Right $ Operators $ split " " datum
    "OFFTIME"                   -> let [dateA, timeA, dateB, timeB] = words datum
                                   in Right $ OffTime (unwords [dateA, timeA]) (unwords [dateB, timeB])
    "SOAPBOX"                   -> Right $ Soapbox $ split "\n" datum
    "DEBUG"                     -> Right $ Debug (read datum :: Integer)
    "QSO"                       -> case toQSO contestName datum of
                                       Just q  -> Right $ QSO q
                                       Nothing -> Left $ "Unsupported contest: " ++ contestName
    "X-QSO"                     -> case toQSO contestName datum of
                                       Just q  -> Right $ XQSO q
                                       Nothing -> Left $ "Unsupported contest: " ++ contestName

    -- These are deprecated - convert them to the new format.
    "ARRL-SECTION"              -> Right $ Location datum
    "CATEGORY"                  -> Right $ XLine name datum

    -- And finally, just catch anything else as an XLine.
    _                           -> Right $ XLine name datum

--
-- THE CABRILLO PARSER
--

-- A file is just a list of tags.  We separate these into 'HeaderTag's (data about the contest)
-- and 'CabrilloQSO's (the QSOs themselves), but there is no requirement that they be divided
-- up.  Tags can basically come in any order they want.
--
-- After we do all the parsing into a list of tags, we have some post-processing work to do.  We
-- need to batch all the QSOs up into their own list.  We need to put all the ADDRESS and SOAPBOX
-- lines into one value.  Stuff like that.
file contestName = do body <- tagList contestName
                      return $ CabrilloFile (doMerge body)
 where
    doMerge = mergeAddressTags .  mergeOperatorsTags .  mergeSoapboxTags

    mergeAddressTags :: [Tag] -> [Tag]
    mergeAddressTags tags = let (addressTags, theRest) = partition isAddressTag tags
                                addressLines = mapMaybe unAddress $ take 6 addressTags
                            in if not (null addressLines) then theRest ++ [Address addressLines]
                               else theRest
     where
        isAddressTag (Address _) = True
        isAddressTag _           = False

        unAddress (Address s) = Just (concat s)
        unAddress _           = Nothing

    mergeOperatorsTags :: [Tag] -> [Tag]
    mergeOperatorsTags tags = let (operatorsTags, theRest) = partition isOperatorsTag tags
                                  operatorsLines = mapMaybe unOperators operatorsTags
                              in if not (null operatorsLines) then theRest ++ [Operators operatorsLines]
                                 else theRest
     where
        isOperatorsTag (Operators _) = True
        isOperatorsTag _             = False

        unOperators (Operators s) = Just (concat s)
        unOperators _             = Nothing

    mergeSoapboxTags :: [Tag] -> [Tag]
    mergeSoapboxTags tags = let (soapboxTags, theRest) = partition isSoapboxTag tags
                                soapboxLines = mapMaybe unSoapbox soapboxTags
                            in if not (null soapboxLines) then theRest ++ [Soapbox soapboxLines]
                               else theRest
     where
        isSoapboxTag (Soapbox _) = True
        isSoapboxTag _           = False

        unSoapbox (Soapbox s) = Just (concat s)
        unSoapbox _           = Nothing

-- We treat every line in the file as a simple Tag.  This includes QSOs - in their case, we just
-- store the entire text in the data type.  We will parse them separately afterwards and convert
-- them into their own type.  It's just easier this way, given how everything can be all jumbled
-- together in no real order.
tagList contestName = tag contestName `sepEndBy` newline
tag contestName     = do (tagName, tagValue) <- tagSpec
                         case stringToTag contestName tagName tagValue of
                             Left err  -> fail err
                             Right tag -> return tag
tagSpec             = do name <- many1 $ noneOf ":"
                         value <- skipMany (oneOf ": ") >> manyTill anyChar (lookAhead $ try newline)
                         return (uppercase name, value)

-- | Given a 'String' containing Cabrillo data, convert it into an 'CabrilloFile'.
parseString :: String -> String -> Either ParseError CabrilloFile
parseString contestName s = let
    txt = T.strip $ T.pack s
    txt' = txt `T.append` T.pack "\n"
 in
    parse (file contestName) "<stdin>" (T.unpack txt')
