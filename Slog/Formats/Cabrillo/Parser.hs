{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE DoAndIfThenElse #-}

-- | This module exports a single function that converts a 'String' into a
-- 'CabrilloFile'.  This is the only way to do such a conversion.  The 'Read' methods
-- in the 'Types' module should not be relied upon to do more than convert a single
-- token.
module Slog.Formats.Cabrillo.Parser(parseString) where

import Control.Applicative((<$>))
import Data.List(partition)
import Data.Maybe(catMaybes, mapMaybe)
import Data.String.Utils(split)
import qualified Data.Text as T hiding(toUpper)
import Text.ParserCombinators.Parsec

import Slog.Formats.Cabrillo.Contest.Convert(toQSO)
import Slog.Formats.Cabrillo.Types
import Slog.Utils(uppercase)

stringToTag :: String -> String -> String -> Maybe Tag
stringToTag contestName name datum = case name of
    "START-OF-LOG"              -> Just $ StartOfLog datum
    "END-OF-LOG"                -> Just EndOfLog
    "CALLSIGN"                  -> Just $ Callsign datum
    "CONTEST"                   -> Just $ Contest datum
    "CATEGORY-ASSISTED"         -> Just $ CategoryAssisted (read datum :: Assistance)
    "CATEGORY-BAND"             -> Just $ CategoryBand (read datum :: Band)
    "CATEGORY-MODE"             -> Just $ CategoryMode (read datum :: Mode)
    "CATEGORY-OPERATOR"         -> Just $ CategoryOperator (read datum :: Operator)
    "CATEGORY-POWER"            -> Just $ CategoryPower (read datum :: Power)
    "CATEGORY-STATION"          -> Just $ CategoryStation (read datum :: Station)
    "CATEGORY-TIME"             -> Just $ CategoryTime (read datum :: TimeOperated)
    "CATEGORY-TRANSMITTER"      -> Just $ CategoryTransmitter (read datum :: Transmitter)
    "CATEGORY-OVERLAY"          -> Just $ CategoryOverlay (read datum :: Overlay)
    "CERTIFICATE"               -> Just $ Certificate (datum == "YES")
    "CLAIMED-SCORE"             -> Just $ ClaimedScore (read datum :: Integer)
    "CLUB"                      -> Just $ Club datum
    "CREATED-BY"                -> Just $ CreatedBy datum
    "EMAIL"                     -> Just $ Email datum
    "LOCATION"                  -> Just $ Location datum
    "NAME"                      -> Just $ Name datum
    "ADDRESS"                   -> Just $ Address $ split "\n" datum
    "ADDRESS-CITY"              -> Just $ AddressCity datum
    "ADDRESS-STATE-PROVINCE"    -> Just $ AddressStateProvince datum
    "ADDRESS-POSTALCODE"        -> Just $ AddressPostalCode datum
    "ADDRESS-COUNTRY"           -> Just $ AddressCountry datum
    "OPERATORS"                 -> Just $ Operators $ split " " datum
    "OFFTIME"                   -> let [dateA, timeA, dateB, timeB] = words datum
                                   in Just $ OffTime (unwords [dateA, timeA]) (unwords [dateB, timeB])
    "SOAPBOX"                   -> Just $ Soapbox $ split "\n" datum
    "DEBUG"                     -> Just $ Debug (read datum :: Integer)
    "QSO"                       -> toQSO contestName datum >>= \q -> Just $ QSO q
    "X-QSO"                     -> toQSO contestName datum >>= \q -> Just $ XQSO q

    -- These are deprecated - convert them to the new format.
    "ARRL-SECTION"              -> Just $ Location datum
    "CATEGORY"                  -> Just $ XLine name datum

    -- And finally, just catch anything else as an XLine.
    _                           -> Just $ XLine name datum

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
file contestName = do body <- catMaybes <$> tagList contestName
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
                         return $ stringToTag contestName tagName tagValue
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
