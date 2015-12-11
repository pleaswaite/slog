module Slog.Formats.Cabrillo.Writer(renderFile,
                                    renderTag)
 where

import Slog.Formats.Cabrillo.Types(CabrilloFile(..), Tag(..))
import Slog.Formats.Cabrillo.Contest.Class(CabrilloQSO(toString))
import Slog.Mode(modeToCabrilloString)

renderFile :: CabrilloFile -> String
renderFile (CabrilloFile tags) = concatMap renderTag tags

renderTag :: Tag -> String
renderTag t = case t of
    StartOfLog v            -> "START-OF-LOG: " ++ v
    EndOfLog                -> "END-OF-LOG:"
    Callsign c              -> "CALLSIGN: " ++ c
    Contest c               -> "CONTEST: " ++ c
    CategoryAssisted c      -> "CATEGORY-ASSISTED: " ++ show c
    CategoryBand c          -> "CATEGORY-BAND: " ++ show c
    CategoryMode c          -> case modeToCabrilloString c of
                                   Just s  -> "CATEGORY-MODE: " ++ s
                                   Nothing -> ""
    CategoryOperator c      -> "CATEGORY-OPERATOR: " ++ show c
    CategoryPower c         -> "CATEGORY-POWER: " ++ show c
    CategoryStation c       -> "CATEGORY-STATION: " ++ show c
    CategoryTime c          -> "CATEGORY-TIME: " ++ show c
    CategoryTransmitter c   -> "CATEGORY-TRANSMITTER: " ++ show c
    CategoryOverlay c       -> "CATEGORY-OVERLAY: " ++ show c
    Certificate c           -> "CERTIFICATE: " ++ if c then "YES" else "NO"
    ClaimedScore c          -> "CLAIMED-SCORE: " ++ show c
    Club c                  -> "CLUB: " ++ c
    CreatedBy c             -> "CREATED-BY: " ++ c
    Email e                 -> "EMAIL: " ++ e
    Location l              -> "LOCATION: " ++ l
    Name n                  -> "NAME: " ++ n
    Address a               -> let a' = take 6 a
                               in unlines $ map ("ADDRESS: " ++) a'
    AddressCity a           -> "ADDRESS-CITY: " ++ a
    AddressStateProvince a  -> "ADDRESS-STATE-PROVINCE: " ++ a
    AddressPostalCode a     -> "ADDRESS-POSTAL-CODE: " ++ a
    AddressCountry a        -> "ADDRESS-COUNTRY: " ++ a
    Operators o             -> unlines $ map ("OPERATORS: " ++) o
    OffTime a b             -> "OFF-TIME: " ++ a ++ " " ++ b
    Soapbox s               -> unlines $ map ("SOAPBOX: " ++) s
    Debug d                 -> "DEBUG: " ++ show d
    XLine key value         -> "X-" ++ key ++ ": " ++ value
    QSO q                   -> maybe "" ("QSO: " ++) (toString q)
    XQSO x                  -> maybe "" ("XQSO: " ++) (toString x)
