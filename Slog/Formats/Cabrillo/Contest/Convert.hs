module Slog.Formats.Cabrillo.Contest.Convert(toQSO,
                                             toString)
 where

import Slog.Formats.Cabrillo.Contest.BAC(bacQSOToString, mkBACQSO)
import Slog.Formats.Cabrillo.Types

toQSO :: String -> String -> Maybe CabrilloQSO
toQSO contestName s = case contestName of
    "BAC"   -> mkBACQSO s
    _       -> Nothing

toString :: String -> CabrilloQSO -> Maybe String
toString contestName q = case contestName of
    "BAC"   -> bacQSOToString q
    _       -> Nothing
