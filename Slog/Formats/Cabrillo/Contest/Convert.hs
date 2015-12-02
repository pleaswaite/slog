module Slog.Formats.Cabrillo.Contest.Convert(toQSO)
 where

import Slog.Formats.Cabrillo.Contest.BAC(BACQSO, mkBACQSO)

toQSO :: String -> String -> Maybe BACQSO
toQSO contestName s = case contestName of
    "BAC"   -> mkBACQSO s
    _       -> Nothing
