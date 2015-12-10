module Slog.Formats.Cabrillo.Contest.Convert(toQSO)
 where

import Slog.Formats.Cabrillo.Contest.BAC(BACQSO, mkBACQSO)
import Slog.Formats.Cabrillo.Contest.Class(CabrilloQSOBox(..))
import Slog.Formats.Cabrillo.Contest.CQWW(CQ_WW_SSB_QSO, mkCQ_WW_SSB_QSO)

-- | Given a contest name and a string containing what is hopefully a single QSO: line
-- from a Cabrillo file (minus the QSO: part at the beginning), convert it into a record.
-- This record will be wrapped up in a 'CabrilloQSOBox', but inside that is just anything
-- that is a member of the 'CabrilloQSO' type class.
toQSO :: String -> String -> Maybe CabrilloQSOBox
toQSO contestName s = case contestName of
    "BAC"       -> mkBACQSO s        >>= Just . CabrilloQSOBox
    "CQ-WW-SSB" -> mkCQ_WW_SSB_QSO s >>= Just . CabrilloQSOBox
    _           -> Nothing
