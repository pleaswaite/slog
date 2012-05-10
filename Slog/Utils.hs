-- | A loose collection of utility functions used throughout Slog.
module Slog.Utils where

import Data.Char(toUpper)

import qualified Slog.Formats.ADIF.Types as ADIF

-- | Given an association list, turn it inside out.  Note that if there are duplicates
-- in b, the last one will be the only one in the resulting association list.
invert :: [(a, b)] -> [(b, a)]
invert = map (\(a, b) -> (b, a))

-- | Convert a string into a double.
stringToDouble :: String -> Maybe Double
stringToDouble s =
    case reads s :: [(Double, String)] of
        [tup]   -> Just $ fst tup
        _       -> Nothing

-- | Convert a string into an integer.
stringToInteger :: String -> Maybe Integer
stringToInteger s =
    case reads s :: [(Integer, String)] of
        [tup]   -> Just $ fst tup
        _       -> Nothing

-- | Return a new string where every letter has been upper-cased.
uppercase :: String -> String
uppercase = map toUpper

-- | Add dashes to a date in YYYYMMDD format, converting it into YYYY-MM-DD format.
-- The former is what ADIF expects, while the latter is what LOTW expects.  Any date
-- not in this initial format is returned as-is.
dashifyDate :: String -> String
dashifyDate [y1, y2, y3, y4, m1, m2, d1, d2] = [y1, y2, y3, y4, '-', m1, m2, '-', d1, d2]
dashifyDate s = s

-- | Remove dashes from a date in YYYY-MM-DD format, converting it into YYYYMMDD format.
-- The former is what LOTW expects, while the latter is what ADIF expects.  Any date
-- not in this initial format is returned as-is.
undashifyDate :: String -> String
undashifyDate [y1, y2, y3, y4, '-', m1, m2, '-', d1, d2] = [y1, y2, y3, y4, m1, m2, d1, d2]
undashifyDate s = s

-- | Add colons between hours, minutes, and (potentially) seconds in several different
-- time formats.  Hours and minutes can initially lack a leading zero.  Any time not in
-- a recognized format is returned as-is.
colonifyTime :: String -> String
colonifyTime [h1, m1, m2] = ['0', h1, ':', m1, m2]
colonifyTime [h1, h2, m1, m2] = [h1, h2, ':', m1, m2]
colonifyTime [h1, m1, m2, s1, s2] = ['0', h1, ':', m1, m2, ':', s1, s2]
colonifyTime [h1, h2, m1, m2, s1, s2] = [h1, h2, ':', m1, m2, ':', s1, s2]
colonifyTime s = s

-- | Remove colons from between hours, minutes, and (potentially) seconds in several
-- different time formats.  Hours and minutes can initially lack a leading zero.  Any
-- time not in a recognized format is returned as-is.
uncolonifyTime :: String -> String
uncolonifyTime [h1, ':', m1, m2] = ['0', h1, m1, m2]
uncolonifyTime [h1, h2, ':', m1, m2] = [h1, h2, m1, m2]
uncolonifyTime [h1, ':', m1, m2, ':', s1, s2] = ['0', h1, m1, m2, s1, s2]
uncolonifyTime [h1, h2, ':', m1, m2, ':', s1, s2] = [h1, h2, m1, m2, s1, s2]
uncolonifyTime s = s

-- | Drop the trailing seconds from several different time formats.  Hours and minutes
-- can initially lack a leading zero.  Any time not in a recognized format is returned
-- as-is.
withoutSeconds :: String -> String
withoutSeconds [h1, m1, m2, _, _] = ['0', h1, m1, m2]
withoutSeconds [h1, h2, m1, m2, _, _] = [h1, h2, m1, m2]
withoutSeconds [h1, ':', m1, m2, _, _, _] = ['0', h1, ':', m1, m2]
withoutSeconds [h1, h2, ':', m1, m2, _, _, _] = [h1, h2, ':', m1, m2]
withoutSeconds s = s

-- | Add \"00\" as the seconds to the end of several different time formats.  Any time not
-- in a recognized format is returned as-is.
withSeconds :: String -> String
withSeconds [h1, h2, m1, m2] = [h1, h2, m1, m2, '0', '0']
withSeconds [h1, h2, ':', m1, m2] = [h1, h1, ':', m1, m2, ':', '0', '0']
withSeconds s = s

-- | Determine the 'ADIF.Band' that a given frequency falls in.
freqToBand :: Double -> Maybe ADIF.Band
freqToBand f | 0.1357 <= f && f <= 0.1378       = Just ADIF.Band2190M
             | 1.8 <= f && f <= 2.0             = Just ADIF.Band160M
             | 3.5 <= f && f <= 4.0             = Just ADIF.Band80M
             | 5.3305 <= f && f <= 5.4035       = Just ADIF.Band60M
             | 7.0 <= f && f <= 7.3             = Just ADIF.Band40M
             | 10.1 <= f && f <= 10.15          = Just ADIF.Band30M
             | 14.0 <= f && f <= 14.35          = Just ADIF.Band20M
             | 18.068 <= f && f <= 18.168       = Just ADIF.Band17M
             | 21.0 <= f && f <= 21.45          = Just ADIF.Band15M
             | 24.89 <= f && f <= 24.99         = Just ADIF.Band12M
             | 28.0 <= f && f <= 29.7           = Just ADIF.Band10M
             | 50.0 <= f && f <= 54.0           = Just ADIF.Band6M
             | 144.0 <= f && f <= 148.0         = Just ADIF.Band2M
             | 222.0 <= f && f <= 225.0         = Just ADIF.Band1Point25M
             | 420.0 <= f && f <= 450.0         = Just ADIF.Band70CM
             | 902.0 <= f && f <= 928.0         = Just ADIF.Band33CM
             | 1240.0 <= f && f <= 1300.0       = Just ADIF.Band23CM
             | 2300.0 <= f && f <= 2450.0       = Just ADIF.Band13CM
             | 3300.0 <= f && f <= 3500.0       = Just ADIF.Band9CM
             | 5650.0 <= f && f <= 5925.0       = Just ADIF.Band6CM
             | 10000.0 <= f && f <= 10500.0     = Just ADIF.Band3CM
             | 24000.0 <= f && f <= 24250.0     = Just ADIF.Band1Point25CM
             | 47000.0 <= f && f <= 47200.0     = Just ADIF.Band6MM
             | 76000.0 <= f && f <= 81900.0     = Just ADIF.Band4MM
             | 119980.0 <= f && f <= 120020.0   = Just ADIF.Band2Point5MM
             | 142000.0 <= f && f <= 149000.0   = Just ADIF.Band2MM
             | 241000.0 <= f && f <= 250000.0   = Just ADIF.Band1MM
             | otherwise                        = Nothing
