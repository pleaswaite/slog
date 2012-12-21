-- | A loose collection of utility functions used throughout Slog.
module Slog.Utils where

import Data.Char(toUpper)

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
