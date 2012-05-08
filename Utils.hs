module Utils where

import Data.Char(toUpper)

import qualified Formats.ADIF.Types as ADIF

-- Given an association list, turn it inside out
invert :: [(a, b)] -> [(b, a)]
invert = map (\(a, b) -> (b, a))

uppercase :: String -> String
uppercase = map toUpper

-- Add dashes to an ADIF-formatted date, since that's what LOTW expects.
dashifyDate :: String -> String
dashifyDate [y1, y2, y3, y4, m1, m2, d1, d2] = [y1, y2, y3, y4, '-', m1, m2, '-', d1, d2]
dashifyDate s = s

-- Remove dashes from a LOTW-formatted date, since that's what ADIF expects.
undashifyDate :: String -> String
undashifyDate [y1, y2, y3, y4, '-', m1, m2, '-', d1, d2] = [y1, y2, y3, y4, m1, m2, d1, d2]
undashifyDate s = s

colonifyTime :: String -> String
colonifyTime [h1, m1, m2] = ['0', h1, ':', m1, m2]
colonifyTime [h1, h2, m1, m2] = [h1, h2, ':', m1, m2]
colonifyTime [h1, m1, m2, s1, s2] = ['0', h1, ':', m1, m2, ':', s1, s2]
colonifyTime [h1, h2, m1, m2, s1, s2] = [h1, h2, ':', m1, m2, ':', s1, s2]
colonifyTime s = s

uncolonifyTime :: String -> String
uncolonifyTime [h1, ':', m1, m2] = ['0', h1, m1, m2]
uncolonifyTime [h1, h2, ':', m1, m2] = [h1, h2, m1, m2]
uncolonifyTime [h1, ':', m1, m2, ':', s1, s2] = ['0', h1, m1, m2, s1, s2]
uncolonifyTime [h1, h2, ':', m1, m2, ':', s1, s2] = [h1, h2, m1, m2, s1, s2]
uncolonifyTime s = s

withoutSeconds :: String -> String
withoutSeconds [h1, m1, m2, _, _] = ['0', h1, m1, m2]
withoutSeconds [h1, h2, m1, m2, _, _] = [h1, h2, m1, m2]
withoutSeconds [h1, ':', m1, m2, _, _, _] = ['0', h1, ':', m1, m2]
withoutSeconds [h1, h2, ':', m1, m2, _, _, _] = [h1, h2, ':', m1, m2]
withoutSeconds s = s

withSeconds :: String -> String
withSeconds [h1, h2, m1, m2] = [h1, h2, m1, m2, '0', '0']
withSeconds [h1, h2, ':', m1, m2] = [h1, h1, ':', m1, m2, ':', '0', '0']
withSeconds s = s

freqToBand :: Double -> ADIF.Band
freqToBand f | 0.1357 <= f && f <= 0.1378 = ADIF.Band2190M
             | 1.8 <= f && f <= 2.0 = ADIF.Band160M
             | 3.5 <= f && f <= 4.0 = ADIF.Band80M
             | 5.3305 <= f && f <= 5.4035 = ADIF.Band60M
             | 7.0 <= f && f <= 7.3 = ADIF.Band40M
             | 10.1 <= f && f <= 10.15 = ADIF.Band30M
             | 14.0 <= f && f <= 14.35 = ADIF.Band20M
             | 18.068 <= f && f <= 18.168 = ADIF.Band17M
             | 21.0 <= f && f <= 21.45 = ADIF.Band15M
             | 24.89 <= f && f <= 24.99 = ADIF.Band12M
             | 28.0 <= f && f <= 29.7 = ADIF.Band10M
             | 50.0 <= f && f <= 54.0 = ADIF.Band6M
             | 144.0 <= f && f <= 148.0 = ADIF.Band2M
             | 222.0 <= f && f <= 225.0 = ADIF.Band1Point25M
             | 420.0 <= f && f <= 450.0 = ADIF.Band70CM
             | 902.0 <= f && f <= 928.0 = ADIF.Band33CM
             | 1240.0 <= f && f <= 1300.0 = ADIF.Band23CM
             | 2300.0 <= f && f <= 2450.0 = ADIF.Band13CM
             | 3300.0 <= f && f <= 3500.0 = ADIF.Band9CM
             | 5650.0 <= f && f <= 5925.0 = ADIF.Band6CM
             | 10000.0 <= f && f <= 10500.0 = ADIF.Band3CM
             | 24000.0 <= f && f <= 24250.0 = ADIF.Band1Point25CM
             | 47000.0 <= f && f <= 47200.0 = ADIF.Band6MM
             | 76000.0 <= f && f <= 81900.0 = ADIF.Band4MM
             | 119980.0 <= f && f <= 120020.0 = ADIF.Band2Point5MM
             | 142000.0 <= f && f <= 149000.0 = ADIF.Band2MM
             | 241000.0 <= f && f <= 250000.0 = ADIF.Band1MM
