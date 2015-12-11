{-# OPTIONS_GHC -Wall #-}

-- | A module for working with amateur radio modes.
module Slog.Mode(Mode(..),
                 cabrilloStringToMode,
                 digitalMode,
                 freqToMode,
                 imageMode,
                 modeToCabrilloString,
                 phoneMode)
 where

import Slog.Utils(uppercase)

-- | All the various modes.
data Mode = AM | AMTORFEC | ASCI | ATV | CHIP64 | CHIP128 | CLO | CONTESTI |
            CW | DSTAR | DOMINO | DOMINOF | FAX | FM | FMHELL | FSK31 | FSK441 |
            GTOR | HELL | HELL80 | HFSK | JT44 | JT4A | JT4B | JT4C | JT4D |
            JT4E | JT4F | JT4G | JT65 | JT65A | JT65B | JT65C | JT6M | JT9 | MFSK8 |
            MFSK16 | MT63 | OLIVIA | PAC | PAC2 | PAC3 | PAX | PAX2 | PCW |
            PSK10 | PSK31 | PSK63 | PSK63F | PSK125 | PSKAM10 | PSKAM31 |
            PSKAM50 | PSKFEC31 | PSKHELL | Q15 | QPSK31 | QPSK63 | QPSK125 |
            ROS | RTTY | RTTYM | SSB | SSTV | THRB | THOR | THRBX | TOR | VOI |
            WINMOR | WSPR
 deriving (Eq, Read, Show)

-- | Cabrillo supports a limited set of modes.  Given a string representing a
-- Cabrillo mode, attempt to convert it into a 'Mode'.
cabrilloStringToMode :: String -> Maybe Mode
cabrilloStringToMode s = case uppercase s of
    "CW"    -> Just CW
    "FM"    -> Just FM
    "RY"    -> Just RTTY
    "PH"    -> Just SSB
    _       -> Nothing

-- | And then perform the opposite operation from 'cabrilloStringToMode'.
modeToCabrilloString :: Mode -> Maybe String
modeToCabrilloString m = case m of
    CW      -> Just "CW"
    FM      -> Just "FM"
    RTTY    -> Just "RY"
    SSB     -> Just "PH"
    _       -> Nothing

-- | Is the given mode a digital mode?
digitalMode :: Mode -> Bool
digitalMode mode = mode `notElem` [AM, ATV, CW, FAX, FM, SSB, SSTV]

-- | Is the given mode an image mode?
imageMode :: Mode -> Bool
imageMode mode = mode `elem` [ATV, FAX, SSTV]

-- | Is the given mode a phone (voice) mode?
phoneMode :: Mode -> Bool
phoneMode mode = mode `elem` [AM, FM, SSB]

-- | Determine the likely 'Mode' in use given a frequency.  This
-- encodes an awful lot of information about band plans, and we're certainly
-- not going to cover all the digital mode possibilities, but it's good
-- enough.  We will cover the main ones plus the SSB, CW, and FM segments
-- of these bands.
freqToMode :: Double -> Maybe Mode
freqToMode f
    -- 160M
    | f == 1.807 = Just PSK31
    -- 80M
    | f == 3.576 = Just JT65
    | f == 3.580 = Just PSK31
    -- 40M
    | f == 7.035 = Just PSK31
    | f == 7.070 = Just PSK31
    | f == 7.076 = Just JT65
    | f == 7.039 = Just JT65
    -- 30M
    | f == 10.138 = Just JT65
    | 10.139 <= f && f <= 10.142 = Just PSK31
    -- 20M
    | f == 14.070 = Just PSK31
    | f == 14.076 = Just JT65
    -- 17M
    | f == 18.100 = Just PSK31
    | f == 18.102 = Just JT65
    -- 15M
    | f == 21.070 = Just JT65
    | f == 21.076 = Just JT65
    -- 12M
    | f == 24.917 = Just JT65
    | f == 24.920 = Just PSK31
    -- 10M
    | f == 28.076 = Just JT65
    | f == 28.120 = Just PSK31
    | f == 29.600 = Just FM
    -- 6M
    | f == 50.276 = Just JT65
    | f == 50.290 = Just PSK31
    -- and the rest
    | otherwise = Nothing
