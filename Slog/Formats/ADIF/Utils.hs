-- | A loose collection of utility functions for working with ADIF.
module Slog.Formats.ADIF.Utils where

import qualified Slog.Formats.ADIF.Types as ADIF

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
