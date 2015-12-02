{-# LANGUAGE RecordWildCards #-}

module Slog.Formats.Cabrillo.Contest.BAC(BACQSO(..),
                                         mkBACQSO)
 where

import Text.Printf(printf)

import Slog.Formats.Cabrillo.Contest.Class(CabrilloQSO(..))
import Slog.Formats.Cabrillo.Types(Band, Date, Mode, Time)

data BACQSO = BACQSO { bacBand :: Band, bacMode :: Mode, bacDate :: Date, bacTime :: Time,
                       bacCall :: String, bacRST :: String, bacTheirCall :: String,
                       bacTheirRST :: String, bacXC :: String, bacTheirXC :: String }

instance CabrilloQSO BACQSO where
    toString BACQSO{..} =
        Just $ printf "%.5s %s %s %s %-13s %-4s %-13s %-4s %.10s %.10s"
                      (show bacBand) (show bacMode) bacDate bacTime
                      bacCall bacRST bacTheirCall bacTheirRST bacXC bacTheirXC

mkBACQSO :: String -> Maybe BACQSO
mkBACQSO s = let splitup = words s
             in if length splitup < 10 then Nothing
                else Just BACQSO { bacBand=read (head splitup) :: Band,
                                   bacMode=read (splitup !! 1) :: Mode,
                                   bacDate=splitup !! 2,
                                   bacTime=splitup !! 3,
                                   bacCall=splitup !! 4,
                                   bacRST=splitup !! 5,
                                   bacTheirCall=splitup !! 6,
                                   bacTheirRST=splitup !! 7,
                                   bacXC="",
                                   bacTheirXC="" }
