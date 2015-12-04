{-# LANGUAGE RecordWildCards #-}

module Slog.Formats.Cabrillo.Contest.BAC(BACQSO(..),
                                         mkBACQSO)
 where

import Text.Printf(printf)

import qualified Slog.Formats.ADIF.Types as A
import           Slog.Formats.Cabrillo.Contest.Class(CabrilloQSO(..))
import qualified Slog.Formats.Cabrillo.Types as C
import           Slog.Formats.Utils(cabrilloBandToADIF, cabrilloModeToADIFMode)
import           Slog.Utils(undashifyDate)

data BACQSO = BACQSO { bacBand :: C.Band, bacMode :: C.Mode, bacDate :: C.Date, bacTime :: C.Time,
                       bacCall :: String, bacRST :: String, bacTheirCall :: String,
                       bacTheirRST :: String, bacXC :: String, bacTheirXC :: String }

instance CabrilloQSO BACQSO where
    toADIF BACQSO{..} = cabrilloBandToADIF bacBand >>= \a ->
        Just [a,
              A.Mode (cabrilloModeToADIFMode bacMode),
              A.QSO_Date (undashifyDate bacDate),
              A.TimeOn bacTime,
              A.StationCall bacCall,
              A.RST_Sent bacRST,
              A.Call bacTheirCall,
              A.RST_Received bacTheirRST]

    toString BACQSO{..} =
        Just $ printf "%.5s %s %s %s %-13s %-4s %-13s %-4s %.10s %.10s"
                      (show bacBand) (show bacMode) bacDate bacTime
                      bacCall bacRST bacTheirCall bacTheirRST bacXC bacTheirXC

mkBACQSO :: String -> Maybe BACQSO
mkBACQSO s = let splitup = words s
             in if length splitup < 10 then Nothing
                else Just BACQSO { bacBand=read (head splitup) :: C.Band,
                                   bacMode=read (splitup !! 1) :: C.Mode,
                                   bacDate=splitup !! 2,
                                   bacTime=splitup !! 3,
                                   bacCall=splitup !! 4,
                                   bacRST=splitup !! 5,
                                   bacTheirCall=splitup !! 6,
                                   bacTheirRST=splitup !! 7,
                                   bacXC="",
                                   bacTheirXC="" }
