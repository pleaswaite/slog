{-# LANGUAGE RecordWildCards #-}

module Slog.Formats.Cabrillo.Contest.ARRL(TenMeterQSO(..),
                                          mkTenMeterQSO)
 where

import Text.Printf(printf)

import qualified Slog.Formats.ADIF.Types as A
import           Slog.Formats.Cabrillo.Contest.Class(CabrilloQSO(..))
import qualified Slog.Formats.Cabrillo.Types as C
import           Slog.Formats.Utils(cabrilloBandToADIF, cabrilloModeToADIFMode)
import           Slog.Utils(undashifyDate)

data TenMeterQSO = TenMeterQSO { tenBand :: C.Band, tenMode :: C.Mode, tenDate :: C.Date, tenTime :: C.Time,
                                 tenCall :: String, tenRST :: String, tenXC :: String,
                                 tenTheirCall :: String, tenTheirRST :: String, tenTheirXC :: String }

instance CabrilloQSO TenMeterQSO where
    toADIF TenMeterQSO{..} = cabrilloBandToADIF tenBand >>= \a ->
        Just [a,
              A.Mode (cabrilloModeToADIFMode tenMode),
              A.QSO_Date (undashifyDate tenDate),
              A.TimeOn tenTime,
              A.StationCall tenCall,
              A.RST_Sent tenRST,
              A.Call tenTheirCall,
              A.RST_Received tenTheirRST]

    toString TenMeterQSO{..} =
        Just $ printf "%.5s %s %s %s %-13s %-4s %-6s %-13s %-4s %-6s"
                      (show tenBand) (show tenMode) tenDate tenTime
                      tenCall tenRST tenXC
                      tenTheirCall tenTheirRST tenTheirXC

mkTenMeterQSO :: String -> Either String TenMeterQSO
mkTenMeterQSO s = let splitup = words s
                  in if length splitup < 10 then Left $ "Not enough fields in QSO line: " ++ s
                     else Right TenMeterQSO { tenBand=read (head splitup) :: C.Band,
                                              tenMode=read (splitup !! 1) :: C.Mode,
                                              tenDate=splitup !! 2,
                                              tenTime=splitup !! 3,
                                              tenCall=splitup !! 4,
                                              tenRST=splitup !! 5,
                                              tenXC=splitup !! 6,
                                              tenTheirCall=splitup !! 7,
                                              tenTheirRST=splitup !! 8,
                                              tenTheirXC=splitup !! 9 }
