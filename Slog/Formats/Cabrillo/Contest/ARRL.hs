{-# LANGUAGE RecordWildCards #-}

module Slog.Formats.Cabrillo.Contest.ARRL(TenMeterQSO(..),
                                          mkTenMeterQSO)
 where

import Text.Printf(printf)

import qualified Slog.Formats.ADIF.Types as A
import           Slog.Formats.Cabrillo.Contest.Class(CabrilloQSO(..))
import qualified Slog.Formats.Cabrillo.Types as C
import           Slog.Formats.Utils(cabrilloBandToADIF)
import           Slog.Mode(Mode, cabrilloStringToMode, modeToCabrilloString)
import           Slog.Utils(undashifyDate)

data TenMeterQSO = TenMeterQSO { tenBand :: C.Band, tenMode :: Mode, tenDate :: C.Date, tenTime :: C.Time,
                                 tenCall :: String, tenRST :: String, tenXC :: String,
                                 tenTheirCall :: String, tenTheirRST :: String, tenTheirXC :: String }

instance CabrilloQSO TenMeterQSO where
    toADIF TenMeterQSO{..} = cabrilloBandToADIF tenBand >>= \a ->
        Just [a,
              A.Mode tenMode,
              A.QSO_Date (undashifyDate tenDate),
              A.TimeOn tenTime,
              A.StationCall tenCall,
              A.RST_Sent tenRST,
              A.Call tenTheirCall,
              A.RST_Received tenTheirRST]

    toString TenMeterQSO{..} = modeToCabrilloString tenMode >>= \mode ->
        Just $ printf "%.5s %s %s %s %-13s %-4s %-6s %-13s %-4s %-6s"
                      (show tenBand) mode tenDate tenTime
                      tenCall tenRST tenXC
                      tenTheirCall tenTheirRST tenTheirXC

mkTenMeterQSO :: String -> Either String TenMeterQSO
mkTenMeterQSO s = let splitup = words s
                  in if length splitup < 10 then Left $ "Not enough fields in QSO line: " ++ s
                     else case cabrilloStringToMode (splitup !! 1) of
                              Just mode -> Right TenMeterQSO { tenBand=read (head splitup) :: C.Band,
                                                               tenMode=mode,
                                                               tenDate=splitup !! 2,
                                                               tenTime=splitup !! 3,
                                                               tenCall=splitup !! 4,
                                                               tenRST=splitup !! 5,
                                                               tenXC=splitup !! 6,
                                                               tenTheirCall=splitup !! 7,
                                                               tenTheirRST=splitup !! 8,
                                                               tenTheirXC=splitup !! 9 }
                              Nothing   -> Left $ "Unknown mode given: " ++ (splitup !! 1)
