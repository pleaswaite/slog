{-# LANGUAGE RecordWildCards #-}

module Slog.Formats.Cabrillo.Contest.CQWW(CQ_WW_SSB_QSO,
                                          mkCQ_WW_SSB_QSO)
 where

import Text.Printf(printf)

import qualified Slog.Formats.ADIF.Types as A
import           Slog.Formats.Cabrillo.Contest.Class(CabrilloQSO(..))
import qualified Slog.Formats.Cabrillo.Types as C
import           Slog.Formats.Utils(cabrilloBandToADIF)
import           Slog.Mode(Mode, cabrilloStringToMode, modeToCabrilloString)
import           Slog.Utils(undashifyDate)

{-# ANN module "HLint: ignore Use camelCase" #-}

data CQ_WW_SSB_QSO = CQ_WW_SSB_QSO { wwSSBBand :: C.Band, wwSSBMode :: Mode, wwSSBDate :: C.Date, wwSSBTime :: C.Time,
                                     wwSSBCall :: String, wwSSBRST :: String, wwSSBXC :: String,
                                     wwSSBTheirCall :: String, wwSSBTheirRST :: String, wwSSBTheirXC :: String,
                                     wwSSBMulti :: Bool }

instance CabrilloQSO CQ_WW_SSB_QSO where
    toADIF CQ_WW_SSB_QSO {..} = cabrilloBandToADIF wwSSBBand >>= \a ->
        Just [a,
              A.Mode wwSSBMode,
              A.QSO_Date (undashifyDate wwSSBDate),
              A.TimeOn wwSSBTime,
              A.StationCall wwSSBCall,
              A.RST_Sent wwSSBRST,
              A.Call wwSSBTheirCall,
              A.RST_Received wwSSBTheirRST]

    toString CQ_WW_SSB_QSO{..} = modeToCabrilloString wwSSBMode >>= \mode ->
        Just $ printf "%.5s %s %s %s %-13s %-3s %-6s %-13s %-3s %-6s %s"
                      (show wwSSBBand) mode wwSSBDate wwSSBTime
                      wwSSBCall wwSSBRST wwSSBXC
                      wwSSBTheirCall wwSSBTheirRST wwSSBTheirXC
                      (if wwSSBMulti then "1" else "0")

mkCQ_WW_SSB_QSO:: String -> Either String CQ_WW_SSB_QSO
mkCQ_WW_SSB_QSO s = let splitup = words s
                    in if length splitup < 11 then Left $ "Not enough fields in QSO line: " ++ s 
                       else case cabrilloStringToMode (splitup !! 1) of
                                Just mode -> Right CQ_WW_SSB_QSO { wwSSBBand=read (head splitup) :: C.Band,
                                                                   wwSSBMode=mode,
                                                                   wwSSBDate=splitup !! 2,
                                                                   wwSSBTime=splitup !! 3,
                                                                   wwSSBCall=splitup !! 4,
                                                                   wwSSBRST=splitup !! 5,
                                                                   wwSSBXC=splitup !! 6,
                                                                   wwSSBTheirCall=splitup !! 7,
                                                                   wwSSBTheirRST=splitup !! 8,
                                                                   wwSSBTheirXC=splitup !! 9,
                                                                   wwSSBMulti=splitup !! 10 == "1" }
                                Nothing   -> Left $ "Unknown mode given: " ++ (splitup !! 1)
