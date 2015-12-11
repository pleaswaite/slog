{-# OPTIONS_GHC -Wall #-}

module Filter(byBand,
              byCall,
              byConfirmed,
              byDigital,
              byDXCC,
              byITU,
              byImage,
              byMode,
              byNone,
              byPhone,
              bySatellite,
              byWAZ)
 where

import Slog.DB(QsosId)
import qualified Slog.Formats.ADIF.Types as ADIF
import Slog.Formats.ADIF.Utils(freqToBand)
import Slog.Mode(Mode, digitalMode, imageMode, phoneMode)
import Slog.QSO

byBand :: ADIF.Band -> (QsosId, QSO, Confirmation) -> Bool
byBand band (_, qso, _) = maybe False (band ==) (freqToBand $ qFreq qso)

byCall :: String -> (QsosId, QSO, Confirmation) -> Bool
byCall call (_, qso, _) = call == qCall qso

byConfirmed :: Bool -> (QsosId, QSO, Confirmation) -> Bool
byConfirmed conf (_, _, conf') =
    isConfirmed conf' && conf

byDigital :: (QsosId, QSO, Confirmation) -> Bool
byDigital (_, qso, _) = digitalMode (qMode qso)

byDXCC :: Integer -> (QsosId, QSO, Confirmation) -> Bool
byDXCC dxcc (_, qso, _) = maybe False (dxcc ==) (qDXCC qso)

byImage :: (QsosId, QSO, Confirmation) -> Bool
byImage (_, qso, _) = imageMode (qMode qso)

byITU :: Integer -> (QsosId, QSO, Confirmation) -> Bool
byITU itu (_, qso, _) = maybe False (itu ==) (qITU qso)

byMode :: Mode -> (QsosId, QSO, Confirmation) -> Bool
byMode mode (_, qso, _) = mode == qMode qso

byNone :: (QsosId, QSO, Confirmation) -> Bool
byNone _ = True

byPhone :: (QsosId, QSO, Confirmation) -> Bool
byPhone (_, qso, _) = phoneMode (qMode qso)

bySatellite :: (QsosId, QSO, Confirmation) -> Bool
bySatellite (_, qso, _) = maybe False (ADIF.SAT ==) (qPropMode qso)

byWAZ :: Integer -> (QsosId, QSO, Confirmation) -> Bool
byWAZ waz (_, qso, _) = maybe False (waz ==) (qWAZ qso)
