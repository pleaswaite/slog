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

import qualified Slog.Formats.ADIF.Types as ADIF
import Slog.Formats.ADIF.Utils(freqToBand)
import Slog.QSO
import Slog.Utils(uppercase)

import Types(ConfirmInfo)

byBand :: ADIF.Band -> ConfirmInfo -> Bool
byBand band (qso, _) = maybe False (band ==) (freqToBand $ qFreq qso)

byCall :: String -> ConfirmInfo -> Bool
byCall call (qso, _) = uppercase call == uppercase (qCall qso)

byConfirmed :: Bool -> ConfirmInfo -> Bool
byConfirmed conf (_, conf') = conf == conf'

byDigital :: ConfirmInfo -> Bool
byDigital (qso, _) = ADIF.digitalMode (qMode qso)

byDXCC :: Integer -> ConfirmInfo -> Bool
byDXCC dxcc (qso, _) = maybe False (dxcc ==) (qDXCC qso)

byImage :: ConfirmInfo -> Bool
byImage (qso, _) = ADIF.imageMode (qMode qso)

byITU :: Integer -> ConfirmInfo -> Bool
byITU itu (qso, _) = maybe False (itu ==) (qITU qso)

byMode :: ADIF.Mode -> ConfirmInfo -> Bool
byMode mode (qso, _) = mode == qMode qso

byNone :: ConfirmInfo -> Bool
byNone _ = True

byPhone :: ConfirmInfo -> Bool
byPhone (qso, _) = ADIF.phoneMode (qMode qso)

bySatellite :: ConfirmInfo -> Bool
bySatellite (qso, _) = maybe False (ADIF.SAT ==) (qPropMode qso)

byWAZ :: Integer -> ConfirmInfo -> Bool
byWAZ waz (qso, _) = maybe False (waz ==) (qWAZ qso)
