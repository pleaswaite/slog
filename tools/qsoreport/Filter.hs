module Filter(byBand,
              byCall,
              byConfirmed,
              byDXCC,
              byITU,
              byMode,
              byWAZ)
 where

import qualified Formats.ADIF.Types as ADIF
import QSO
import Types(ConfirmInfo)
import Utils(freqToBand, uppercase)

byBand :: ADIF.Band -> ConfirmInfo -> Bool
byBand band (qso, _) = band == (freqToBand $ qFreq qso)

byCall :: String -> ConfirmInfo -> Bool
byCall call (qso, _) = uppercase call == uppercase (qCall qso)

byConfirmed :: Bool -> ConfirmInfo -> Bool
byConfirmed conf (_, conf') = conf == conf'

byDXCC :: Integer -> ConfirmInfo -> Bool
byDXCC dxcc (qso, _) = maybe False (dxcc ==) (qDXCC qso)

byITU :: Integer -> ConfirmInfo -> Bool
byITU itu (qso, _) = maybe False (itu ==) (qITU qso)

byMode :: ADIF.Mode -> ConfirmInfo -> Bool
byMode mode (qso, _) = mode == qMode qso

byWAZ :: Integer -> ConfirmInfo -> Bool
byWAZ waz (qso, _) = maybe False (waz ==) (qWAZ qso)
