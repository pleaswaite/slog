module Filter(dxccByContinent,
              dxccByNone,
              qsoByBand,
              qsoByNone)
 where

import qualified Slog.Formats.ADIF.Types as ADIF
import Slog.DXCC(DXCC(dxccContinent), entityFromID)
import Slog.Formats.ADIF.Utils(freqToBand)
import Slog.QSO(QSO(qDXCC, qFreq), QSO)

dxccByContinent :: ADIF.Continent -> DXCC -> Bool
dxccByContinent cont dxcc = cont == (dxccContinent dxcc)

dxccByNone :: DXCC -> Bool
dxccByNone _ = True

qsoByBand :: ADIF.Band -> QSO -> Bool
qsoByBand band qso = maybe False (band ==) (freqToBand $ qFreq qso)

qsoByNone :: QSO -> Bool
qsoByNone _ = True
