module Slog.Formats.Cabrillo.Contest.Class(CabrilloQSO(..))
 where

import Slog.Formats.ADIF.Types(Field)

class CabrilloQSO c where
    toADIF   :: c -> [Field]
    toString :: c -> Maybe String
