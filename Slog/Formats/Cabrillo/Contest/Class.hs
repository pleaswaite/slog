{-# LANGUAGE ExistentialQuantification #-}

module Slog.Formats.Cabrillo.Contest.Class(CabrilloQSO(..),
                                           CabrilloQSOBox(..))
 where

import Slog.Formats.ADIF.Types(Field)

-- | The CabrilloQSO class defines two methods that all the various Cabrillo contest
-- QSOs should support - 'toADIF', for converting into a list of ADIF 'Field's, and
-- 'toString', for converting into a String.
class CabrilloQSO c where
    toADIF   :: c -> Maybe [Field]
    toString :: c -> Maybe String

-- | Wrap up anything that is a member of the 'CabrilloQSO' class into a box so we can
-- pass them around as heterogeneous lists, or return multiple different types of them
-- from a function.
data CabrilloQSOBox = forall a. CabrilloQSO a => CabrilloQSOBox a

-- And then, a 'CabrilloQSOBox' is also a member of the 'CabrilloQSO' class with all the
-- functions just being pass throughs.
instance CabrilloQSO CabrilloQSOBox where
    toADIF (CabrilloQSOBox q)   = toADIF q
    toString (CabrilloQSOBox q) = toString q
