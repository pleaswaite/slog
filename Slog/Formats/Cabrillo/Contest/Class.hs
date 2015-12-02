module Slog.Formats.Cabrillo.Contest.Class(CabrilloQSO(..))
 where

class CabrilloQSO c where
    toString :: c -> Maybe String
